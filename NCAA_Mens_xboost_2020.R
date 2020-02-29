library(data.table)
library(tidyverse)
library(xgboost)
library(lme4)
library(Metrics)

# Run NCAA_Mens_Data_Prep_2020.R 

features = setdiff(names(data_matrix), c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff", "Location"))
dtrain = xgb.DMatrix(as.matrix(data_matrix[, features]), label = data_matrix$ResultDiff)

cauchyobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c <- 5000 
  x <-  preds-labels
  grad <- x / (x^2/c^2+1)
  hess <- -c^2*(x^2-c^2)/(x^2+c^2)^2
  return(list(grad = grad, hess = hess))
}

xgb_parameters = 
  list(objective = cauchyobj, 
       eval_metric = "mae",
       booster = "gbtree", 
       eta = 0.02,
       subsample = 0.35,
       colsample_bytree = 0.7,
       num_parallel_tree = 4,
       min_child_weight = 40,
       gamma = 10,
       max_depth = 3)

N = nrow(data_matrix)
fold5list = c(
  rep( 1, floor(N/5) ),
  rep( 2, floor(N/5) ),
  rep( 3, floor(N/5) ),
  rep( 4, floor(N/5) ),
  rep( 5, N - 4*floor(N/5) )
)


### Build cross-validation model, repeated 10-times

iteration_count = c()
smooth_model = list()

for (i in 1:10) {
  
  ### Resample fold split
  set.seed(i)
  folds = list()  
  fold_list = sample(fold5list)
  for (k in 1:5) folds[[k]] = which(fold_list == k)
  
  set.seed(120)
  xgb_cv = 
    xgb.cv(
      params = xgb_parameters,
      data = dtrain,
      #nrounds = 3000,
      nrounds = 300,
      verbose = 0,
      nthread = 12,
      folds = folds,
      early_stopping_rounds = 25,
      maximize = FALSE,
      prediction = TRUE
    )
  iteration_count = c(iteration_count, xgb_cv$best_iteration)
  
  ### Fit a smoothed GAM model on predicted result point differential to get probabilities
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = ifelse(data_matrix$ResultDiff > 0, 1, 0))
  
}


### Build submission models

submission_model = list()

for (i in 1:10) {
  set.seed(i)
  print(i)
  submission_model[[i]] = 
    xgb.train(
      params = xgb_parameters,
      data = dtrain,
      nrounds = round(iteration_count[i]*1.05),
      verbose = 0,
      nthread = 12,
      maximize = FALSE,
      prediction = TRUE
    )
}


### Run predictions

#sub$Season = 2018

sub$Season = as.numeric(substring(sub$ID,1,4))
sub$T1 = as.numeric(substring(sub$ID,6,9))
sub$T2 = as.numeric(substring(sub$ID,11,14))

Z = sub %>% 
  left_join(season_summary_X1, by = c("Season", "T1")) %>% 
  left_join(season_summary_X2, by = c("Season", "T2")) %>%
  left_join(select(seeds, Season, T1 = TeamID, X1_Seed = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, X2_Seed = Seed), by = c("Season", "T2")) %>% 
  mutate(SeedDiff = X1_Seed - X2_Seed,) %>%
  left_join(select(quality, Season, T1 = Team_Id, X1_quality_march = quality), by = c("Season", "T1")) %>%
  left_join(select(quality, Season, T2 = Team_Id, X2_quality_march = quality), by = c("Season", "T2")) 
  #left_join(select(quality2, Season, T1 = Team_Id, X1_quality2_march = quality), by = c("Season", "T1")) %>%
  #left_join(select(quality2, Season, T2 = Team_Id, X2_quality2_march = quality), by = c("Season", "T2")) 

Z =
sub %>%
left_join(season_summary_X1, by = c("Season", "T1")) %>% 
  left_join(season_summary_X2, by = c("Season", "T2")) %>%
  left_join(select(seeds, Season, T1 = TeamID, X1_Seed = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, X2_Seed = Seed), by = c("Season", "T2")) %>% 
  mutate(SeedDiff = X1_Seed - X2_Seed) %>%
  left_join(select(quality, Season, T1 = Team_Id, X1_quality_march = quality), by = c("Season", "T1")) %>%
  left_join(select(quality, Season, T2 = Team_Id, X2_quality_march = quality), by = c("Season", "T2")) 

Z = left_join(Z,last_rank_T1,by=c("Season"="T1_Season",'T1'='T1_TeamID'))
Z = left_join(Z,last_rank_T2,by=c("Season"="T2_Season",'T2'='T2_TeamID'))
#Z[is.na(Z)] <- 0

temp <- names(Z)

Z <-
  Z %>%
  left_join(top25DT, by = c("Season" = "Season", "T1" = "T1")) %>%
  left_join(top25DT, by = c("Season" = "Season", "T2" = "T1")) 

names(Z) <- c(temp,"T1_25Wins","T2_25_Wins")
names(data_matrix)
Z[is.na(Z)] <- 0

dtest = xgb.DMatrix(as.matrix(Z[, features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}
Z$Pred = Reduce("+", probs) / 10

### Better be safe than sorry
Z$Pred[Z$Pred <= 0.025] = 0.025
Z$Pred[Z$Pred >= 0.975] = 0.975

### Anomaly event happened only once before - be brave
Z$Pred[Z$Seed1 == 16 & Z$Seed2 == 1] = 0
Z$Pred[Z$Seed1 == 15 & Z$Seed2 == 2] = 0
Z$Pred[Z$Seed1 == 14 & Z$Seed2 == 3] = 0
Z$Pred[Z$Seed1 == 13 & Z$Seed2 == 4] = 0
Z$Pred[Z$Seed1 == 1 & Z$Seed2 == 16] = 1
Z$Pred[Z$Seed1 == 2 & Z$Seed2 == 15] = 1
Z$Pred[Z$Seed1 == 3 & Z$Seed2 == 14] = 1
Z$Pred[Z$Seed1 == 4 & Z$Seed2 == 13] = 1

write.csv(select(Z, ID, Pred), "sub.csv", row.names = FALSE)

#############################

# look at model performance

ZZ <-
Z %>%
  select(Season,T1,T2,Pred)

test <-
results %>%
  filter(Season > 2014) %>%
  select(Season,WTeamID,LTeamID,WScore,LScore) %>%
  left_join(ZZ,by = c("Season" = "Season", "WTeamID" = "T1", "LTeamID" = "T2")) %>%
  left_join(ZZ,by = c("Season" = "Season", "WTeamID" = "T2", "LTeamID" = "T1")) %>%
  as.data.table()

test$Pred <- rowSums(test[,c("Pred.x", "Pred.y")],na.rm = TRUE)

test <-
test %>%
  select(Season,WTeamID,LTeamID,WScore,LScore,Pred)%>%
  mutate(Act = rep(1,times = nrow(test))) %>%
  as.data.table()
  
test[, logLoss(Act,Pred), by = list(Season)]

# Factor Importance
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[1]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[2]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[3]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[4]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[5]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[6]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[7]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[8]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[9]]), top_n = 20)
xgb.plot.importance(importance_matrix = xgb.importance(colnames(dtrain), submission_model[[10]]), top_n = 20)
         

