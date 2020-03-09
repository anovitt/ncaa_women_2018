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
       num_parallel_tree = 2,
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
  print(i)
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

dtest = xgb.DMatrix(as.matrix(Z[, features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}
Z$Pred = Reduce("+", probs) / 10

Z <- 
Z %>%
  mutate(Pred = ifelse(Pred <= 0.025, 0.025, Pred),
         Pred = ifelse(Pred >= 0.975, 0.975, Pred),
         Pred = ifelse(X1_Seed == 16 & X2_Seed == 1, 0.001, Pred),
         Pred = ifelse(X1_Seed == 15 & X2_Seed == 2, 0.001, Pred),
         #Pred = ifelse(X1_Seed == 14 & X2_Seed == 3, 0.001, Pred),
         #Pred = ifelse(X1_Seed == 13 & X2_Seed == 4, 0.001, Pred),
         Pred = ifelse(X1_Seed == 1 & X2_Seed == 16, 0.999, Pred),
         Pred = ifelse(X1_Seed == 2 & X2_Seed == 15, 0.999, Pred))
         #Pred = ifelse(X1_Seed == 3 & X2_Seed == 14, 0.999, Pred),
         #Pred = ifelse(X1_Seed == 4 & X2_Seed == 13, 0.999, Pred),
         #Pred = ifelse(X1_Seed == 1 & X2_Seed == 8 |X1_Seed == 1 & X2_Seed == 9, 0.999, Pred),
         #Pred = ifelse(X1_Seed == 8 & X2_Seed == 1 |X1_Seed == 9 & X2_Seed == 1, 0.001, Pred))


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
  
testRunOne <- test[, logLoss(Act,Pred), by = list(Season)]
testRunOne[, Run := rep(1,times = nrow(testRunOne))]

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


#  after the inital run select the top 40 features re-run increasing the number of trees and number of rounds
         
featTot <- c()
for(i in 1:10){
  feat <-xgb.importance(model = submission_model[[i]])[1:50,]$Feature
  featTot <- c(featTot,feat)
}

featTot <- unique(featTot)

#######################################################################################################################

features = featTot
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
       num_parallel_tree = 10,
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
  print(i)
  folds = list()  
  fold_list = sample(fold5list)
  for (k in 1:5) folds[[k]] = which(fold_list == k)
  
  set.seed(120)
  xgb_cv = 
    xgb.cv(
      params = xgb_parameters,
      data = dtrain,
      #nrounds = 3000,
      nrounds = 1000,
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

dtest = xgb.DMatrix(as.matrix(Z[, features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}
Z$Pred = Reduce("+", probs) / 10

Z <- 
  Z %>%
  mutate(Pred = ifelse(Pred <= 0.025, 0.025, Pred),
         Pred = ifelse(Pred >= 0.975, 0.975, Pred),
         Pred = ifelse(X1_Seed == 16 & X2_Seed == 1, 0.001, Pred),
         Pred = ifelse(X1_Seed == 15 & X2_Seed == 2, 0.001, Pred),
         #Pred = ifelse(X1_Seed == 14 & X2_Seed == 3, 0.001, Pred),
         #Pred = ifelse(X1_Seed == 13 & X2_Seed == 4, 0.001, Pred),
         Pred = ifelse(X1_Seed == 1 & X2_Seed == 16, 0.999, Pred),
         Pred = ifelse(X1_Seed == 2 & X2_Seed == 15, 0.999, Pred))
        #Pred = ifelse(X1_Seed == 3 & X2_Seed == 14, 0.999, Pred),
        #Pred = ifelse(X1_Seed == 4 & X2_Seed == 13, 0.999, Pred),
        #Pred = ifelse(X1_Seed == 1 & X2_Seed == 8 |X1_Seed == 1 & X2_Seed == 9, 0.999, Pred),
        #Pred = ifelse(X1_Seed == 8 & X2_Seed == 1 |X1_Seed == 9 & X2_Seed == 1, 0.001, Pred))

write.csv(select(Z, ID, Pred), "sub2.csv", row.names = FALSE)

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

testRunTwo <- test[, logLoss(Act,Pred), by = list(Season)]
testRunTwo[, Run := rep(2,times = nrow(testRunOne))]

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

testRunOne <- rbind(testRunOne,testRunTwo)
