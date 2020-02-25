library(data.table)
library(tidyverse)
library(xgboost)
library(lme4)
library(Metrics)

# Load the data

regresults <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
results <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
sub <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MSampleSubmissionStage1_2020.csv")
seeds <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MNCAATourneySeeds.csv")

seeds$Seed = as.numeric(substring(seeds$Seed,2,4))

#repeat results twice with switched team positions
regular_season = 
  rbind(
    select(regresults,
           Season,
           DayNum,
           T1=WTeamID,
           T1_Points=WScore,
           T2=LTeamID,
           T2_Points=LScore,
           Location=WLoc,
           NumOT,
           T1_fgm=WFGM,
           T1_fga=WFGA,
           T1_fgm3=WFGM3,
           T1_fga3=WFGA3,
           T1_ftm=WFTM, 
           T1_fta=WFTA,
           T1_or=WOR, 
           T1_dr=WDR, 
           T1_ast=WAst, 
           T1_to=WTO, 
           T1_stl=WStl, 
           T1_blk=WBlk, 
           T1_pf=WPF,
           T2_fgm=LFGM,
           T2_fga=LFGA,
           T2_fgm3=LFGM3,
           T2_fga3=LFGA3,
           T2_ftm=LFTM, 
           T2_fta=LFTA,
           T2_or=LOR, 
           T2_dr=LDR, 
           T2_ast=LAst, 
           T2_to=LTO, 
           T2_stl=LStl, 
           T2_blk=LBlk, 
           T2_pf=LPF          
    ),
    select(regresults,
           Season,
           DayNum,
           T1=LTeamID,
           T1_Points=LScore,
           T2=WTeamID,
           T2_Points=WScore,
           Location=WLoc,
           NumOT,
           T1_fgm=LFGM,
           T1_fga=LFGA,
           T1_fgm3=LFGM3,
           T1_fga3=LFGA3,
           T1_ftm=LFTM, 
           T1_fta=LFTA,
           T1_or=LOR, 
           T1_dr=LDR, 
           T1_ast=LAst, 
           T1_to=LTO, 
           T1_stl=LStl, 
           T1_blk=LBlk, 
           T1_pf=LPF,
           T2_fgm=WFGM,
           T2_fga=WFGA,
           T2_fgm3=WFGM3,
           T2_fga3=WFGA3,
           T2_ftm=WFTM, 
           T2_fta=WFTA,
           T2_or=WOR, 
           T2_dr=WDR, 
           T2_ast=WAst, 
           T2_to=WTO, 
           T2_stl=WStl, 
           T2_blk=WBlk, 
           T2_pf=WPF         
    ) %>% mutate(Location=ifelse(Location=='A','H',ifelse(Location=='H','A','N')))
  )

### Collect tourney results - double the data by swapping team positions

t1 = results[, c("Season", "DayNum", "WTeamID", "LTeamID", "WScore", "LScore")] %>% mutate(ResultDiff = WScore - LScore)
t2 = results[, c("Season", "DayNum", "LTeamID", "WTeamID", "LScore", "WScore")] %>% mutate(ResultDiff = LScore - WScore)
names(t1) = c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff")
names(t2) = c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff")
tourney = rbind(t1, t2)

tourney = 
  rbind(
    select(regresults,
           Season,
           DayNum,
           T1=WTeamID,
           T1_Points=WScore,
           T2=LTeamID,
           T2_Points=LScore,
           Location=WLoc       
    ),
    select(regresults,
           Season,
           DayNum,
           T1=LTeamID,
           T1_Points=LScore,
           T2=WTeamID,
           T2_Points=WScore,
           Location=WLoc 
    ) %>% mutate(Location=ifelse(Location=='A','H',ifelse(Location=='H','A','N')))
  ) %>%
  mutate(ResultDiff = T1_Points - T2_Points)

### Fit GLMM on regular season data (selected march madness teams only) - extract random effects for each team

march_teams = select(seeds, Season, Team = TeamID)
X =  regular_season %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T1" = "Team")) %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T2" = "Team")) %>% 
  select(Season, T1, T2, T1_Points, T2_Points, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

quality = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(T1_Points > T2_Points) ~  (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  quality[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}
quality = do.call(rbind, quality)

### Fit GLMM on regular season data (all teams all games) - extract random effects for each team

X =  regular_season %>% 
  select(Season, T1, T2, T1_Points, T2_Points, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

quality2 = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(T1_Points > T2_Points) ~  (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  quality2[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}
quality2 = do.call(rbind, quality2)

### Regular season statistics

season_summary = 
  regular_season %>%
  mutate(win14days = ifelse(DayNum > 118 & T1_Points > T2_Points, 1, 0),
         last14days = ifelse(DayNum > 118, 1, 0)) %>% 
  group_by(Season, T1) %>%
  summarize(
    WinRatio14d = sum(win14days) / sum(last14days),
    Points_mean=mean(T1_Points),
    Points_sd=sd(T1_Points),
    Points_median=median(T1_Points),
    fgm_mean=mean(T1_fgm),     
    fga_mean=mean(T1_fga), 
    fgp=sum(T1_fgm)/sum(T1_fga),
    fgm3_mean=mean(T1_fgm3), 
    fga3_mean=mean(T1_fga3),
    fg3p=sum(T1_fgm3)/sum(T1_fga3),    
    ftm_mean=mean(T1_ftm), 
    fta_mean=mean(T1_fta),
    ftp=sum(T1_ftm)/sum(T1_fta),        
    or_mean=mean(T1_or), 
    dr_mean=mean(T1_dr), 
    orp=sum(T1_or)/sum(T1_or+T1_dr),          
    ast_mean=mean(T1_ast), 
    to_mean=mean(T1_to),
    astto=sum(T1_ast)/sum(T1_to),        
    stl_mean=mean(T1_stl), 
    blk_mean=mean(T1_blk), 
    pf_mean=mean(T1_pf), 
    fgm3fgm2=sum(T1_fgm3)/sum(T1_fgm), 
    
    fgm_sd=sd(T1_fgm),     
    fga_sd=sd(T1_fga), 
    fgm3_sd=sd(T1_fgm3), 
    fga3_sd=sd(T1_fga3),
    ftm_sd=sd(T1_ftm), 
    fta_sd=sd(T1_fta),
    or_sd=sd(T1_or), 
    dr_sd=sd(T1_dr), 
    ast_sd=sd(T1_ast), 
    to_sd=sd(T1_to),
    stl_sd=sd(T1_stl), 
    blk_sd=sd(T1_blk), 
    pf_sd=sd(T1_pf),
    
    fgm_median=median(T1_fgm),     
    fga_median=median(T1_fga), 
    fgm3_median=median(T1_fgm3), 
    fga3_median=median(T1_fga3),
    ftm_median=median(T1_ftm), 
    fta_median=median(T1_fta),
    or_median=median(T1_or), 
    dr_median=median(T1_dr), 
    ast_median=median(T1_ast), 
    to_median=median(T1_to),
    stl_median=median(T1_stl), 
    blk_median=median(T1_blk), 
    pf_median=median(T1_pf),     
    
    OppFgmMean = mean(T2_fgm),
    OppFgaMean = mean(T2_fga),
    OppFgm3Mean = mean(T2_fgm3),
    OppFga3Mean = mean(T2_fga3),
    OppFgaMin = min(T2_fga)  
  )

season_summary_X1 = season_summary
season_summary_X2 = season_summary
names(season_summary_X1) = c("Season", "T1", paste0("X1_",names(season_summary_X1)[-c(1,2)]))
names(season_summary_X2) = c("Season", "T2", paste0("X2_",names(season_summary_X2)[-c(1,2)]))


### Combine all features into a data frame

data_matrix =
  tourney %>% 
  left_join(season_summary_X1, by = c("Season", "T1")) %>% 
  left_join(season_summary_X2, by = c("Season", "T2")) %>%
  left_join(select(seeds, Season, T1 = TeamID, X1_Seed = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, X2_Seed = Seed), by = c("Season", "T2")) %>% 
  mutate(SeedDiff = X1_Seed - X2_Seed) %>%
  left_join(select(quality, Season, T1 = Team_Id, X1_quality_march = quality), by = c("Season", "T1")) %>%
  left_join(select(quality, Season, T2 = Team_Id, X2_quality_march = quality), by = c("Season", "T2")) 
  #left_join(select(quality2, Season, T1 = Team_Id, X1_quality2_march = quality), by = c("Season", "T1")) %>%
  #left_join(select(quality2, Season, T2 = Team_Id, X2_quality2_march = quality), by = c("Season", "T2")) %>%
  #mutate(Location = as.numeric(as.factor(Location)))

data_matrix[is.na(data_matrix)] <- 0


### Prepare xgboost 

write.csv(data_matrix,file='data_matrix.csv',row.names = FALSE)

data_matrix <-
  data_matrix %>%
  filter(Season < 2019)

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
       num_parallel_tree = 1,
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
         

