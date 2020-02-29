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
masey <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MMasseyOrdinals.csv")

seeds$Seed = as.numeric(substring(seeds$Seed,2,4))

valid_masey = group_by(masey,SystemName) %>% 
  summarize(nn=min(Season),mm=max(Season), n=n(), nd=n_distinct(TeamID)) %>% 
  filter(nn==2003,mm==2019)

last_rank = masey %>% 
  filter(SystemName %in% valid_masey$SystemName, RankingDayNum<=133) %>% 
  group_by(SystemName,TeamID) %>% 
  mutate(r=row_number(desc(RankingDayNum)))%>% 
  filter(r==1) %>% 
  select(-r,-RankingDayNum)

last_rank
last_rank = dcast(Season+TeamID~SystemName,data=last_rank,value.var='OrdinalRank')

last_rank[is.na(last_rank)] <- 200

tail(last_rank)

last_rank_T1 = last_rank; names(last_rank_T1) = paste0('T1_',names(last_rank))
last_rank_T2 = last_rank; names(last_rank_T2) = paste0('T2_',names(last_rank))

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
    off_eff=sum(T1_fgm + T1_fgm3)/sum(T1_fga+T1_fga3+T1_or+0.4*T1_fta),
    def_eff=sum(T2_fgm + T2_fgm3)/sum(T1_fga+T1_fga3+T1_or+0.4*T1_fta),
    
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

data_matrix = left_join(data_matrix,last_rank_T1,by=c("Season"="T1_Season",'T1'='T1_TeamID'))
data_matrix = left_join(data_matrix,last_rank_T2,by=c("Season"="T2_Season",'T2'='T2_TeamID'))

#data_matrix[is.na(data_matrix)] <- 200

data_matrix <- as.data.table(data_matrix)
#data_matrix[, T1_25Wins := rep(0,times = nrow(data_matrix))]
#data_matrix[, T2_25Wins := rep(0,times = nrow(data_matrix))]

top25DT <- 
data_matrix %>%
  filter( T1_AP <= 25 | T2_AP <= 25) %>%
  select(Season,T1,T1_Points,T2,T2_Points,T1_AP,T2_AP) %>%
  mutate(T1_25Wins= ifelse(T1_Points > T2_Points, 1, 0),
          T2_25Wins = ifelse(T2_Points > T1_Points,1 , 0)) %>%
  as.data.table()

top25DT <-
top25DT %>%
  group_by(Season, T1) %>%
  summarise(Wins25 = sum(T1_25Wins)) %>%
  as.data.table()

temp <- (names(data_matrix))

data_matrix <-
data_matrix %>%
  left_join(top25DT, by = c("Season" = "Season", "T1" = "T1")) %>%
  left_join(top25DT, by = c("Season" = "Season", "T2" = "T1")) 

names(data_matrix) <- c(temp,"T1_25Wins","T2_25_Wins")
    
data_matrix[is.na(data_matrix)] <- 0  
### Prepare xgboost 
## remove this for round 2

data_matrix <-
  data_matrix %>%
  filter(Season < 2019)

write.csv(data_matrix,file='data_matrix.csv',row.names = FALSE)



