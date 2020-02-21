library(dplyr)
library(data.table)
library(xgboost)
library(lme4)
library(Metrics)

regresults_compact<-fread("R/kaggle_mania_2019_Men/DataFiles/RegularSeasonCompactResults.csv")
regresults<-fread("R/kaggle_mania_2019_Men/DataFiles/RegularSeasonDetailedResults.csv")
sub<-fread("R/kaggle_mania_2019_Men/SampleSubmissionStage1.csv")
seasons<-fread("R/kaggle_mania_2019_Men/DataFiles/Seasons.csv")
teams<-fread("R/kaggle_mania_2019_Men/DataFiles/teams.csv")
results_compact<-fread("R/kaggle_mania_2019_Men/DataFiles/NCAATourneyCompactResults.csv")
results<-fread("R/kaggle_mania_2019_Men/DataFiles/NCAATourneyDetailedResults.csv")
tourneySeeds=fread("R/kaggle_mania_2019_Men/DataFiles/NCAATourneySeeds.csv")
slots<-fread("R/kaggle_mania_2019_Men/DataFiles/NCAATourneySlots.csv")


#masey ordinals at last date before march madness; only ordinals for every season since 2003!
masey <- fread("R/kaggle_mania_2019_Men/MasseyOrdinals/MasseyOrdinals.csv")
#masey=fread('massey_ordinals_2003-2016.csv')
#masey2=fread('MasseyOrdinals_2017_ThruDay133_50systems.csv')
#masey=rbind(masey,masey2)

valid_masey = group_by(masey,SystemName) %>% 
  summarize(nn=min(Season),mm=max(Season), n=n(), nd=n_distinct(TeamID)) %>% 
  filter(nn==2003,mm==2018)

last_rank = masey %>% 
  filter(SystemName %in% valid_masey$SystemName, RankingDayNum<=133) %>% 
  group_by(SystemName,TeamID) %>% 
  mutate(r=row_number(desc(RankingDayNum)))%>% 
  filter(r==1) %>% 
  select(-r,-RankingDayNum)

last_rank
last_rank = dcast(Season+TeamID~SystemName,data=last_rank,value.var='OrdinalRank')

last_rank_T1 = last_rank; names(last_rank_T1) = paste0('T1_',names(last_rank))
last_rank_T2 = last_rank; names(last_rank_T2) = paste0('T2_',names(last_rank))



#repeat results twice with switched team positions
team_regresults = 
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


#team quality

team_regresults_nlm = team_regresults
team_regresults_nlm$T1=as.factor(team_regresults_nlm$T1)
team_regresults_nlm$T2=as.factor(team_regresults_nlm$T2)

mbt2003 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2003,], family = binomial) 
mbt2004 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2004,], family = binomial) 
mbt2005 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2005,], family = binomial) 
mbt2006 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2006,], family = binomial) 
mbt2007 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2007,], family = binomial) 
mbt2008 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2008,], family = binomial) 
mbt2009 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2009,], family = binomial) 
mbt2010 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2010,], family = binomial) 
mbt2011 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2011,], family = binomial) 
mbt2012 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2012,], family = binomial) 
mbt2013 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2013,], family = binomial) 
mbt2014 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2014,], family = binomial) 
mbt2015 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2015,], family = binomial) 
mbt2016 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2016,], family = binomial) 
mbt2017 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2017,], family = binomial)
mbt2018 <- glmer(I(T1_Points>T2_Points) ~  (1 | T1) + (1 | T2), data = team_regresults_nlm[team_regresults_nlm$Season==2018,], family = binomial) 

re2003 <- ranef(mbt2003)$T1;teamquality2003 = data.frame(Season=2003, Team_Id= as.numeric(row.names(re2003)),quality=exp(re2003[,"(Intercept)"]))
re2004 <- ranef(mbt2004)$T1;teamquality2004 = data.frame(Season=2004, Team_Id= as.numeric(row.names(re2004)),quality=exp(re2004[,"(Intercept)"]))
re2005 <- ranef(mbt2005)$T1;teamquality2005 = data.frame(Season=2005, Team_Id= as.numeric(row.names(re2005)),quality=exp(re2005[,"(Intercept)"]))
re2006 <- ranef(mbt2006)$T1;teamquality2006 = data.frame(Season=2006, Team_Id= as.numeric(row.names(re2006)),quality=exp(re2006[,"(Intercept)"]))
re2007 <- ranef(mbt2007)$T1;teamquality2007 = data.frame(Season=2007, Team_Id= as.numeric(row.names(re2007)),quality=exp(re2007[,"(Intercept)"]))
re2008 <- ranef(mbt2008)$T1;teamquality2008 = data.frame(Season=2008, Team_Id= as.numeric(row.names(re2008)),quality=exp(re2008[,"(Intercept)"]))
re2009 <- ranef(mbt2009)$T1;teamquality2009 = data.frame(Season=2009, Team_Id= as.numeric(row.names(re2009)),quality=exp(re2009[,"(Intercept)"]))
re2010 <- ranef(mbt2010)$T1;teamquality2010 = data.frame(Season=2010, Team_Id= as.numeric(row.names(re2010)),quality=exp(re2010[,"(Intercept)"]))
re2011 <- ranef(mbt2011)$T1;teamquality2011 = data.frame(Season=2011, Team_Id= as.numeric(row.names(re2011)),quality=exp(re2011[,"(Intercept)"]))
re2012 <- ranef(mbt2012)$T1;teamquality2012 = data.frame(Season=2012, Team_Id= as.numeric(row.names(re2012)),quality=exp(re2012[,"(Intercept)"]))
re2013 <- ranef(mbt2013)$T1;teamquality2013 = data.frame(Season=2013, Team_Id= as.numeric(row.names(re2013)),quality=exp(re2013[,"(Intercept)"]))
re2014 <- ranef(mbt2014)$T1;teamquality2014 = data.frame(Season=2014, Team_Id= as.numeric(row.names(re2014)),quality=exp(re2014[,"(Intercept)"]))
re2015 <- ranef(mbt2015)$T1;teamquality2015 = data.frame(Season=2015, Team_Id= as.numeric(row.names(re2015)),quality=exp(re2015[,"(Intercept)"]))
re2016 <- ranef(mbt2016)$T1;teamquality2016 = data.frame(Season=2016, Team_Id= as.numeric(row.names(re2016)),quality=exp(re2016[,"(Intercept)"]))
re2017 <- ranef(mbt2017)$T1;teamquality2017 = data.frame(Season=2017, Team_Id= as.numeric(row.names(re2017)),quality=exp(re2017[,"(Intercept)"]))
re2018 <- ranef(mbt2017)$T1;teamquality2018 = data.frame(Season=2018, Team_Id= as.numeric(row.names(re2018)),quality=exp(re2018[,"(Intercept)"]))

quality=rbind(teamquality2003,teamquality2004,teamquality2005,teamquality2006,teamquality2007,teamquality2008,teamquality2009,teamquality2010,
              teamquality2011,teamquality2012,teamquality2013,teamquality2014,teamquality2015,teamquality2016,teamquality2017,teamquality2018)



#overall season statistics

OVERALL = 
  team_regresults %>% 
  filter(NumOT == 0) %>%
  group_by(Season,T1) %>%
  summarize(
    
    T1_Points_mean=mean(T1_Points),
    T2_Points_mean=mean(T2_Points),
    T1_Points_sd=sd(T1_Points),
    T2_Points_sd=sd(T2_Points),    
    T1_Points_median=median(T1_Points),
    T2_Points_median=median(T2_Points),    
    T1_fgm_mean=mean(T1_fgm),     
    T1_fga_mean=mean(T1_fga), 
    T1_fgp=sum(T1_fgm)/sum(T1_fga),
    T1_fgm3_mean=mean(T1_fgm3), 
    T1_fga3_mean=mean(T1_fga3),
    T1_fg3p=sum(T1_fgm3)/sum(T1_fga3),    
    T1_ftm_mean=mean(T1_ftm), 
    T1_fta_mean=mean(T1_fta),
    T1_ftp=sum(T1_ftm)/sum(T1_fta),        
    T1_or_mean=mean(T1_or), 
    T1_dr_mean=mean(T1_dr), 
    T1_orp=sum(T1_or)/sum(T1_or+T1_dr),          
    T1_ast_mean=mean(T1_ast), 
    T1_to_mean=mean(T1_to),
    T1_astto=sum(T1_ast)/sum(T1_to),        
    T1_stl_mean=mean(T1_stl), 
    T1_blk_mean=mean(T1_blk), 
    T1_pf_mean=mean(T1_pf), 
    
    T2_fgm_mean=mean(T2_fgm),     
    T2_fga_mean=mean(T2_fga), 
    T2_fgp=sum(T2_fgm)/sum(T2_fga),
    T2_fgm3_mean=mean(T2_fgm3), 
    T2_fga3_mean=mean(T2_fga3),
    T2_fg3p=sum(T2_fgm3)/sum(T2_fga3),    
    T2_ftm_mean=mean(T2_ftm), 
    T2_fta_mean=mean(T2_fta),
    T2_ftp=sum(T2_ftm)/sum(T2_fta),        
    T2_or_mean=mean(T2_or), 
    T2_dr_mean=mean(T2_dr), 
    T2_orp=sum(T2_or)/sum(T2_or+T2_dr),          
    T2_ast_mean=mean(T2_ast), 
    T2_to_mean=mean(T2_to),
    T2_astto=sum(T2_ast)/sum(T2_to),        
    T2_stl_mean=mean(T2_stl), 
    T2_blk_mean=mean(T2_blk), 
    T2_pf_mean=mean(T2_pf), 
    
    T1_fgm_sd=sd(T1_fgm),     
    T1_fga_sd=sd(T1_fga), 
    T1_fgm3_sd=sd(T1_fgm3), 
    T1_fga3_sd=sd(T1_fga3),
    T1_ftm_sd=sd(T1_ftm), 
    T1_fta_sd=sd(T1_fta),
    T1_or_sd=sd(T1_or), 
    T1_dr_sd=sd(T1_dr), 
    T1_ast_sd=sd(T1_ast), 
    T1_to_sd=sd(T1_to),
    T1_stl_sd=sd(T1_stl), 
    T1_blk_sd=sd(T1_blk), 
    T1_pf_sd=sd(T1_pf),     
    
    T2_fgm_sd=sd(T2_fgm),     
    T2_fga_sd=sd(T2_fga), 
    T2_fgm3_sd=sd(T2_fgm3), 
    T2_fga3_sd=sd(T2_fga3),
    T2_ftm_sd=sd(T2_ftm), 
    T2_fta_sd=sd(T2_fta),
    T2_or_sd=sd(T2_or), 
    T2_dr_sd=sd(T2_dr), 
    T2_ast_sd=sd(T2_ast), 
    T2_to_sd=sd(T2_to),
    T2_stl_sd=sd(T2_stl), 
    T2_blk_sd=sd(T2_blk), 
    T2_pf_sd=sd(T2_pf),
    
    T1_fgm_median=median(T1_fgm),     
    T1_fga_median=median(T1_fga), 
    T1_fgm3_median=median(T1_fgm3), 
    T1_fga3_median=median(T1_fga3),
    T1_ftm_median=median(T1_ftm), 
    T1_fta_median=median(T1_fta),
    T1_or_median=median(T1_or), 
    T1_dr_median=median(T1_dr), 
    T1_ast_median=median(T1_ast), 
    T1_to_median=median(T1_to),
    T1_stl_median=median(T1_stl), 
    T1_blk_median=median(T1_blk), 
    T1_pf_median=median(T1_pf),     
    
    T2_fgm_median=median(T2_fgm),     
    T2_fga_median=median(T2_fga), 
    T2_fgm3_median=median(T2_fgm3), 
    T2_fga3_median=median(T2_fga3),
    T2_ftm_median=median(T2_ftm), 
    T2_fta_median=median(T2_fta),
    T2_or_median=median(T2_or), 
    T2_dr_median=median(T2_dr), 
    T2_ast_median=median(T2_ast), 
    T2_to_median=median(T2_to),
    T2_stl_median=median(T2_stl), 
    T2_blk_median=median(T2_blk), 
    T2_pf_median=median(T2_pf)
    
  )


OVERALL_P1=OVERALL;names(OVERALL_P1)=paste0("P1_",names(OVERALL))
OVERALL_P2=OVERALL;names(OVERALL_P2)=paste0("P2_",names(OVERALL))


# standard script from public scripts

getSeedDivision <- function(seedsInfo){
  seasonFromData <- seedsInfo[["Season"]]
  seedAndDivision <- seedsInfo[["Seed"]]
  teamFromData <- seedsInfo[["TeamID"]]
  seedTeam <- gsub(pattern = "[A-Z+a-z]", replacement = "", x = seedAndDivision)
  divisionTeam <- gsub(pattern = "[0-9]", replacement = "", x = seedAndDivision)
  divisionTeam <- gsub(pattern = "[a-z]", replacement = "", x = divisionTeam)  
  return(c(seasonFromData, teamFromData, seedTeam, divisionTeam))
}

seasons2Test <- seq(2003, 2018)
seedsAndDivisionsMatrix <- t(apply(tourneySeeds[Season %in% seasons2Test], 1, getSeedDivision))
seedsAndDivisionsMatrix[,3]=as.numeric(seedsAndDivisionsMatrix[,3])
seedsAndDivisions=data.frame(seedsAndDivisionsMatrix,stringsAsFactors=F)
names(seedsAndDivisions)=c('Season','TeamID','Seed','Division')
seedsAndDivisions[,1]=as.numeric(seedsAndDivisions[,1])
seedsAndDivisions[,2]=as.numeric(seedsAndDivisions[,2])
seedsAndDivisions[,3]=as.numeric(seedsAndDivisions[,3])


#calculate division strength; we take top16 teams from each division and calculate mean win/losing team points, demeaned by divisions

regresults2 = left_join(regresults,select(seedsAndDivisions,Season,TeamID,Division),by=c('Season'='Season','WTeamID'='TeamID'))
divisionT16stat = regresults2 %>% group_by(Season,Division) %>% summarize(T16n=n(), T16W=mean(WScore), T16L=mean(LScore)) %>% filter(!is.na(Division))
divisionT16stat = left_join(divisionT16stat, divisionT16stat %>% group_by(Season) %>% summarize(ot16W=sum(T16n*T16W)/sum(T16n),ot16L=sum(T16n*T16L)/sum(T16n)), 'Season')
divisionT16stat$T16W=divisionT16stat$T16W-divisionT16stat$ot16W
divisionT16stat$T16L=divisionT16stat$T16L-divisionT16stat$ot16L


TMatrixW=select(results,Season,DayNum,WTeamID,LTeamID,WScore,LScore) %>% mutate(S=1)
TMatrixL=select(results,Season,DayNum,LTeamID,WTeamID,LScore,WScore) %>% mutate(S=0)
names(TMatrixL)=names(TMatrixW)
TMatrix=rbind(TMatrixW,TMatrixL)


TMatrix2 =
  TMatrix %>% 
  left_join(select(seedsAndDivisions,Season,TeamID,Division),by=c('Season'='Season','WTeamID'='TeamID')) %>%
  left_join(select(divisionT16stat,Season,Division,T16W,T16L),by=c('Season'='Season','Division'='Division'))

TMatrix3 =
  TMatrix2 %>% 
  left_join(select(seedsAndDivisions,Season,TeamID,Division),by=c('Season'='Season','LTeamID'='TeamID')) %>%
  left_join(select(divisionT16stat,Season,Division,T16W,T16L),by=c('Season'='Season','Division.x'='Division'))


#final data matrix, everything joined

FDATA = 
  TMatrix3 %>% 
  left_join(OVERALL_P1,by=c('WTeamID'='P1_T1','Season'='P1_Season')) %>% 
  left_join(OVERALL_P2,by=c('LTeamID'='P2_T1','Season'='P2_Season')) %>%
  left_join(select(seedsAndDivisions,Season,TeamID,Seed1=Seed),by=c("WTeamID"='TeamID','Season'='Season')) %>% 
  left_join(select(seedsAndDivisions,Season,TeamID,Seed2=Seed),by=c("LTeamID"='TeamID','Season'='Season')) %>% 
  mutate(SeedD = Seed1-Seed2)


FDATA = left_join(FDATA,last_rank_T1,by=c("Season"="T1_Season",'WTeamID'='T1_TeamID'))
FDATA = left_join(FDATA,last_rank_T2,by=c("Season"="T2_Season",'LTeamID'='T2_TeamID'))
FDATA = left_join(FDATA,select(quality,Season,Team_Id,quality_T1=quality),by=c('Season'='Season','WTeamID'='Team_Id'))
FDATA = left_join(FDATA,select(quality,Season,Team_Id,quality_T2=quality),by=c('Season'='Season','LTeamID'='Team_Id'))

class(FDATA)

# model time!
# 5 fold cross-validation

set.seed(1)
FDATA$foldid = sample(
  c(
    rep(1,floor(nrow(FDATA)/5)),
    rep(2,floor(nrow(FDATA)/5)),
    rep(3,floor(nrow(FDATA)/5)),
    rep(4,floor(nrow(FDATA)/5)),
    rep(5,nrow(FDATA)-4*floor(nrow(FDATA)/5))
  )
)

folds=list()
folds[[1]]=which(FDATA$foldid==1)
folds[[2]]=which(FDATA$foldid==2)
folds[[3]]=which(FDATA$foldid==3)
folds[[4]]=which(FDATA$foldid==4)
folds[[5]]=which(FDATA$foldid==5)

# to run tensorflow model goto line 424 to build the submission file

feature.names=setdiff(names(FDATA),c('Season','WTeamID','LTeamID','DayNum','S','WScore','LScore','foldid','Division.x','Division.y'))
dtrain = xgb.DMatrix(as.matrix(FDATA[,feature.names]), label = FDATA$WScore-FDATA$LScore, missing=NaN)

param = list(objective = "reg:linear", 
             eval_metric = "rmse",
             booster = "gbtree", 
             eta = 0.05,
             subsample = 0.7,
             colsample_bytree = 0.7,
             num_parallel_tree = 1,
             #min_child_weight = 5,
             gamma = 10,
             max_depth = 25)


set.seed(120)
xgbcv=xgb.cv(
  params=param,
  data=dtrain,
  nrounds=70,
  verbose=1,
  nthread=12,
  folds=folds,
  print.every.n=1,
  early.stop.round=25,
  maximize=FALSE,
  prediction=TRUE
)

set.seed(120)
xgb=xgb.train(
  params=param,
  data=dtrain,
  nrounds=70,
  verbose=1,
  nthread=12,
  print.every.n=1,
  maximize=FALSE,
  prediction=TRUE
)




FDATA$pred1=xgbcv$pred
sm=smooth.spline(x=FDATA$pred1, y=ifelse(FDATA$WScore-FDATA$LScore>0,1,0)) #### IMPORTANT - apply smoothing to converted probabilities
logLoss(FDATA$S,FDATA$pred_s)
FDATA %>% group_by(Season) %>% summarize(logLoss(S,pred_s))



#making submission files

sub$Season=2017

sub$Season=as.numeric(substring(sub$ID,1,4))
sub$Team1=as.numeric(substring(sub$ID,6,9))
sub$Team2=as.numeric(substring(sub$ID,11,14))

subD =
  sub %>% 
  left_join(select(seedsAndDivisions,Season,TeamID,Division),by=c('Season'='Season','Team1'='TeamID')) %>%
  left_join(select(divisionT16stat,Season,Division,T16W,T16L),by=c('Season'='Season','Division'='Division'))

subD2 =
  subD %>% 
  left_join(select(seedsAndDivisions,Season,TeamID,Division),by=c('Season'='Season','Team2'='TeamID')) %>%
  left_join(select(divisionT16stat,Season,Division,T16W,T16L),by=c('Season'='Season','Division.x'='Division'))


subDATA = 
  subD2 %>% 
  left_join(OVERALL_P1,by=c('Team1'='P1_T1','Season'='P1_Season')) %>% 
  left_join(OVERALL_P2,by=c('Team2'='P2_T1','Season'='P2_Season')) %>%
  left_join(select(seedsAndDivisions,Season,TeamID,Seed1=Seed),by=c("Team1"='TeamID','Season'='Season')) %>% 
  left_join(select(seedsAndDivisions,Season,TeamID,Seed2=Seed),by=c("Team2"='TeamID','Season'='Season')) %>% 
  mutate(SeedD = Seed1-Seed2)

subDATA = left_join(subDATA,last_rank_T1,by=c("Season"="T1_Season",'Team1'='T1_TeamID'))
subDATA = left_join(subDATA,last_rank_T2,by=c("Season"="T2_Season",'Team2'='T2_TeamID'))
subDATA = left_join(subDATA,select(quality,Season,Team_Id,quality_T1=quality),by=c('Season'='Season','Team1'='Team_Id'))
subDATA = left_join(subDATA,select(quality,Season,Team_Id,quality_T2=quality),by=c('Season'='Season','Team2'='Team_Id'))

# to run the tensor flow model goto line 489
dtest = xgb.DMatrix(as.matrix(subDATA[,feature.names]), missing=NaN)

#conservative approach for first round 1/2 seeded teams
subDATA$pred1=predict(xgb,dtest)
subDATA$pred_s=predict(sm,subDATA$pred1)$y
subDATA$pred_s[subDATA$pred_s<=0.001]=0.001
subDATA$pred_s[subDATA$pred_s>=0.999]=0.999
subDATA$pred_s[subDATA$Seed1==16 & subDATA$Seed2==1]=0.001
subDATA$pred_s[subDATA$Seed1==1 & subDATA$Seed2==16]=0.999
subDATA$pred_s[subDATA$Seed1==15 & subDATA$Seed2==2]=0.07
subDATA$pred_s[subDATA$Seed1==2 & subDATA$Seed2==15]=0.93



#aggressive approach for first round 1/2 seeded teams
subDATA$pred_s2=subDATA$pred_s
subDATA$pred_s2[subDATA$Seed1==16 & subDATA$Seed2==1]=0
subDATA$pred_s2[subDATA$Seed1==1 & subDATA$Seed2==16]=1
subDATA$pred_s2[subDATA$Seed1==15 & subDATA$Seed2==2]=0
subDATA$pred_s2[subDATA$Seed1==2 & subDATA$Seed2==15]=1

subDATA<-select(subDATA,ID,Pred=pred_s)
subDATA <- as.data.table(subDATA)
subDATA<-unique(subDATA)
sub
write.csv(select(subDATA,ID,Pred=pred_s),'xgboostConser1.csv',row.names=F)
write.csv(select(subDATA,ID,Pred=pred_s2),'xgboostAgre.csv',row.names=F)

write.csv(subDATA,'xgboostConser1.csv',row.names=F)


##########
# Tesnsor Flow Model
##########

library(tfestimators)
library(keras)
library(tensorflow)

FRUN <- FDATA

col

FRUN$y_data <- FDATA$WScore-FDATA$LScore

#feature.names=setdiff(names(FDATA),c('Season','WTeamID','LTeamID','DayNum','S','WScore','LScore','foldid','Division.x','Division.y'))

FRUN[] <- lapply(FRUN, function(x){ 
  x[is.nan(x)] <- NA 
  x 
}) 


nacols <- function(x){
  y <- sapply(x, function(xx)any(is.na(xx)))
  names(y[y])
}  

nacols(FRUN)

colnames(FRUN)

feature.names=setdiff(names(FRUN),c('Season','WTeamID','LTeamID','DayNum','S','WScore','LScore','foldid','Division.x','Division.y',nacols(FRUN)))

      
FDATA_input_fn <- function(data, num_epochs = 1) {
  input_fn(data, 
           features = feature.names, 
           response = y_data,
           batch_size = 128,
           num_epochs = num_epochs)
}
  
cols <- feature_columns(
  column_numeric(feature.names)
  )

column_indicator("am")
#model <- linear_regressor(feature_columns = cols)
model <- dnn_regressor(hidden_units = c(243,243,243,243,243),feature_columns = cols,dropout = 0.5)

model %>% train(FDATA_input_fn(FRUN, num_epochs = 100))
model %>% evaluate(FDATA_input_fn(FRUN))

#obs<-FRUN[,feature.names]
predictions<-model %>% predict(FDATA_input_fn(FRUN))

pred<-unlist(predictions)
FRUN$Pred_tf<-pred

#FDATA$pred1=xgbcv$pred
sm=smooth.spline(x=FRUN$Pred_tf, y=ifelse(FRUN$y_data>0,1,0)) #### IMPORTANT - apply smoothing to converted probabilities

obs<-subDATA
obs$y_data<-rep(0, times = nrow(obs))

obs$pred1

#obs <- FRUN[,c(feature.names,"y_data") ]
predictions<-model %>% predict(FDATA_input_fn(obs))

pred<-unlist(predictions)

obs$Pred_tf<-pred

length(pred)
dim(obs)
subDATA$pred_TF=predict(sm,obs$Pred_tf)$y



subDATA$pred_TF[subDATA$pred_TF<=0.001]=0.001
subDATA$pred_TF[subDATA$pred_TF>=0.999]=0.999
subDATA$pred_TF[subDATA$Seed1==16 & subDATA$Seed2==1]=0.001
subDATA$pred_TF[subDATA$Seed1==1 & subDATA$Seed2==16]=0.999
subDATA$pred_TF[subDATA$Seed1==15 & subDATA$Seed2==2]=0.07
subDATA$pred_TF[subDATA$Seed1==2 & subDATA$Seed2==15]=0.93

subDATA<-select(subDATA,ID,Pred=pred_TF)

subDATA <- as.data.table(subDATA)
subDATA<-unique(subDATA)
