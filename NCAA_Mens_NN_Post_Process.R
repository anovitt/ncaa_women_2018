library(data.table)
library(tidyverse)

# Load the data

sub <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MSampleSubmissionStage1_2020.csv")
seeds <- fread("R/kaggle_mania_2020_Men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MNCAATourneySeeds.csv")
nnResults <- fread("C:/Users/anovitt/AppData/Local/Programs/Python/Python36/Scripts/kaggle_mania_2020_men/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/submitProb.csv")

seeds$Seed = as.numeric(substring(seeds$Seed,2,3))

sub$Season = as.numeric(substring(sub$ID,1,4))
sub$T1 = as.numeric(substring(sub$ID,6,9))
sub$T2 = as.numeric(substring(sub$ID,11,14))

Z = sub %>% 
  left_join(select(seeds, Season, T1 = TeamID, X1_Seed = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, X2_Seed = Seed), by = c("Season", "T2"))
 
Z =
  Z %>%
  mutate(Pred = nnResults$V1[2:nrow(nnResults)])

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


write.csv(select(Z, ID, Pred), "nnsub.csv", row.names = FALSE)

