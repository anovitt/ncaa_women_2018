### Retrieve Kenpom Ratings from kenpom.com
#   Kenpom ratings are a pythangorean transformation of tempo free NCAABB stats 
#   that have been compiled by Ken Pomeroy since 2002 
### Adapted from: https://github.com/lbenz730/NCAA_Hoops_Play_By_Play/blob/master/kenpom_scraper.R

library(XML)
library(RCurl)
library(dplyr)
library(data.table)

years <- 2002:2020

for(year in years) {
  cat("Getting", year,"\n")
  ### Pull Data
  url <- paste0("https://kenpom.com/index.php?y=", year)
  x <- as.data.frame(readHTMLTable(getURL(url))[[1]])
  
  ### Clean Data
  names(x) <- c("rank", "team", "conference", "record", "adj_em", "adj_o", 
                "adj_o_rank", "adj_d", "adj_d_rank", "adj_tempo", "adj_tempo_rank", 
                "luck", "luck_rank", "sos_adj_em", "sos_adj_em_rank", "sos_adj_o",
                "sos_adj_o_rank","sos_adj_d", "sos_adj_d_rank", "nc_sos_adj_em", 
                "nc_sos_adj_em_rank")
  x <- filter(x, !team %in% c("", "Team"))
  for(i in 5:ncol(x)) {
    x[,i] <- as.numeric(as.character(x[,i]))
  }
  
  x <- mutate(x, 
              "ncaa_seed" = sapply(team, function(arg) { as.numeric(gsub("[^0-9]", "", arg)) }),
              "team" = sapply(team, function(arg) { gsub("\\s[0-9]+", "", arg) }),
              "year" = year)
  
  ### Store Data
  if(year == 2002) {
    kenpom <- x
  }else {
    kenpom <- rbind(kenpom, x)
  }
}

setDT(kenpom)

####Clean Team Names so that they can be merged to NCAA data
# Replacing Southern with Southen Univ forces recorrecting TX Southern & Miss Southern
kenpom[,TeamName:=gsub("\\.","",team)]
kenpom[,TeamName:=gsub("Cal St","CS",TeamName)]
kenpom[,TeamName:=gsub("Albany","SUNY Albany",TeamName)]
kenpom[,TeamName:=gsub("Abilene Christian","Abilene Chr",TeamName)]
kenpom[,TeamName:=gsub("American","American Univ",TeamName)]
kenpom[,TeamName:=gsub("Arkansas Little Rock","Ark Little Rock",TeamName)]
kenpom[,TeamName:=gsub("Arkansas Pine Bluff","Ark Pine Bluff",TeamName)]
kenpom[,TeamName:=gsub("Boston University","Boston Univ",TeamName)]
kenpom[,TeamName:=gsub("Central Michigan","C Michigan",TeamName)]
kenpom[,TeamName:=gsub("Central Connecticut","Central Conn",TeamName)]
kenpom[,TeamName:=gsub("Coastal Carolina","Coastal Car",TeamName)]
kenpom[,TeamName:=gsub("East Carolina","E Kentucky",TeamName)]
kenpom[,TeamName:=gsub("Eastern Washington","E Washington",TeamName)]
kenpom[,TeamName:=gsub("East Tennessee St","ETSU",TeamName)]
kenpom[,TeamName:=gsub("Fairleigh Dickinson","F Dickinson",TeamName)]
kenpom[,TeamName:=gsub("Florida Atlantic","FL Atlantic",TeamName)]
kenpom[,TeamName:=gsub("Florida Gulf Coast","FL Gulf Coast",TeamName)]
kenpom[,TeamName:=gsub("George Washington","G Washington",TeamName)]
kenpom[,TeamName:=gsub("Illinois Chicago","IL Chicago",TeamName)]
kenpom[,TeamName:=gsub("Kent St","Kent",TeamName)]
kenpom[,TeamName:=gsub("Monmouth","Monmouth NJ",TeamName)]
kenpom[,TeamName:=gsub("Mississippi Valley St","MS Valley St",TeamName)]
kenpom[,TeamName:=gsub("Mount St Mary's","Mt St Mary's",TeamName)]
kenpom[,TeamName:=gsub("Montana St","MTSU",TeamName)]
kenpom[,TeamName:=gsub("Northern Colorado","N Colorado",TeamName)]
kenpom[,TeamName:=gsub("North Dakota St","N Dakota St",TeamName)]
kenpom[,TeamName:=gsub("Northern Kentucky","N Kentucky",TeamName)]
kenpom[,TeamName:=gsub("North Carolina A&T","NC A&T",TeamName)]
kenpom[,TeamName:=gsub("North Carolina Central","NC Central",TeamName)]
kenpom[,TeamName:=gsub("North Carolina St","NC State",TeamName)]
kenpom[,TeamName:=gsub("Northwestern St","Northwestern LA",TeamName)]
kenpom[,TeamName:=gsub("Prairie View A&M","Prairie View",TeamName)]
kenpom[,TeamName:=gsub("South Carolina St","S Carolina St",TeamName)]
kenpom[,TeamName:=gsub("South Dakota St","S Dakota St",TeamName)]
kenpom[,TeamName:=gsub("Southern Illinois","S Illinois",TeamName)]
kenpom[,TeamName:=gsub("Southeastern Louisiana","SE Louisiana",TeamName)]
kenpom[,TeamName:=gsub("Stephen F Austin","SF Austin",TeamName)]
kenpom[,TeamName:=gsub("Southern","Southern Univ",TeamName)]
kenpom[,TeamName:=gsub("Southern Univ Miss","Southern Miss",TeamName)]
kenpom[,TeamName:=gsub("Saint Joseph's","St Joseph's PA",TeamName)]
kenpom[,TeamName:=gsub("Saint Louis","St Louis",TeamName)]
kenpom[,TeamName:=gsub("Saint Mary's","St Mary's CA",TeamName)]
kenpom[,TeamName:=gsub("Saint Peter's","St Peter's",TeamName)]
kenpom[,TeamName:=gsub("Texas A&M Corpus Chris","TAM C. Christi",TeamName)]
kenpom[,TeamName:=gsub("Troy St","Troy",TeamName)]
kenpom[,TeamName:=gsub("Texas Southern Univ","TX Southern",TeamName)]
kenpom[,TeamName:=gsub("Louisiana Lafayette","Louisiana",TeamName)]
kenpom[,TeamName:=gsub("UTSA","UT San Antonio",TeamName)]
kenpom[,TeamName:=gsub("Western Michigan","W Michigan",TeamName)]
kenpom[,TeamName:=gsub("Green Bay","WI Green Bay",TeamName)]
kenpom[,TeamName:=gsub("Milwaukee","WI Milwaukee",TeamName)]
kenpom[,TeamName:=gsub("Western Kentucky","WKU",TeamName)]
kenpom[,TeamName:=gsub("College of Charleston","Col Charleston",TeamName)]
kenpom[,TeamName:=gsub("Loyola Chicago","Loyola-Chicago",TeamName)]

###### Validate match by all team-year combinations ####
#Load teams & merge to seeds for all tourney combinations
teams <- fread("../input/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MTeams.csv")
tourney_seeds <- fread("../input/google-cloud-ncaa-march-madness-2020-division-1-mens-tournament/MDataFiles_Stage1/MNCAATourneySeeds.csv")
teamyr <- merge(teams,tourney_seeds,by="TeamID")
teamyr

#Merge Team-Years to Kenpom and check for any missing seeds
teamKenpom <- merge(teamyr,kenpom,by.x=c("TeamName","Season"),by.y=c("TeamName","year"),all.x = T)

#Should be zero length data table (meaning no unmatched NCAA tournament teams) or display unmatched team-year combinations
teamKenpom[Season>=2002 & is.na(Seed),.(TeamName,Season)]

#write out file
fwrite(teamKenpom[Season>=2002,],"NCAA2020_Kenpom.csv")