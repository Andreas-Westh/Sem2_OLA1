library(dplyr)
library(jsonlite)
library(mongolite)


# Retrieve data from mongo
# Filter on polish and dutch
# Feature Engineering




#### Data Retrieval ####
cong <- mongo(collection = "games", db = "NewWyscout", url = "mongodb://localhost")
conm <- mongo(collection = "matches", db = "NewWyscout", url = "mongodb://localhost")
conp <- mongo(collection = "players", db = "NewWyscout", url = "mongodb://localhost")

#allpasses <- cong$find(query = '{"type.primary": "pass"}')
allshot <- cong$find(query = '{"type.primary": "shot"}')
allmatches <- conm$find(query = '{}')
allmatches_id <- conm$find(query = '{}', fields = '{"_id": 1}')




#### Data Preperation ####
allmatches <- cbind(allmatches_id, allmatches)
colnames(allmatches)[1] <- 'id'
allmatches_id <- NULL

# Flatten data
allshot <- jsonlite::flatten(allshot)


##### Filter for all, dutch and polish #####
dutch_id <- 635
polish_id <- 692

##### only for season 2021/2022 #####
dutch_matches <- allmatches %>% filter(competitionId == dutch_id)
dutch_matches_2122 <- dutch_matches %>% filter(seasonId == 187502)
dutch_teams <- dutch_matches_2122$home_team

polish_matches <- allmatches %>% filter(competitionId == polish_id)
polish_matches_2122 <- polish_matches %>% filter(seasonId == 186215)
polish_teams <- polish_matches_2122$home_team

allmatches_2122 <- rbind(dutch_matches_2122, polish_matches_2122)

## all shots in those seasons
allshot_2122 <- allshot %>% filter(allshot$matchId %in% allmatches_2122$id)

#### Remove deadball situations ####
dead_balls <- c("free_kick","corner","penalty")

allshot_xG <- allshot_2122 %>% filter(
                                      str_detect(type.secondary, "shot_after_") |
                                        !str_detect(possession.types, paste(dead_balls, collapse = "|"))
)
#
saveRDS(allshot_xG, "Opgave5_xG-Modelling/allshot_xG.RDS")



