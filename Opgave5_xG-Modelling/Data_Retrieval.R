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
allshot_2223 <- allshot %>%
  filter(!(matchId %in% allmatches_2122$id))



#### Remove deadball situations ####
dead_balls <- c("free_kick","corner","penalty")

allshot_xG <- allshot_2122 %>% filter(
                                      str_detect(type.secondary, "shot_after_") |
                                        !str_detect(possession.types, paste(dead_balls, collapse = "|"))
)

#### Better player positions ####
goalkeeper <- c("GK")

defense <- c("CB", "LCB", "RCB", "LCB3", "RCB3", 
             "RB", "LB", "RWB", "LWB", "RB5", "LB5")

midfield <- c("DMF", "RDMF", "LDMF", 
              "CMF", "RCMF", "LCMF", "RCMF3", "LCMF3", 
              "AMF", "RAMF", "LAMF")

attack <- c("CF", "SS", 
            "RW", "LW", "RWF", "LWF")

allshot_xG <- allshot_xG %>%
  mutate(position_category = case_when(
    player.position %in% goalkeeper ~ "Goalkeeper",
    player.position %in% defense ~ "Defense",
    player.position %in% midfield ~ "Midfield",
    player.position %in% attack ~ "Attack",
    TRUE ~ "Unknown"  # fallback if position is not in any vector
  ))

# Make from counter or other thing
allshot_xG <- allshot_xG %>% 
  mutate(from_counter = grepl("counter", possession.types))

from_counter_df <- allshot_xG %>% 
  group_by(from_counter) %>% 
  summarise(
    count=n())
# Make boolean over or under 80 min
allshot_xG <- allshot_xG %>% 
  mutate(late_game = case_when(
    minute >= 80 ~ TRUE,
    TRUE ~ FALSE
  ))
late_game_df <- allshot_xG %>% 
  group_by(late_game) %>% 
  summarise(
    count = n()
  )

saveRDS(allshot_xG, "Opgave5_xG-Modelling/allshot_xG.RDS")



