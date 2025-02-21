library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(ggplot2)
library(factoextra)


##### Data Retrieval #####
cong <- mongo(collection = "games", db = "NewWyscout", url = "mongodb://localhost")
conm <- mongo(collection = "matches", db = "NewWyscout", url = "mongodb://localhost")
conp <- mongo(collection = "players", db = "NewWyscout", url = "mongodb://localhost")


allpasses <- cong$find(query = '{"type.primary": "pass"}')
allshot <- cong$find(query = '{"type.primary": "shot"}')
allmatches <- conm$find(query = '{}')





# Procent of shots is goal
isGoal_df <- as.data.frame(allshot$shot$isGoal)
colnames(isGoal_df) <- "isGoal"
goal_percentage <- mean(isGoal_df$isGoal) * 100
print(goal_percentage)





