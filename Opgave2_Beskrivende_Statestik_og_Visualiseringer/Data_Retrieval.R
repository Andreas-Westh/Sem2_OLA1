library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(ggplot2)
library(factoextra)


##### Data Retrieval #####
cong <- mongo(collection = "games", db = "soccer", url = "mongodb://localhost")
conm <- mongo(collection = "matches", db = "soccer", url = "mongodb://localhost")
conp <- mongo(collection = "players", db = "wyscout", url = "mongodb://localhost")
confe <- mongo(collection = "flattened_events", db = "soccer", url = "mongodb://localhost")
cone <- mongo(collection = "events", db = "soccer", url = "mongodb://localhost")

allpasses_flat <- confe$find(query = '{"events.type.primary": "pass"}')
