#### Opgave 2.1 - Afleveringer ####
# Lav beskrivende statistik på afleveringer i henholdsvis den polske og hollandske liga. 
# I repræsenterer IKKE et unikt hold, men er alene ekstern analytiker ansat hos fx Wyscout. 
# Der er minimum fem elementer som det ønskes I kigger nærmere på:

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

# Retrieve all matches
allmatches <- conm$find(query = '{}', fields = '{}')
allevents_flat <- confe$find( query = '{}', fields = '{}')
allevents <- cone$find( query = '{}', fields = '{}')

# Filter matches for the selected countries
allDutch <- allmatches %>% filter(competitionId == 635)
allPolish <- allmatches %>% filter(competitionId == 692)

# Extract match id
id_ducth <- allDutch[,'_id']

# MongoDB query for events within the match IDs
query_d <- jsonlite::toJSON(list(`_id` = list(`$in` = id_ducth)), auto_unbox = TRUE)
result_d <- cong$find(query = query_d, fields = '{}')
testl_d <- result_d$events
testdf_d <- bind_rows(testl_d)

# Convert to dataframe and filter for the selected team
resdf_d <- fromJSON(toJSON(testdf_d), flatten = TRUE)

# Filter only passes
df_passes_d <- resdf_d %>% filter(type.primary == "pass")

##### 1. Hvordan er afleveringerne for henholdsvis top og bund 5 holdene for sæsonen 2021/2022 i den polske og hollandske liga? #####




##### 2. Hvordan er afleveringerne sidst i kampene i sæsonen 2021/2022, efter minut 80, sammenlignet med resten af kampen for henholdsvis den polske og hollandske liga? #####




##### 3. Hvordan er længde på afleveringerne, der er ”assist” sammenlignet med afleveringerne, der ikke er ”assist” for henholdsvis den polske og hollandske liga? #####




##### 4. Hvor en del af afleveringerne, der er ”assist” kommer fra henholdsvis forsvars-, midtbane- og angrebsspillere? (Hint: husk at præsentere jeres definition af de tre grupper) #####




##### 5. Hvilke 10 spillere lavede flest assist i henholdsvis den polske og hollandske liga? (Sammenlign fx med spillernes xA) #####