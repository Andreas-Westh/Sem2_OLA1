library(rvest)

#### Opgave 2.1 - Afleveringer ####
# Lav beskrivende statistik på afleveringer i henholdsvis den polske og hollandske liga. 
# I repræsenterer IKKE et unikt hold, men er alene ekstern analytiker ansat hos fx Wyscout. 
# Der er minimum fem elementer som det ønskes I kigger nærmere på:

##### 1. Hvordan er afleveringerne for henholdsvis top og bund 5 holdene for sæsonen 2021/2022 i den polske og hollandske liga? #####
# https://www.eurosport.com/football/eredivisie/2023-2024/standings.shtml

###### Simple Team Ranking WebScrape ######
      # URL for rankings
      url <- "https://www.eurosport.com/football/eredivisie/2023-2024/standings.shtml"
      
      # read the page html
      page <- read_html(url)
      
      # extract the teams name
      teams_ranking <- page %>% 
        html_nodes('a.absolute.right-1.max-w-full.truncate.left-8.lg\\:caps-s5-fx.hidden.md\\:block') %>% 
        html_text(trim = TRUE) %>% as.data.frame()
      
      # since the df is already in order, row name == ranking
      teams_ranking$ranking <- row.names(teams_ranking)
      colnames(teams_ranking) <- c("team","ranking")

top_5 <- head(teams_ranking$team,5)
bottom_5 <- tail(teams_ranking$team,5)

top_5_passes <- allpasses %>% filter(events$team$name %in% top_5)
bottom_5_passes <- allpasses %>% filter(events$team$name %in% bottom_5)

# FC Twente doesnt exist within the df, maybe data is from an older season?

# Pass length
hist(top_5_passes$events$pass$length)
hist(bottom_5_passes$events$pass$length)



# Pass successrate

# Secondary passtypes? 
  # How many lead to assits
  
# Pass angle



##### 2. Hvordan er afleveringerne sidst i kampene i sæsonen 2021/2022, efter minut 80, sammenlignet med resten af kampen for henholdsvis den polske og hollandske liga? #####




##### 3. Hvordan er længde på afleveringerne, der er ”assist” sammenlignet med afleveringerne, der ikke er ”assist” for henholdsvis den polske og hollandske liga? #####




##### 4. Hvor en del af afleveringerne, der er ”assist” kommer fra henholdsvis forsvars-, midtbane- og angrebsspillere? (Hint: husk at præsentere jeres definition af de tre grupper) #####




##### 5. Hvilke 10 spillere lavede flest assist i henholdsvis den polske og hollandske liga? (Sammenlign fx med spillernes xA) #####