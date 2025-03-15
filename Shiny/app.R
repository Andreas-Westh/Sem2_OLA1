############################
##### Opgave 4 – Shiny #####
############################
library(mongolite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
library(ggsoccer)
library(plotly)

########################################
#######  Connection to MongoDB ########
########################################
#conm <- mongo(url = "mongodb://localhost", db = "NewWyscout", collection = "matches")
#cong <- mongo(url = "mongodb://localhost", db = "NewWyscout", collection = "games")
#cond <- mongo(url = "mongodb://localhost", db = "NewWyscout", collection = "players")

###############################
####### Data retrieval ########
###############################




#allmatches <- conm$find(query = "{}", fields = "{}")
#allplayers <- cond$find(query = "{}", fields = "{}")
#allshots <- cong$find(query = '{"type.primary": "shot"}', fields = "{}")

#allpasses <- cong$find(query = '{"type.primary": "passes"}', fields = "{}")
allmatches <- readRDS("allmatches.rds")
allshots <- readRDS("allshots.rds")
allplayers <- readRDS("allplayers.rds")
allpasses <- readRDS("passesflat.rds")
allshots <- jsonlite::flatten(allshots)
allplayers <- jsonlite::flatten(allplayers)

###################################################
########## data cleaning & manipulation #############
###################################################

#### Skud-data (til skud og spillere) ####
Polske_kampe <- allmatches %>% filter(competitionId == 692)
Hollandske_kampe <- allmatches %>% filter(competitionId == 635)

Polen_match_ids <- Polske_kampe$`_id`
Hollandske_match_ids <- Hollandske_kampe$`_id`

Polske_skud <- allshots %>% filter(matchId %in% Polen_match_ids)
Hollandske_skud <- allshots %>% filter(matchId %in% Hollandske_match_ids)

alle_skud <- bind_rows(
  Polske_skud %>% mutate(Liga = "Polen"),
  Hollandske_skud %>% mutate(Liga = "Holland")
) %>%
  left_join(allmatches %>% select(`_id`, seasonId, label), by = c("matchId" = "_id"))

# Inkluder label i den rene skuddata
alle_skud_clean <- alle_skud %>%
  select(matchId, player.name, team.name, Liga, label, seasonId, location.x, location.y,
         shot.isGoal, shot.onTarget, shot.bodyPart, shot.xg, matchTimestamp, opponentTeam.name)

#### Afleverings-data (til afleveringsoversigt) ####
matches_with_labels <- allmatches %>% select(`_id`, label)

# Opret allpasses_with_labels ved at kombinere allpasses med matches_with_labels
allpasses_with_labels <- allpasses %>%
  left_join(matches_with_labels, by = c("matchId" = "_id"))

# Fjern duplikerede matchId-værdier i alle_skud_clean
alle_skud_clean_unique <- alle_skud_clean %>%
  distinct(matchId, .keep_all = TRUE) %>%
  select(matchId, opponentTeam.name)

############################
####  Dataframe & stats ####
############################

# Opret player_passes uden many-to-many-problemer
player_passes <- allpasses_with_labels %>%
  group_by(matchId, label, team.id, team.name, opponentTeam.name) %>%
  summarise(
    Total_passes      = n(),
    Accurate_passes   = sum(pass.accurate),
    Inaccurate_passes = sum(!pass.accurate),
    .groups = "drop"
  ) %>%
  left_join(allmatches %>% select(`_id`, competitionId), by = c("matchId" = "_id")) %>%
  mutate(
    Liga = case_when(
      competitionId == 692 ~ "Polen",
      competitionId == 635 ~ "Holland",
      TRUE ~ "Andet"
    )
  )

#### Spiller-mål-data (til skud og spillere) ####
player_goals <- alle_skud %>%
  group_by(player.name, player.id, Liga, seasonId, team.name) %>%
  summarise(
    Position = names(which.max(table(player.position))),
    Total_Shots = n(),
    Avg_shot_xg = round(mean(shot.xg), 3),
    sd_xg = round(sd(shot.xg), 3),
    xG_Variation = round((Avg_shot_xg / sd_xg), 3),
    Total_goals = sum(shot.isGoal == 1),
    Afslutningseffektivitet = paste0(round((Total_goals / Total_Shots) * 100, 1), "%"),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_goals))

# Spiller-assist
player_assists <- allpasses_with_labels %>%
  filter(grepl("shot_assist", type.secondary)) %>%
  left_join(allmatches %>% select(`_id`, seasonId), by = c("matchId" = "_id")) %>%
  group_by(player.id, player.name, team.name, seasonId) %>%
  summarise(assist = n(), .groups = "drop")

player_goals <- player_goals %>%
  left_join(player_assists, by = c("player.id", "player.name", "team.name", "seasonId"))


################# ####################### ################
################# #####   SHINY     ##### ################                                         
################# ####################### ################

#####################################
####### UI-del (forenklet) ##########
#####################################

ui <- dashboardPage(
  dashboardHeader(title = "Fodboldanalyse"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Skud og Spillere", tabName = "skud_spillere", selected = TRUE),
      menuItem("Afleveringer og Spillere", tabName = "afleveringer"),
      menuItem("Skud i en kamp", tabName = "skud_kamp")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Behold dashboardets mørke baggrund */
        .content-wrapper, .right-side { background-color: #3B4C5D; }
        .box { background-color: #3B4C5D; color: #ffffff; }
        .box-title { color: #ffffff; }
        .sidebar-menu>li>a { background-color: transparent; color: black; }
        .main-header .logo { background-color: transparent !important; }
        .main-header .navbar { background-color: #3B4C5D; }
        .skin-blue .main-header .navbar { background-color: #2a3135; }
        table.dataTable { 
          color: black !important; 
          background-color: white !important; 
          opacity: 0.75;
        }
        table.dataTable tbody tr { 
          background-color: rgba(255, 255, 255, 0.75) !important; 
          color: black !important; 
        }
        table.dataTable thead { 
          background-color: rgba(248, 249, 250, 0.75) !important; 
          color: black !important; 
        }
      "))
    ),
    tabItems(
      # Fane 1: Skud & Spillere
      tabItem(
        tabName = "skud_spillere",
        fluidRow(
          box(
            width = 4,
            title = NULL,
            radioButtons("season", "Vælg sæson:",
                         choices = c("Alle", "21/22", "22/23"),
                         selected = "Alle", inline = TRUE),
            selectInput("liga", "Vælg liga:", choices = c("Alle", unique(player_goals$Liga))),
            uiOutput("team_ui"),
            sliderInput("min_goals", "Flere mål end:", min = 0, max = 25, value = 9)
          ),
          box(
            width = 8,
            title = "Spillernes mål & xG variation",
            plotOutput("barplot")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Detaljeret spillertabel",
            DT::dataTableOutput("player_table")
          )
        )
      ),
      # Fane 2: Afleveringer og Spillere
      tabItem(
        tabName = "afleveringer",
        fluidRow(
          box(
            width = 4,
            title = NULL,
            selectInput("liga_pass", "Vælg liga:", choices = c("Alle", unique(player_passes$Liga)),
                        selected = "Holland"),
            uiOutput("team_ui_pass"),
            uiOutput("match_ui_pass")
          ),
          box(
            width = 8,
            title = "Afleveringer fordelt pr. hold. pr. kamp inkl. præcision",
            plotOutput("pass_barplot")
          )
        )
      ),
      # Fane 3: Skud i en kamp
      tabItem(
        tabName = "skud_kamp",
        fluidRow(
          box(
            width = 4,
            title = NULL,
            selectInput("liga_shot", "Vælg liga:",
                        choices = c("Alle", unique(alle_skud_clean$Liga)),
                        selected = "Holland"),
            uiOutput("team_ui_shot"),
            uiOutput("match_ui_shot")
          ),
          box(
            width = 8,
            title = "Skud fordelt på hold",
            plotlyOutput("shot_plot", height = "600px")
          )
        )
      )
    )
  )
)

#####################################
####### SERVER-del  ###############
#####################################

server <- function(input, output, session) {
  
  season_mapping <- list(
    "21/22" = c(186215, 187502),
    "22/23" = c(188088, 188125)
  )
  
  output$team_ui <- renderUI({
    req(input$liga)
    teams <- unique(player_goals$team.name[player_goals$Liga == input$liga])
    if (length(teams) == 0) teams <- "Ingen hold"
    selectInput("team", "Vælg hold:", choices = c("Alle hold", teams))
  })
  
  filtered_data <- reactive({
    data <- player_goals
    if (input$season != "Alle") {
      valid_ids <- season_mapping[[input$season]]
      data <- data %>% filter(seasonId %in% valid_ids)
    }
    if (input$liga != "Alle") {
      data <- data %>% filter(Liga == input$liga)
    }
    if (input$team != "Alle hold" && input$team != "Ingen hold") {
      data <- data %>% filter(team.name == input$team)
    }
    data <- data %>% filter(Total_goals >= input$min_goals)
    data
  })
  
  output$barplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = reorder(player.name, Total_goals))) +
      theme_dark() +
      geom_bar(aes(y = Total_goals, fill = Liga), stat = "identity", alpha = 0.7) +
      geom_point(aes(y = xG_Variation), color = "red", size = 3) +
      annotate("point", x = Inf, y = Inf, color = "red", size = 3) +
      geom_hline(yintercept = 1, color = "white", linetype = "dashed", size = 1) +
      coord_flip() +
      labs(x = "Spiller", y = "Antal mål") +
      scale_y_continuous(sec.axis = sec_axis(~ ., name = "xG Variation Index")) +
      scale_fill_manual(values = c("royalblue4", "dodgerblue3")) +
      theme(
        plot.background = element_rect(fill = "#2E3440"),
        panel.background = element_rect(fill = "#2E3440"),
        panel.grid.major = element_line(color = "#4C566A", size = 0.2),
        panel.grid.minor = element_line(color = "#4C566A", size = 0.1),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title.x = element_text(face = "bold", color = "white"),
        axis.title.y = element_text(face = "bold", color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#3B4252", color = "#434C5E"),
        legend.key = element_rect(fill = "#3B4252", color = "#434C5E"),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", color = "white")
      ) +
      guides(fill = guide_legend(title = NULL, override.aes = list(shape = NA)),
             point = guide_legend(title = NULL, override.aes = list(shape = 16)))
  })
  
  output$player_table <- DT::renderDataTable({
    data <- filtered_data()
    data <- data[, !(names(data) %in% c("player.id", "seasonId", "Liga", "team.name", "sd_xg"))]
    DT::datatable(data, options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
  })
  
  output$team_ui_pass <- renderUI({
    req(input$liga_pass)
    teams <- unique(player_passes$team.name[player_passes$Liga == input$liga_pass])
    if (length(teams) == 0) teams <- "Ingen hold"
    selected_team <- if("Ajax" %in% teams) "Ajax" else "Alle hold"
    selectInput("team_pass", "Vælg hold:", 
                choices = c("Alle hold", teams), 
                selected = selected_team)
  })
  
  output$match_ui_pass <- renderUI({
    req(input$team_pass)
    matches <- player_passes %>%
      filter(team.name == input$team_pass) %>%
      pull(label) %>%
      unique()
    if (length(matches) == 0) matches <- "Ingen kampe"
    selected_match <- if("Ajax - AZ, 1-2" %in% matches) "Ajax - AZ, 1-2" else "Alle kampe"
    selectInput("match_pass", "Vælg kamp:", 
                choices = c("Alle kampe", matches), 
                selected = selected_match)
  })
  
  filtered_pass_data <- reactive({
    data <- player_passes
    if (input$liga_pass != "Alle") {
      data <- data %>% filter(Liga == input$liga_pass)
    }
    if (!is.null(input$team_pass) && input$team_pass != "Alle hold" && input$team_pass != "Ingen hold") {
      data <- data %>% filter(team.name == input$team_pass)
    }
    if (!is.null(input$match_pass) && input$match_pass != "Alle kampe" && input$match_pass != "Ingen kampe") {
      data <- data %>% filter(label == input$match_pass)
      opponent_team <- unique(data$opponentTeam.name)
      data <- data %>% bind_rows(
        player_passes %>% filter(team.name == opponent_team & label == input$match_pass)
      )
    }
    data
  })
  
  output$pass_barplot <- renderPlot({
    req(filtered_pass_data())
    pass_data <- filtered_pass_data() %>%
      mutate(
        Accurate_pct = round(Accurate_passes / Total_passes * 100, 1),
        Inaccurate_pct = round(Inaccurate_passes / Total_passes * 100, 1)
      )
    
    ggplot(pass_data, aes(x = team.name)) +
      theme_dark() +
      geom_bar(aes(y = Total_passes, fill = "Total afleveringer"), stat = "identity", alpha = 0.7) +
      geom_bar(aes(y = Accurate_passes, fill = "Præcise afleveringer"), stat = "identity", alpha = 0.7) +
      geom_bar(aes(y = Inaccurate_passes, fill = "Upræcise afleveringer"), stat = "identity", alpha = 0.7) +
      geom_text(aes(y = Accurate_passes, label = paste0(Accurate_pct, "%")), 
                vjust = 1.5, color = "white", size = 4) +
      geom_text(aes(y = Inaccurate_passes, label = paste0(Inaccurate_pct, "%")), 
                vjust = 1.5, color = "white", size = 4) +
      labs(x = "Hold", y = "Antal afleveringer") + 
      scale_fill_manual(values = c("Total afleveringer" = "royalblue4", 
                                   "Præcise afleveringer" = "dodgerblue3", 
                                   "Upræcise afleveringer" = "skyblue")) +
      theme(
        plot.background = element_rect(fill = "#2E3440"),  
        panel.background = element_rect(fill = "#2E3440"),  
        panel.grid.major = element_line(color = "#4C566A", size = 0.2),  
        panel.grid.minor = element_line(color = "#4C566A", size = 0.1),  
        legend.position = "bottom",
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(face = "bold", size = 12, color = "white"),
        axis.title.y = element_text(face = "bold", color = "white"),
        legend.text = element_text(face = "bold", color = "white"),
        legend.background = element_rect(fill = "#3B4252", color = "#434C5E"),
        legend.key = element_rect(fill = "#3B4252", color = "#434C5E"),
        legend.title = element_blank()
      )
  })
  
  output$team_ui_shot <- renderUI({
    req(input$liga_shot)
    teams <- unique(alle_skud_clean$`team.name`[alle_skud_clean$Liga == input$liga_shot])
    if (length(teams) == 0) teams <- "Ingen hold"
    default_team <- if("Ajax" %in% teams) "Ajax" else "Alle hold"
    selectInput("team_shot", "Vælg hold:", choices = c("Alle hold", teams), selected = default_team)
  })
  
  output$match_ui_shot <- renderUI({
    req(input$team_shot)
    df <- alle_skud_clean
    if (input$liga_shot != "Alle") {
      df <- df %>% filter(Liga == input$liga_shot)
    }
    if (input$team_shot != "Alle hold" && input$team_shot != "Ingen hold") {
      df <- df %>% filter(`team.name` == input$team_shot)
    }
    kamp_list <- unique(df$label)
    if (length(kamp_list) == 0) kamp_list <- "Ingen kampe"
    selectInput("match_shot", "Vælg kamp:", choices = c("Alle kampe", kamp_list))
  })
  
  filtered_shots_for_selected_match <- reactive({
    df <- alle_skud_clean
    if (input$liga_shot != "Alle") {
      df <- df %>% filter(Liga == input$liga_shot)
    }
    if (!is.null(input$team_shot) && input$team_shot != "Alle hold" && input$team_shot != "Ingen hold") {
      df <- df %>% filter(`team.name` == input$team_shot)
    }
    if (!is.null(input$match_shot) && input$match_shot != "Alle kampe" && input$match_shot != "Ingen kampe") {
      df <- df %>% filter(label == input$match_shot)
    }
    df
  })
  
  output$shot_plot <- renderPlotly({
    df <- filtered_shots_for_selected_match() %>%
      mutate(
        shotOutcome = case_when(
          shot.isGoal == TRUE ~ "Mål",
          shot.onTarget == TRUE ~ "På mål",
          TRUE ~ "Forsøg"
        ),
        hover_text = paste0("Spiller: ", player.name, "<br>",
                            "Hold: ", `team.name`, "<br>",
                            "BodyPart: ", shot.bodyPart)
      )
    p <- ggplot(df, aes(x = location.x, y = location.y)) +
      annotate_pitch(colour = "white", fill = "forestgreen", limits = FALSE) +
      theme_pitch() +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      geom_point(aes(color = shotOutcome, text = hover_text), size = 3, alpha = 0.8) +
      scale_color_manual(values = c("Forsøg" = "red", "På mål" = "blue", "Mål" = "limegreen")) +
      labs(color = "Skud fordelt på hold") +
      theme_minimal() +  
      theme(
        plot.background = element_rect(fill = "#2E3440"), 
        panel.background = element_rect(fill = "#2E3440"),  
        panel.grid.major = element_line(color = "#4C566A", size = 0.2),  
        panel.grid.minor = element_line(color = "#4C566A", size = 0.1),  
        text = element_text(color = "white"), 
        axis.text = element_text(color = "white"),  
        axis.title = element_text(color = "white"),  
        legend.background = element_rect(fill = "#3B4252", color = "#434C5E"),  
        legend.key = element_rect(fill = "#3B4252", color = "#434C5E"), 
        legend.text = element_text(color = "white"),  
        legend.title = element_text(color = "white")  
      )
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

