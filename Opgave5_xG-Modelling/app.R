library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(rpart.plot)
library(randomForest)
library(rpart)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "xG-Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("xG-Calculator", tabName = "grid_bane", selected = TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "grid_bane",
        h2("Dynamisk xG-Beregner, på baggrund af simplificeret RandomForest"),
        fluidRow(
          box(
            width = 8,
            title = "Af: Andreas W, Enes Y, Lukas C, Peter A, Sevim K.",
            plotlyOutput("grid_plot", height = "600px")
          ),
          
          # Kolonne med to bokse + et imageOutput
          column(
            width = 4,
            
            box(
              width = 12,
              title = "Klik-info",
              verbatimTextOutput("grid_info")
            ),
            
            box(
              width = 12,
              title = "Vælg kropsdel",
              selectInput(
                inputId = "bodypart",
                label   = "Kropsdel:",
                choices = c("right_foot", "left_foot", "head_or_other"),
                selected = "right_foot"
              ),
              
            )
          ),
          
          box(
            width = 12,
            title = "Skud-indikator",
            imageOutput("player_img", height = "auto")
          )
          
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rf_model <- readRDS(here::here("Opgave5_xG-Modelling", "rsconnect", "rf_model_simple.rds"))
  input_df <- reactiveVal(data.frame(shot.bodyPart = character(),
                                     shot_angle_geom = numeric(), shot_distance = numeric()))
  shot_distance <- reactiveVal(NA_real_)
  shot_angle_geom <- reactiveVal(NA_real_)
  
  # -------------------------------------------------
  # Parametre for målet
  # -------------------------------------------------
  goal_width    <- 11.43
  goal_center_y <- 50
  goal_x        <- 100
  
  # -------------------------------------------------
  # "Legetøjsmodel" for xG
  # -------------------------------------------------
  toy_xg_model <- function(x, y, bp) {
    dist_to_goal <- sqrt((goal_x - x)^2 + (goal_center_y - y)^2)
    sx <- abs(goal_x - x)
    sy <- abs(goal_center_y - y)
    angle_deg <- atan2(goal_width * sx, sx^2 + sy^2 - (goal_width/2)^2) * 180 / pi
    
    base_xg <- 0.1 + 0.3*(1 - dist_to_goal/50) + 0.1*(angle_deg/60)
    if (bp == "right_foot") {
      final_xg <- base_xg
    } else if (bp == "left_foot") {
      final_xg <- base_xg * 0.9
    } else {
      final_xg <- base_xg * 0.8
    }
    if (final_xg < 0) final_xg <- 0
    if (final_xg > 1) final_xg <- 1
    final_xg
  }
  
  # -------------------------------------------------
  # Lav coarse/fine grids
  # -------------------------------------------------
  x_seq_coarse <- seq(50, 95, by = 5)
  y_seq_coarse <- seq(0,  95, by = 5)
  
  df_coarse <- expand.grid(x_left = x_seq_coarse, y_left = y_seq_coarse) %>%
    mutate(
      x_right  = pmin(x_left + 5, 100),
      y_right  = pmin(y_left + 5, 100),
      center_x = (x_left + x_right)/2,
      center_y = (y_left + y_right)/2,
      cell_id  = paste0("coarse_(",
                        round((x_left + x_right)/2), ", ",
                        round((y_left + y_right)/2), ")"),
      cell_type = "coarse"
    )
  
  x_seq_fine <- seq(83, 100, by = 1)
  y_seq_fine <- seq(21, 78,  by = 1)
  
  df_fine <- expand.grid(x_left = x_seq_fine, y_left = y_seq_fine) %>%
    mutate(
      x_right  = pmin(x_left + 1, 100),
      y_right  = pmin(y_left + 1, 100),
      center_x = (x_left + x_right)/2,
      center_y = (y_left + y_right)/2,
      cell_id  = paste0("(", x_left, ", ", y_left, ")"),
      cell_type = "fine"
    )
  
  df_grid <- bind_rows(df_coarse, df_fine)
  
  # -------------------------------------------------
  # REAKTIVT data.frame TIL PRIK (kun 1)
  # -------------------------------------------------
  click_point <- reactiveVal(data.frame(x=numeric(), y=numeric()))
  
  # REAKTIVT xG => som seneste beregning
  current_xg <- reactiveVal(NA_real_)
  
  observeEvent(event_data("plotly_click"), {
    evt <- event_data("plotly_click")
    if (!is.null(evt)) {
      key_str <- as.character(unlist(evt$key)[1])
      
      coords_str <- sub("^coarse_\\(", "", key_str)
      coords_str <- gsub("[()]", "", coords_str)
      parts <- strsplit(coords_str, ",")[[1]]
      x_val <- as.numeric(trimws(parts[1]))
      y_val <- as.numeric(trimws(parts[2]))
      
      # Sæt prik
      click_point(data.frame(x=x_val, y=y_val))
      
      # Beregn distance
      dist_to_goal <- sqrt((goal_x - x_val)^2 + (goal_center_y - y_val)^2)
      
      # Beregn vinkel
      sx <- abs(goal_x - x_val)
      sy <- abs(goal_center_y - y_val)
      angle_deg <- atan2(goal_width * sx, sx^2 + sy^2 - (goal_width/2)^2) * 180 / pi
      
      
      # Beregn xG => gem i current_xg
      bp <- input$bodypart
      xg_val <- toy_xg_model(x_val, y_val, bp)
      current_xg(xg_val)
      
      input_df(data.frame(
        shot.bodyPart = bp,
        shot_angle_geom = angle_deg,
        shot_distance = dist_to_goal
      ))
    }
  })
  
  # -------------------------------------------------
  # Plot: Halv bane + prik
  # -------------------------------------------------
  output$grid_plot <- renderPlotly({
    df_click <- click_point()
    
    p <- ggplot(df_grid, aes(x = center_x, y = center_y)) +
      annotate_pitch(colour = "white", fill = "forestgreen", limits = FALSE) +
      theme_pitch() +
      coord_fixed(xlim = c(50, 100), ylim = c(0, 100)) +
      
      geom_tile(
        aes(
          width  = x_right - x_left,
          height = y_right - y_left,
          fill   = cell_type,
          key    = cell_id,
          text   = paste("Felt:", cell_id)
        ),
        alpha = 0.3
      ) +
      {
        if (nrow(df_click) > 0) {
          geom_point(data = df_click, aes(x = x, y = y),
                     color="red", size=3)
        } else {
          NULL
        }
      } +
      scale_fill_manual(values = c(coarse = "green", fine = "green")) +
      labs(
        title = "xG-Calculator2000",
        fill  = "Felt-type"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # -------------------------------------------------
  # Klik-info
  # -------------------------------------------------
  output$grid_info <- renderPrint({
    evt <- event_data("plotly_click")
    if (is.null(evt)) {
      cat("Klik på et felt for at se ID + distance + vinkel + xG.\n")
    } else {
      key_str <- as.character(unlist(evt$key)[1])
      cat("cell_id =", key_str, "\n")
      
      coords_str <- sub("^coarse_\\(", "", key_str)
      coords_str <- gsub("[()]", "", coords_str)
      parts <- strsplit(coords_str, ",")[[1]]
      x_val <- as.numeric(trimws(parts[1]))
      y_val <- as.numeric(trimws(parts[2]))
      
      cat(sprintf(" -> (x,y) = (%.1f, %.1f)\n", x_val, y_val))
      
      # Calculate distance and angle here
      dist_to_goal <- sqrt((goal_x - x_val)^2 + (goal_center_y - y_val)^2)
      sx <- abs(goal_x - x_val)
      sy <- abs(goal_center_y - y_val)
      angle_deg <- atan2(goal_width * sx, sx^2 + sy^2 - (goal_width/2)^2) * 180 / pi
      
      # Display the values
      cat(sprintf(" -> Distance til (%.0f,%.0f) = %.1f\n", goal_x, goal_center_y, dist_to_goal))
      cat(sprintf(" -> Skydevinkel (geom) = %.2f grader\n", angle_deg))
      
      # xG and other info
      bp <- input$bodypart
      xg_val <- predict(rf_model, input_df(), type = "prob")[, "TRUE"]
      cat(sprintf(" -> Kropsdel = %s\n", bp))
      cat(sprintf(" -> xG = %.3f\n", xg_val))
    }
  })
  
  # -------------------------------------------------
  # Vælg PNG (sort / rod / gul / gron) => imageOutput
  # -------------------------------------------------
  output$player_img <- renderImage({
    # Se, hvad xG er => Vælg billede
    xg_now <- current_xg()
    
    # Hvis intet klik => xg_now = NA => brug sort.png
    # Ellers:
    #   if xg < 0.1 => rod.png
    #   else if xg < 0.2 => gul.png
    #   else => gron.png
    
    imgfile <- "Opgave5_xG-Modelling/Spiller_indikator/sort.png"  # default
    
    if (!is.na(xg_now)) {
      if (xg_now < 0.1) {
        imgfile <- "Opgave5_xG-Modelling/Spiller_indikator/rod.png"
      } else if (xg_now < 0.2) {
        imgfile <- "Opgave5_xG-Modelling/Spiller_indikator//gul.png"
      } else {
        imgfile <- "Opgave5_xG-Modelling/Spiller_indikator//gron.png"
      }
    }
    
    # Returner stien i www + attributter
    list(
      src = file.path("Opgave5_xG-Modelling/Spiller_indikator/", imgfile),
      contentType = "image/png",
      alt = "Skud-indikator",
      width = "70%"  
    )
    
  }, deleteFile = FALSE)
}

shinyApp(ui, server)