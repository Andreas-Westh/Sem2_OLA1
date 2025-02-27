library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
#
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SUPER DUPER SEJ xG CALCULATOR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("xG-Calculator2000", tabName = "grid_bane", selected = TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "grid_bane",
        h2("xG på banen ud fra distance fra mål, vinkel, kropsdel og ny xG-MODEL2000"),
        fluidRow(
          box(
            width = 8,
            title = "xG-MODEL2000 simulator",
            plotlyOutput("grid_plot", height = "600px")
          ),
          
          # Kolonne med to bokse under hinanden (4 bred)
          column(width = 4,
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
                   )
                 )
          )
        )
        # Ingen debug-boks længere
      )
    )
  )
)

server <- function(input, output, session) {
  
  # -----------------------------
  # Parametre for målet
  # -----------------------------
  goal_width    <- 7.32
  goal_center_y <- 50
  goal_x        <- 100
  
  # -----------------------------
  # "Legetøjsmodel" for xG (valgfrit)
  # -----------------------------
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
  
  # -----------------------------
  # LAV COARSE OMRÅDE (5x5) + FINE OMRÅDE (1x1)
  # -----------------------------
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
  
  # -----------------------------
  # (A) REAKTIVT data.frame TIL PRIK - men kun ÉN prik
  # -----------------------------
  # Gemmer KUN seneste klik => overskriver
  click_point <- reactiveVal(data.frame(x=numeric(), y=numeric()))
  
  # Når man klikker, parse x_val, y_val => sæt som EEN prik
  observeEvent(event_data("plotly_click"), {
    evt <- event_data("plotly_click")
    if (!is.null(evt)) {
      key_str <- as.character(unlist(evt$key)[1])
      
      coords_str <- sub("^coarse_\\(", "", key_str)
      coords_str <- gsub("[()]", "", coords_str)
      parts <- strsplit(coords_str, ",")[[1]]
      x_val <- as.numeric(trimws(parts[1]))
      y_val <- as.numeric(trimws(parts[2]))
      
      # Overskriv den reaktive DF, så vi kun har 1 row
      click_point(data.frame(x=x_val, y=y_val))
    }
  })
  
  # -----------------------------
  # (B) Plot: Halv bane + ÉN prik
  # -----------------------------
  output$grid_plot <- renderPlotly({
    df_click <- click_point()  # Henter (x, y)
    
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
      # PRIK - kun hvis DF ikke er tom
      {
        if (nrow(df_click) > 0) {
          geom_point(data = df_click, 
                     aes(x = x, y = y),
                     color = "red", size=3)
        } else {
          NULL
        }
      } +
      scale_fill_manual(values = c(coarse = "red", fine = "blue")) +
      labs(
        title = "xG-Calculator2000",
        fill  = "Felt-type"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # -----------------------------
  # (C) Klik-info (distance, vinkel, xG)
  # -----------------------------
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
      
      # Dist
      dist_to_goal <- sqrt((goal_x - x_val)^2 + (goal_center_y - y_val)^2)
      cat(sprintf(" -> Distance til (%.0f,%.0f) = %.1f\n", goal_x, goal_center_y, dist_to_goal))
      
      # Vinkel
      sx <- abs(goal_x - x_val)
      sy <- abs(goal_center_y - y_val)
      angle_deg <- atan2(goal_width * sx, sx^2 + sy^2 - (goal_width/2)^2) * 180 / pi
      cat(sprintf(" -> Skydevinkel (geom) = %.2f grader\n", angle_deg))
      
      # Evt. xG
      bp <- input$bodypart
      xg_val <- toy_xg_model(x_val, y_val, bp)
      cat(sprintf(" -> Kropsdel = %s\n", bp))
      cat(sprintf(" -> xG (toy-model) = %.3f\n", xg_val))
    }
  })
}

shinyApp(ui, server)