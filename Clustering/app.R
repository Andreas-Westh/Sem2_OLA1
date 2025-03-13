library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(rpart)
library(factoextra)

setwd("/Users/andreaswesth/Documents/R/Projects/GIT/Sem2_OLA1")

# Load data
allmatches <- readRDS("Shiny/allmatches.rds")
allshots <- readRDS("Shiny/allshots.rds")
allplayers <- readRDS("Shiny/allplayers.rds")
allpasses <- readRDS("Shiny/passesflat.rds")
allshots <- jsonlite::flatten(allshots)
allplayers <- jsonlite::flatten(allplayers)

# Data preparation
clustering_vars <- allpasses[, c("pass.angle", "pass.length")]
allpasses_scaled <- as.data.frame(scale(clustering_vars))

allpasses_test <- allpasses %>% 
  group_by(player.name, matchId) %>%
  summarise(player_passes = n(), .groups = "drop") %>% 
  group_by(player.name) %>%
  summarise(player_avgpass = mean(player_passes))

allpasses <- allpasses %>%
  left_join(allpasses_test, by = "player.name")

df <- allpasses[, c("pass.angle", "pass.length", "player_avgpass")]
df_scaled <- as.data.frame(scale(df))

# Sample for efficiency
set.seed(1970)
sample_size <- 50000
sample_indices <- sample(1:nrow(df_scaled), sample_size)
df_sampled <- df_scaled[sample_indices, ]

# Precompute elbow plot data
dftwss <- data.frame(k = 1:20, twss = NA)
for (i in 1:20) {
  tmod <- kmeans(df_sampled, centers = i, nstart = 10, iter.max = 500)
  dftwss[i, 'twss'] <- tmod$tot.withinss
}

# Precompute PCA on sampled data
data.pca <- princomp(df_sampled)
pca_scores <- as.data.frame(data.pca$scores[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Football Pass Clustering App"),
  dashboardSidebar(
    sliderInput("k", "Number of Clusters:", min = 1, max = 20, value = 4, step = 1)
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Elbow Plot", status = "primary", solidHeader = TRUE,
        plotOutput("elbow_plot"), width = 6
      ),
      box(
        title = "Players per Main Cluster", status = "primary", solidHeader = TRUE,
        plotOutput("bar_plot"), width = 6
      )
    ),
    fluidRow(
      box(
        title = "Custom 3D Plot", status = "primary", solidHeader = TRUE,
        selectInput("x_var", "X Variable:", 
                    choices = c("matches_played", "total_passes", "avg_passes_per_match", 
                                "avg_pass_length", "sd_pass_lenght", "avg_pass_angle", 
                                "sd_pass_angle", "pass_acc"), selected = "total_passes"),
        selectInput("y_var", "Y Variable:", 
                    choices = c("matches_played", "total_passes", "avg_passes_per_match", 
                                "avg_pass_length", "sd_pass_lenght", "avg_pass_angle", 
                                "sd_pass_angle", "pass_acc"), selected = "avg_pass_length"),
        selectInput("z_var", "Z Variable:", 
                    choices = c("matches_played", "total_passes", "avg_passes_per_match", 
                                "avg_pass_length", "sd_pass_lenght", "avg_pass_angle", 
                                "sd_pass_angle", "pass_acc"), selected = "avg_pass_angle"),
        plotlyOutput("plot_3d"), width = 6
      ),
      box(
        title = "PCA Plot", status = "primary", solidHeader = TRUE,
        plotOutput("pca_plot"), width = 6
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive k-means clustering
  kmod <- reactive({
    kmeans(df_scaled, centers = input$k, nstart = 10, iter.max = 500)
  })
  
  # Reactive cluster labels
  cluster_labels_df_scaled <- reactive({
    kmod()$cluster
  })
  
  # Update allpasses with cluster labels
  allpasses_reactive <- reactive({
    allpasses$cluster <- as.factor(cluster_labels_df_scaled())
    allpasses
  })
  
  # Player cluster counts
  player_cluster_counts <- reactive({
    allpasses_reactive() %>%
      group_by(player.name, cluster) %>%
      summarise(count = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = cluster, values_from = count, 
                         names_prefix = "cluster_", values_fill = 0)
  })
  
  # Base player stats
  base_player_stats <- reactive({
    allpasses_reactive() %>%
      group_by(player.name) %>%
      filter(n() > 100) %>% 
      summarise(
        matches_played = n_distinct(matchId),
        total_passes = n(),
        avg_passes_per_match = total_passes / matches_played,
        avg_pass_length = mean(pass.length),
        sd_pass_lenght = sd(pass.length),
        avg_pass_angle = mean(pass.angle),
        sd_pass_angle = sd(pass.angle),
        pass_acc = (sum(pass.accurate == TRUE) / total_passes) * 100
      )
  })
  
  # Full player stats with main cluster
  player_stats <- reactive({
    base_stats <- base_player_stats()
    cluster_counts <- player_cluster_counts()
    full_stats <- base_stats %>%
      left_join(cluster_counts, by = "player.name") %>%
      rowwise() %>%
      mutate(main_cluster = which.max(c_across(starts_with("cluster_")))) %>%
      ungroup() %>%
      mutate(
        main_cluster = factor(main_cluster, levels = 1:input$k),
        cluster_counts_str = {
          counts <- c_across(starts_with("cluster_"))
          paste("Cluster", 1:input$k, ":", counts, collapse = ", ")
        }
      )
    full_stats
  })
  
  # Elbow plot (static)
  output$elbow_plot <- renderPlot({
    ggplot(dftwss, aes(x = k, y = twss)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Elbow Method for Optimal K",
           x = "Number of Clusters (K)",
           y = "Total Within-Cluster Sum of Squares") +
      theme_minimal()
  })
  
  # Bar plot
  output$bar_plot <- renderPlot({
    ggplot(player_stats(), aes(x = main_cluster)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Number of Players per Main Cluster",
           x = "Main Cluster",
           y = "Number of Players") +
      theme_minimal()
  })
  
  # Custom 3D plot
  output$plot_3d <- renderPlotly({
    plot_ly(
      data = player_stats(),
      x = ~get(input$x_var),
      y = ~get(input$y_var),
      z = ~get(input$z_var),
      type = "scatter3d",
      mode = "markers",
      color = ~main_cluster,
      text = ~paste0(
        "Player: ", player.name, "<br>",
        "Main cluster: ", main_cluster, "<br>",
        cluster_counts_str, "<br>",
        input$x_var, ": ", round(get(input$x_var), 1), "<br>",
        input$y_var, ": ", round(get(input$y_var), 1), "<br>",
        input$z_var, ": ", round(get(input$z_var), 1)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = input$x_var),
          yaxis = list(title = input$y_var),
          zaxis = list(title = input$z_var)
        )
      )
  })
  
  # PCA plot
  output$pca_plot <- renderPlot({
    cluster_labels_df_sampled <- cluster_labels_df_scaled()[sample_indices]
    pca_data <- pca_scores
    pca_data$cluster <- as.factor(cluster_labels_df_sampled)
    ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(alpha = 0.5) +
      labs(title = "PCA of Pass Data by Cluster",
           x = "Principal Component 1",
           y = "Principal Component 2") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)