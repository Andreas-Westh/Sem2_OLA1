library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(rpart)
library(factoextra)

allmatches <- readRDS("Shiny/allmatches.rds")
allshots <- readRDS("Shiny/allshots.rds")
allplayers <- readRDS("Shiny/allplayers.rds")
allpasses <- readRDS("Shiny/passesflat.rds")
allshots <- jsonlite::flatten(allshots)
allplayers <- jsonlite::flatten(allplayers)


clustering_vars <- allpasses[, c("pass.angle", "pass.length","location.x","location.y")]
# pass success


allpasses_scaled <- as.data.frame(scale(clustering_vars))

# correlations
passes_Corr <- cor(allpasses_scaled)
corrplot::corrplot(passes_Corr,addCoef.col = "black",method = "square",type = "lower")

allpasses_test <- allpasses %>% 
  group_by(player.name, matchId) %>%
  summarise(player_passes = n(),
            .groups = "drop") %>%  # Antal afleveringer pr. kamp
  group_by(player.name) %>%
  summarise(player_avgpass = mean(player_passes))  # Gennemsnit pr. spiller

allpasses <- allpasses %>%
  left_join(allpasses_test, by = "player.name")

# evt add the player avg passes, if cluster is unclean
df <- allpasses[, c("pass.angle","pass.length","location.x","location.y","player_avgpass")]
df_scaled <- as.data.frame(scale(df))

set.seed(1970)
sample_size <- 50000
df_sampled <- df_scaled[sample(1:nrow(df_scaled), sample_size), ]

# Kmeans
for (i in 1:20) {
  tmod <- kmeans(df_sampled, centers = i, nstart = 10, iter.max = 500)
  dftwss[i, 'twss'] <- tmod$tot.withinss
}
# elbow
plot(dftwss)

ggplot(dftwss, aes(x = k, y = twss)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  theme_minimal()


#kmeans cluster
kmod <- kmeans(df_scaled, nstart = 10, centers = 5)
fviz_cluster(kmod, data = df_scaled)
df$cluster <- as.factor(kmod$cluster)

clusters <- df %>% group_by(cluster) %>%
  summarise(
    pass.angle = mean(pass.angle),
    pass.length = mean(pass.length),
    player_avgpass = mean(player_avgpass),
    player_avg_y = mean(location.y),
    player_avg_x = mean(location.x),
    count = n()
  )

allpasses$cluster <- as.factor(kmod$cluster)
player_cluster <- allpasses[,c("team.id","team.name","player.position")]
player_cluster <- allpasses %>% group_by(player.name)


# pca
data.pca <- princomp(df_sampled)
summary(data.pca)
data.pca$loadings[, 1:3]
fviz_pca_var(data.pca, col.var = "black")



# opsummering i spiler statestik
player_stats <- allpasses %>%
  group_by(player.name) %>%
  filter(n() > 100) %>% 
  summarise(
    matches_played = n_distinct(matchId),    
    total_passes = n(),                      
    avg_passes_per_match = total_passes / matches_played, 
    # giver fejl: sd_passes_per_match = sd(player_passes_per_match$total_passes_per_match),
    avg_pass_length = mean(pass.length),
    sd_pass_lenght = sd(pass.length),
    avg_pass_angle = mean(pass.angle),
    sd_pass_angle = sd(pass.angle),
    avg_y = mean(location.y),
    avg_x = mean(location.x),
    pass_acc = (sum(pass.accurate == TRUE) / total_passes) * 100,
    cluster_1 = sum(cluster == 1),
    cluster_2 = sum(cluster == 2), 
    cluster_3 = sum(cluster == 3),
    cluster_4 = sum(cluster == 4),
    cluster_5 = sum(cluster == 5)
  )

# finde en spillers main cluster
player_stats <- player_stats %>%
  mutate(main_cluster = max.col(across(starts_with("cluster_"))))

# Måske også finde spillerens primære position? 


#### Loop heatmap pass location ####
for (k in 1:5) {
  ggplot(allpasses %>% filter(cluster == k)) +
    annotate_pitch(colour = "white", fill = "gray") +  
    stat_density_2d_filled(aes(x = location.x, y = location.y), 
                           alpha = 0.7, contour_var = "ndensity") +  
    theme_pitch() +
    scale_fill_viridis_d(option = "magma") +  
    labs(title = paste("Passes Positions Heatmap - Cluster", k),
         x = "Pitch Length", y = "Pitch Width") +
    theme(legend.position = "right") -> p
  
  print(p)
}

for (k in 1:5) {
  ggplot(allpasses %>% filter(cluster == k)) +
    annotate_pitch(colour = "white", fill = "gray") +  
    stat_density_2d_filled(aes(x = possession.endLocation.x, y = possession.endLocation.y), 
                           alpha = 0.7, contour_var = "ndensity") +  
    theme_pitch() +
    scale_fill_viridis_d(option = "magma") +  
    labs(title = paste("Passes End Positions Heatmap - Cluster", k),
         x = "Pitch Length", y = "Pitch Width") +
    theme(legend.position = "right") -> p_end
  
  print(p_end)
}


#### General plots ####
allplayers_roles <- allplayers[,c("shortName","role.name")]
allplayers_roles <- allplayers_roles %>% rename(player.name = shortName)
  player_stats <- player_stats %>%
    left_join(allplayers_roles, by = "player.name")
  
  # number of player per role
  ggplot(player_stats, aes(x = as.factor(role.name), fill = role.name)) +
    geom_bar() +
    labs(title = "number in every role",
         x = "Main Cluster",
         y = "Number of Players") +
    theme_minimal()
  
  # roles in each cluster
  ggplot(player_stats, aes(x = as.factor(main_cluster), fill = role.name)) +
    geom_bar() +
    labs(title = "Number of Players per Main Cluster",
         x = "Main Cluster",
         y = "Number of Players") +
    theme_minimal()
  

plot_ly(
  data = player_stats,
  x=~avg_x,y=~avg_y,z=~main_cluster,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",player.name,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, ", Cluster 2: ", cluster_2, ", Cluster 3: ",cluster_3, ", Cluster 4: ", cluster_4,"<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_pass_length,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>"
  ),
  hoverinfo="text")

plot_ly(
  data = player_stats,
  x=~total_passes,y=~avg_pass_length,z=~avg_pass_angle,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",player.name,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, ", Cluster 2: ", cluster_2, ", Cluster 3: ",cluster_3, ", Cluster 4: ", cluster_4,"<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_pass_length,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>"
  ),
  hoverinfo="text")

plot_ly(
  data = player_stats,
  x=~total_passes,y=~sd_pass_lenght,z=~avg_pass_angle,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",player.name,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, ", Cluster 2: ", cluster_2, ", Cluster 3: ",cluster_3, ", Cluster 4: ", cluster_4,"<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_pass_length,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>"
  ),
  hoverinfo="text")
