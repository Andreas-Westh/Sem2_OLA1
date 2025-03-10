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


clustering_vars <- allpasses[, c("pass.angle", "pass.length")]
# pass success


allpasses_scaled <- as.data.frame(scale(clustering_vars))

# correlations
passes_Corr <- cor(allpasses_scaled)
corrplot::corrplot(passes_Corr,addCoef.col = "black",method = "square",type = "lower")

allpasses_test <- allpasses %>% 
  group_by(player.name, matchId) %>%
  summarise(player_passes = n(), .groups = "drop") %>%  # Antal afleveringer pr. kamp
  group_by(player.name) %>%
  summarise(player_avgpass = mean(player_passes, na.rm = TRUE))  # Gennemsnit pr. spiller

allpasses <- allpasses %>%
  left_join(allpasses_test, by = "player.name")


df <- allpasses[, c("pass.angle","pass.length","player_avgpass")]
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
  theme_minimal()


#kmeans cluster
kmod <- kmeans(df_scaled, nstart = 10, centers = 4)
fviz_cluster(kmod, data = df_scaled)
df$cluster <- as.factor(kmod$cluster)

clusters <- df %>% group_by(cluster) %>%
  summarise(
    pass.angle = mean(pass.angle),
    pass.length = mean(pass.length),
    player_avgpass = mean(player_avgpass),
    count = n()
  )

# pca
data.pca <- princomp(df_sampled)
summary(data.pca)
data.pca$loadings[, 1:3]
fviz_pca_var(data.pca, col.var = "black")
