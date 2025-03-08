library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(rpart)

allmatches <- readRDS("Shiny/allmatches.rds")
allshots <- readRDS("Shiny/allshots.rds")
allplayers <- readRDS("Shiny/allplayers.rds")
allpasses <- readRDS("Shiny/passesflat.rds")
allshots <- jsonlite::flatten(allshots)
allplayers <- jsonlite::flatten(allplayers)


clustering_vars <- allpasses[, c("pass.angle", "pass.length")]
# pass success


allpasses_scaled <- as.data.frame(scale(clustering_vars))


passes_Corr <- cor(allpasses_scaled)
corrplot::corrplot(passes_Corr,addCoef.col = "black",method = "square",type = "lower")




# correlations


# kmeans


# elbow


