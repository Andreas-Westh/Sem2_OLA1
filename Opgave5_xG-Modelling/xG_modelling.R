library(dplyr)
library(ggplot2)
library(ggsoccer)
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)


# Great video:
# https://youtu.be/VcdFOiBWsKM?si=YFJajpPF4IbolKr1



#### Opgave 5.1 - Opdeling i trænings- og testdata for skud ####
# Hent alle skud for den polske og hollandske liga og opdel skuddene i trænings- og testdata. 
# Forklar jeres fremgangsmåde. 
# (Hint: det er fx ikke sikkert en tilfældig 70/30 fordeling er den ideelle løsning)

##### TO DO #####
######Mongo call ######
######Clear id for polish and dutch ######
######Clear that it is only shots ######
      # filter by primary shot
######Which season ######
      # Season 2021/2022
######Dead ball situations with or not ######
######Amount of obs and so on ######
######ONLY DESCRIBE X-VARIABLES! ######



unique(allmatches$competitionId)
# May not be relevant, can just filter on label
allmatches <- allmatches %>%
  mutate(
    home_team = trimws(str_extract(label, "^[^-]+")),  # Extract and trim first team name
    away_team = trimws(str_extract(label, "(?<=– ).*?(?=,)")),  # Extract and trim second team name
    home_goals = as.numeric(str_extract(label, "(?<=, )\\d+(?=-)")),  # First digit after comma
    away_goals = as.numeric(str_extract(label, "(?<=-)\\d+"))  # Number after hyphen
  )

## Remove dead ball
# free_kick, corner, penalty
# throw_in gives problems
dead_balls <- c("free_kick","corner","penalty")
nrow(allshot_flat)
# 21069 shots
allshot_xG <- allshot_flat %>%
  filter(
    str_detect(type.secondary, "shot_after_") | 
      !str_detect(possession.types, paste(dead_balls, collapse = "|"))
  )
nrow(allshot_xG)
# 18419

dutch_id <- 635
polish_id <- 692

## only for season 2021/2022 
dutch_matches <- allmatches %>% filter(competitionId == dutch_id)
dutch_matches_2122 <- dutch_matches %>% filter(seasonId == 187502)
dutch_teams <- dutch_matches_2122$home_team
polish_matches <- allmatches %>% filter(competitionId == polish_id)
polish_matches_2122 <- polish_matches %>% filter(seasonId == 186215)
polish_teams <- polish_matches_2122$home_team


dutch_shot <- allshot_xG%>% filter(team.name %in% dutch_teams)
polish_shot <- allshot_xG %>% filter(team.name %in% polish_teams)





#### 5.2 ####

# X-variables
# Foot or head
# Shot position
# Possession type? 
# Source: https://dataglossary.wyscout.com/pitch_coordinates/
  # location x
  # location y
  # Possession end
  # Possession start?
    # Length to goal
# Sick pythagorean math shit
allshot_xG$shot_distance <- sqrt((100 - allshot_xG$possession.endLocation.x)^2 + 
                                   (50 - allshot_xG$possession.endLocation.y)^2)
hist(allshot_xG$shot_distance)
ggplot(allshot_xG, aes(x = shot_distance, y = as.numeric(shot.isGoal))) +
  geom_point(alpha = 0.3, color = "black") + # Points for each shot
  geom_smooth(method = "loess", color = "blue", fill = "lightgray") + # Smooth curve
  labs(
    title = "Shot Distance vs Goal Probability",
    x = "Shot Distance to Goal",
    y = "Goal (1) / No Goal (0)"
  ) +
  theme_minimal()
# The tail indicates outliers? 
    # Angle to goal
# with absolute values, 90 - -90
allshot_xG$shot_angle <- atan2(allshot_xG$possession.endLocation.y - 50, 
                                   100 - allshot_xG$possession.endLocation.x) * 180 / pi
# with absolute values, aka no negatives 90 - 0
allshot_xG$shot_angle_abs <- atan2(abs(allshot_xG$possession.endLocation.y - 50), 
                               abs(100 - allshot_xG$possession.endLocation.x)) * 180 / pi

# calculate the shot angle using the two goalposts and ensure it stays between 0 and 90 degrees
allshot_xG$angle <- abs(
  atan2(63 - allshot_xG$possession.endLocation.y, 100 - allshot_xG$possession.endLocation.x) - 
    atan2(37 - allshot_xG$possession.endLocation.y, 100 - allshot_xG$possession.endLocation.x)
)

# convert radians to degrees
allshot_xG$angle <- allshot_xG$shot_angle * 180 / pi

allshot_xG$angle <- pmin(allshot_xG$shot_angle, 90)





# grouped by
xG_group <- allshot_xG %>% group_by(shot.isGoal) %>% 
  summarise(
    avg_distance = mean(shot_distance),
    avg_angle = mean(shot_angle),
    avg_angle_abs = mean(shot_angle_abs)
  )

##### Visualize it #####
# Soruce: https://soccermatics.readthedocs.io/en/latest/gallery/lesson1/plot_PlottingShots.html

# More visible outliers
ggplot(allshot_xG) +
  annotate_pitch(colour = "white", fill = "green") +
  geom_bin2d(aes(x = possession.endLocation.x, y = possession.endLocation.y), 
             bins = 30) + # more or less detail
  theme_pitch() +
  scale_fill_gradient(low = "blue", high = "red") + # Color scale for heatmap
  labs(title = "Shot Locations Heatmap", x = "Pitch Length", y = "Pitch Width")

# General heatmap
ggplot(allshot_xG) +
  annotate_pitch(colour = "white", fill = "green") +
  geom_density_2d_filled(aes(x = possession.endLocation.x, y = possession.endLocation.y), 
                         alpha = 0.8) +
  theme_pitch() +
  labs(title = "Shot Locations Density Heatmap", x = "Pitch Length", y = "Pitch Width")
      ###### Evt make these for also actual goals #####
      ###### For this also make a shot to goal successrate, to be at the beginning of report ######







# Could be interesting to check out 
# https://wiscostret.wordpress.com/2019/04/22/shot-distance-xg-efficiency-and-player-style/
# https://www.datofutbol.cl/blog/xg-model/index.html













#### 5.3 ####
# https://marclamberts.medium.com/correlation-between-shooting-angles-and-expected-goals-xg-68db6f2e6045

# goal angle for goal width

glm_all_noab <- glm(shot.isGoal ~ shot_angle + shot_distance, data = allshot_xG)
summary(glm_all_noab)

glm_all_ab <- glm(shot.isGoal ~ shot_angle_abs + shot_distance, data = allshot_xG)
summary(glm_all_ab)
# shot_angle > shot_angle_abs
# Smæk flere ind, evt også direkte x og y

glm <- glm(shot.isGoal ~ shot_angle + shot_distance + shot.bodyPart, data = allshot_xG)
summary(glm)

# måske tjek cor
cor_df <- data.frame(allshot_xG$shot_distance, allshot_xG$shot_angle, allshot_xG$shot.isGoal)
cor <- cor(cor_df)
corrplot(cor, 
         method = "square",     # Farvede firkanter
         type = "lower",        # Kun nederste trekant
         diag = FALSE,          # Fjern diagonalen
         tl.col = "black",      # Labels i sort
         number.digits = 2,     # Antal decimaler
         addCoef.col = "black",) # Tilføj koefficienter i sort
# Weird result, multicollinearity?

# checking for multicollinearity
library(car)
vif(glm(shot.isGoal ~ shot_angle + shot_distance, data = allshot_xG))
# There is none, unsure about the corrplot issue


# Treeplot
# skaler data før modellering
allshot_xG$possession.duration <- as.numeric(allshot_xG$possession.duration)
tree_df <- allshot_xG %>%
  mutate(
    possession.duration = scale(possession.duration),
    shot_distance = scale(shot_distance),
    shot_angle = scale(shot_angle)
  )

tree_model <- rpart(shot.isGoal ~ shot_angle_geom + shot_distance + 
                      shot.bodyPart + possession.duration + 
                      possession.endLocation.x + possession.endLocation.y + 
                      player.position,
                         data = allshot_xG,
                         method = "class",
                         control = rpart.control(#maxdepth = 6,   # øg maks dybde
                                                 minsplit = 3,    # lavere min split
                                                 cp = 0.001)     # lavere kompleksitet
)

rpart.plot(tree_model, type = 2, extra = 104, box.palette = "BuGn")
# check importance
tree_model$variable.importance

##### Tree model plots #####
# general heatmap with decision boundary lines
ggplot(allshot_xG) +
  annotate_pitch(colour = "white", fill = "green") +
  
  # adjust the density heatmap
  geom_density_2d_filled(aes(x = possession.endLocation.x, y = possession.endLocation.y), 
                         alpha = 0.6, contour_var = "ndensity") +
  
  ## smaller and more transparent goal indicators
  #geom_point(data = subset(allshot_xG, shot.isGoal == TRUE),
  #           aes(x = possession.endLocation.x, y = possession.endLocation.y),
  #           color = "gray", size = 1.5, shape = 16, alpha = 0.6) +
  
  # hardcoded decision tree boundary lines
  geom_vline(xintercept = 91.6, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 94.8, color = "red", linetype = "dashed", size = 1) +
  
  theme_pitch() +
  labs(title = "Shot Locations with Decision Tree Boundaries and Goal Indicators",
       x = "Pitch Length", 
       y = "Pitch Width") 

# another plot but ugly af
ggplot(allshot_xG) +
  annotate_pitch(colour = "white", fill = "green") +
  
  # heatmap using xG values directly
  stat_summary_2d(aes(x = possession.endLocation.x, 
                      y = possession.endLocation.y, 
                      z = xG),
                  fun = mean, bins = 30) +
  
  # add decision tree boundary lines
  geom_vline(xintercept = 91.6, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 94.8, color = "red", linetype = "dashed", size = 1) +
  
  scale_fill_viridis_c(option = "plasma", name = "Average xG") +
  theme_pitch() +
  labs(title = "Shot Locations with xG-based Heatmap and Decision Tree Boundaries",
       x = "Pitch Length", 
       y = "Pitch Width")




##### Split data #####
set.seed(123) # for reproducablility
train_index <- createDataPartition(y = allshot_xG$shot.isGoal,
                                   # times = x
                                   p = 0.7,
                                   list = FALSE)# createDataPartition helps unbalanced datasets maintain a similar ratio of goals

train_data <- allshot_xG[train_index,]
test_data <- allshot_xG[-train_index,]


###### Checking out the split data ######
                table(train_data$shot.isGoal)
                table(test_data$shot.isGoal)
                
                prop.table(table(train_data$shot.isGoal))
                prop.table(table(test_data$shot.isGoal))
                # Very close 
                
                # combine training and test sets with a label
                train_data$dataset <- "Training"
                test_data$dataset <- "Test"
                combined_data <- rbind(train_data, test_data)
                
                # plot the distribution of shot.isGoal in both sets
                ggplot(combined_data, aes(x = shot.isGoal, fill = dataset)) +
                  geom_bar(position = "dodge") +
                  labs(title = "Distribution of Shot Outcomes in Training vs. Test Sets",
                       x = "Shot is Goal",
                       y = "Count") +
                  theme_minimal()
                
                # distance and angle in both datasets
                ggplot(combined_data, aes(x = shot_distance, y = shot_angle, color = dataset)) +
                  geom_point(alpha = 0.7) +
                  labs(title = "Shot Distance vs. Shot Angle by Dataset",
                       x = "Shot Distance",
                       y = "Shot Angle") +
                  theme_minimal()


##### Training #####
tree_model_train <- rpart(shot.isGoal ~ shot_angle + shot_distance + 
                            shot.bodyPart + possession.duration + 
                            possession.endLocation.x + possession.endLocation.y + 
                            player.position,
                          data = train_data,
                          method = "class")
rpart.plot(tree_model_train, type = 2, extra = 104, box.palette = "BuGn")

# train for glm
            glm_train <- glm(shot.isGoal ~ shot_angle + shot_distance + 
                               shot.bodyPart + possession.duration + 
                               possession.endLocation.x + possession.endLocation.y + 
                               player.position,
                             data = train_data,
                             family = "binomial")
            summary(glm_train)

##### Testing #####
tree_test <- predict(tree_model, test_data, type = "class")

glm_test <- predict(glm_train, test_data, type = "response")
glm_test_class <- ifelse(glm_test > 0.2, TRUE, FALSE)

##### Evaluating #####
goal_summary <- allshot_xG %>%
  summarise(
    Total_Shots = n(),
    Successful_Goals = sum(shot.isGoal),
    Goal_Ratio = round(Successful_Goals / Total_Shots, 4),
    baseline_acc = round(mean(allshot_xG$shot.isGoal == FALSE), 4)
  )
tree_confusion <- confusionMatrix(as.factor(tree_test), as.factor(test_data$shot.isGoal))
tree_confusion
# 90% acc
### MAKE SURE THAT THIS ACC IS BETTER THAN THE RATIO OF SUCC GOALS!!!! ###
tree_acc <- tree_confusion$overall['Accuracy']
goal_summary$tree_acc <- tree_acc

glm_confusion <- confusionMatrix(as.factor(glm_test_class), as.factor(test_data$shot.isGoal))
glm_confusion


allshot_xG$xG <- predict(tree_model, allshot_xG, type = "prob")[, "TRUE"]
  # the [, TRUE], makes it so we only get the TRUE column, not the false
# xG kan have mange af samme værdier, dette er grundet et simelt træ med få noder
printcp(tree_model) # tjek træets kompleksitet

# testing by forcing it to be more complex
                tree_model_komp <- rpart(shot.isGoal ~ shot_angle + shot_distance + 
                                           shot.bodyPart + possession.duration + 
                                           possession.endLocation.x + possession.endLocation.y + 
                                           player.position,
                                    data = allshot_xG,
                                    method = "class",
                                    control = rpart.control(maxdepth = 6,   # øg maks dybde
                                                            minsplit = 5,    # lavere min split
                                                            cp = 0.001))     # lavere kompleksitet
                
                rpart.plot(tree_model_komp, type = 2, extra = 104, box.palette = "BuGn")

 
allshot_xG$xG_diff <- allshot_xG$xG - allshot_xG$shot.xg

ggplot(allshot_xG, aes(x = xG_diff, fill = shot.isGoal)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7) +
  labs(title = "Distribution of xG Differences (Decision Tree xG - Original xG)",
       x = "xG Difference",
       y = "Count") +
  theme_minimal()
# ikke helt så optimistisk, som den eksiterende xg

# abs is used, so that over/undershooting (direction) doesnt change the overall avg
mae_tree <- mean(abs(allshot_xG$xG - allshot_xG$shot.isGoal))
mae_original <- mean(abs(allshot_xG$shot.xg - allshot_xG$shot.isGoal))
# see the mean absolute error between ours and wyscout
print(mae_tree, 4)
print(mae_original, 4)

# residual sum of squares (rss)
rss_tree <- sum((allshot_xG$xG - allshot_xG$shot.isGoal)^2)
rss_original <- sum((allshot_xG$shot.xg - allshot_xG$shot.isGoal)^2)

# mean squared error (mse)
mse_tree <- mean((allshot_xG$xG - allshot_xG$shot.isGoal)^2)
mse_original <- mean((allshot_xG$shot.xg - allshot_xG$shot.isGoal)^2)

cat("Original xG Model - RSS:", round(rss_original, 4), "MSE:", round(mse_original, 4), "\n")
cat("Tree Model - RSS:", round(rss_tree, 4), "MSE:", round(mse_tree, 4), "\n")
# nuværende variabler slår WyScout:
# shot.isGoal ~
#  shot_angle + shot_distance + 
#  shot.bodyPart + possession.duration + 
#  possession.endLocation.x + possession.endLocation.y + 
#  player.position,

#### Highest acc tree model ####
tree_model <- rpart(shot.isGoal ~ shot_angle_geom + shot_distance + 
                      shot.bodyPart + possession.duration + 
                      possession.endLocation.x + possession.endLocation.y + 
                      player.position,
                    data = allshot_xG,
                    method = "class",
                    control = rpart.control(#maxdepth = 6,   # øg maks dybde
                      minsplit = 3,    # lavere min split
                      cp = 0.001)     # lavere kompleksitet
)

tree_test <- predict(tree_model, test_data, type = "class")
tree_confusion <- confusionMatrix(as.factor(tree_test), as.factor(test_data$shot.isGoal))
tree_confusion





