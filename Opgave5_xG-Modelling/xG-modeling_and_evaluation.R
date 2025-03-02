library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(rpart)
library(caret)
library(ggplot2)
library(ggplot2)
library(ggsoccer)

# Make this for polish, dutch and total!!!!!




#### make for all, dutch and poland ####
#
#### SET VARIABLES HERE!!!! ####
#
x_variables <- c("shot_angle_geom", 
                 "shot.bodyPart", 
                 "possession.duration", 
                 "shot_distance"
                 )

# Add from counter

variables <- as.formula(paste("shot.isGoal ~", paste(x_variables, collapse = " + ")))
allshot_xG$shot.isGoal <- as.factor(allshot_xG$shot.isGoal) 


#### Splitting data #### 
set.seed(123) # for reproducablility
train_index <- createDataPartition(y = allshot_xG$shot.isGoal,
                                   # times = x
                                   p = 0.7,
                                   list = FALSE)# createDataPartition helps unbalanced datasets maintain a similar ratio of goals

train_data <- allshot_xG[train_index,]
test_data <- allshot_xG[-train_index,]

train_data <- allshot_2122
test_data <- allshot_2223

##### Checking out the split data #####
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


#### GLM ####
  # make with training
glm_result <- data.frame(x_variable = character(), p_value = numeric(), p_stars = character())

get_significance_stars <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else if (p < 0.1) {
    return(".")
  } else {
    return("")
  }
}

for (i in x_variables) {
  #Sys.sleep(0.5)
  formula_glm <- as.formula(paste("shot.isGoal ~", i))
  glm_model <- glm(formula_glm, data = train_data, family = "binomial")
  glm_pval <- summary(glm_model)$coefficients[2,4]
  glm_stars <- get_significance_stars(glm_pval)
  
  tmp_glm <- data.frame(x_variable = i, p_value = as.numeric(glm_pval), p_stars = glm_stars)
  glm_result <- rbind(glm_result, tmp_glm)
}

  glm_train <- glm(variables, 
                   data = train_data, 
                   family = "binomial")
  summary(glm_train)
  # multicollinearity
  vif(glm_train)
  
#### Random Forest ####
  rf_model <- randomForest(variables, 
                           data = train_data,
                           ntree = 10000,       # Number of trees (adjust for speed/performance)
                           mtry = floor(sqrt(length(x_variables))),  # Number of variables per split
                           importance = TRUE)
  
  # Predict on test set
  rf_test <- predict(rf_model, test_data, type = "class")
  
  # Evaluate accuracy
  rf_confusion <- confusionMatrix(as.factor(rf_test), as.factor(test_data$shot.isGoal))
  rf_confusion
  
  allshot_xG$xG <- predict(rf_model, allshot_xG, type = "prob")[, "TRUE"]
  
  mse_tree <- mean((allshot_xG$xG - allshot_xG$shot.isGoal)^2)
  mse_original <- mean((allshot_xG$shot.xg - allshot_xG$shot.isGoal)^2)
  rss_tree <- sum((allshot_xG$xG - allshot_xG$shot.isGoal)^2)
  rss_original <- sum((allshot_xG$shot.xg - allshot_xG$shot.isGoal)^2)
  cat("WyScout xG Model - RSS:", round(rss_original, 4), "MSE:", round(mse_original, 4), "\n")
  cat("Random Forest Model - RSS:", round(rss_tree, 4), "MSE:", round(mse_tree, 4), "\n")
  RMSE <- sqrt(mse_tree)
  # Feature importance
  importance(rf_model)
  varImpPlot(rf_model)
  
  #### find the best depth for a singular tree-model ####
  depth_range <- 10
  mse_train <- numeric(depth_range)
  mse_test <- numeric(depth_range)
  mse_cv <- numeric(depth_range)
  
  # create amount of folds for cv
  set.seed(123)
  folds <- createFolds(train_data$shot.isGoal, k = 20, list = TRUE)
  
  # loop for the different tree lenghts
  for (i in 1:depth_range) {
    tree_loop <- rpart(variables,
                       data = train_data,
                       method = "class",
                       control = rpart.control(maxdepth = i, cp = 0))  # Fix maxdepth
    
    # predicts
    loop_train <- predict(tree_loop, newdata = train_data, type = "prob")[, "TRUE"]
    loop_test <- predict(tree_loop, newdata = test_data, type = "prob")[, "TRUE"]
    
    # make shot.isGoal numeric
    goal_numeric_train <- as.numeric(train_data$shot.isGoal) - 1
    goal_numeric_test <- as.numeric(test_data$shot.isGoal) - 1
    
    # MSE 
    mse_train[i] <- mean((loop_train - goal_numeric_train)^2)
    mse_test[i] <- mean((loop_test - goal_numeric_test)^2)
    
    #Cross-Validation
    mse_folds <- numeric(20)
    for (f in 1:20) {
      train_idx <- setdiff(seq_len(nrow(train_data)), folds[[f]])
      val_idx   <- folds[[f]]
      
      # Train on training fold
      tree_cv <- rpart(variables,
                       data = train_data[train_idx, ],
                       method = "class",
                       control = rpart.control(maxdepth = i, cp = 0))
      
      # Predict probabilities on validation fold
      loop_val <- predict(tree_cv, newdata = train_data[val_idx, ], type = "prob")[, "TRUE"]
      goal_numeric_val <- as.numeric(train_data$shot.isGoal[val_idx]) - 1
      
      # Compute MSE for this fold
      mse_folds[f] <- mean((loop_val - goal_numeric_val)^2)
    }
    
    # Average cross-validation MSE
    mse_cv[i] <- mean(mse_folds)
  }
  
  # Store results in a dataframe for easy plotting
  depth_results <- data.frame(
    maxdepth = rep(1:depth_range, 3),
    MSE = c(mse_train, mse_test, mse_cv),
    Type = rep(c("Training", "Test", "Cross-Validation"), each = depth_range)
  )

  ggplot(depth_results, aes(x = maxdepth, y = MSE, color = Type)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:depth_range) +  # Ensures x-axis has breaks at every integer
    labs(title = "A tree depth of 6 is the most optimal within Cross-Validation",
         x = "Tree Depth",
         y = "MSE") +
    theme_minimal()
  
  ##### evt try LOOCV #####
  ##### Try boosting via gbm or entropi #####
  ##### Roc curve
#### tree model ####
  # make with training
  tree_model_train <- rpart(variables,
                            data = train_data,
                            method = "class",
                            control = rpart.control(maxdepth = 6,   # øg maks dybde
                                                    minsplit = 6,    # lavere min split
                                                    cp = 0))     # lavere kompleksitet
  rpart.plot(tree_model_train, type = 2, extra = 104, box.palette = "BuGn")
  tree_model_train$variable.importance
  
  tree_test <- predict(tree_model_train, test_data, type = "class")
  goal_summary <- allshot_xG %>%
    summarise(
      Total_Shots = n(),
      Successful_Goals = sum(shot.isGoal),
      Goal_Ratio = round(Successful_Goals / Total_Shots, 4),
      baseline_acc = round(mean(allshot_xG$shot.isGoal == FALSE), 4)
    )
  tree_confusion <- confusionMatrix(as.factor(tree_test), as.factor(test_data$shot.isGoal))
  tree_confusion
  allshot_xG$xG_simple <- predict(tree_model_train, allshot_xG, type = "prob")[, "TRUE"]
  mse_tree_simple <- mean((allshot_xG$xG_simple - allshot_xG$shot.isGoal)^2)
  rss_tree_simple <- sum((allshot_xG$xG_simple - allshot_xG$shot.isGoal)^2)
  cat("Simple Tree Model - RSS:", round(rss_tree_simple, 4), "MSE:", round(mse_tree_simple, 4), "\n")
  
  
  
  ## Boosting?

          
            ###### RSS/MSE NOTES ######
          # WyScout scores better in all
          #lower rss/mse indicates a better model fit.
          #if the tree model's mse is lower than the original xg model, it means the tree model's predictions are closer to actual outcomes on average, with less severe errors.
          
          #use mse if you want to penalize large errors more (e.g., when big mispredictions are costly).
          #use rss if you are comparing models on the same dataset, as it emphasizes total error.
          #stick with mae if you want to avoid outlier influence, keeping all errors equally weighted.
          
          #if your xG model occasionally makes big mistakes, mse will highlight this more clearly than mae.
          #if your model predictions are generally close but with a few big misses, comparing mae vs. mse can reveal this behavior.
          #
# confusion matrix
  # is it better than baseline
# Roc curve:
  library(pROC)
  pred_probs <- predict(rf_model, test_data, type = "prob")[, "TRUE"] 
  roc_curve <- roc(test_data$shot.isGoal, pred_probs)
  plot(roc_curve, col = "blue", main = "ROC Curve for RF Model")
  auc(roc_curve)
  
  #### Plots with xG ####
  # Squares
  ggplot(allshot_xG, aes(x = possession.endLocation.x, y = possession.endLocation.y)) +
    annotate_pitch(fill = "grey20", colour = "white") +  
    stat_summary_2d(aes(z = xG), fun = mean, bins = 60) +  
    scale_fill_viridis_c(option = "inferno", name = "Mean xG") + 
    theme_minimal() +
    labs(title = "Highest xG cluster close to the goal",
         x = "Pitch Length (%)",
         y = "Pitch Width (%)") +
    coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) 
  
  # More fluid?
  
  
  #### Save RDS for Shiny ####
  saveRDS(allshot_xG,"allshot_xG.rds")
  saveRDS(rf_model,"rf_model_simple.rds")  
  