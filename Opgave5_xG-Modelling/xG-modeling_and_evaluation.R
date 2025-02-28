library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)

#### make for all, dutch and poland ####
#
#### SET VARIABLES HERE!!!! ####
#
x_variables <- c("shot_angle_geom", "shot_distance", 
                 "shot.bodyPart", "possession.duration", 
                 "player.position")

x_variables <- c("shot_angle_geom","shot_distance","shot.bodyPart","position_category")

# Add from counter

variables <- as.formula(paste("shot.isGoal ~", paste(x_variables, collapse = " + ")))

#### Splitting data #### 
set.seed(123) # for reproducablility
train_index <- createDataPartition(y = allshot_xG$shot.isGoal,
                                   # times = x
                                   p = 0.7,
                                   list = FALSE)# createDataPartition helps unbalanced datasets maintain a similar ratio of goals

train_data <- allshot_xG[train_index,]
test_data <- allshot_xG[-train_index,]


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
  glm_train <- glm(variables, 
                   data = train_data, 
                   family = "binomial")
  summary(glm_train)
  # multicollinearity
  vif(glm_train)

  # same validation as for tree
# correlation (maybe should be in data_exploration)
  # checking for multicollinearity

#tree model
  # make with training
  tree_model_train <- rpart(variables,
                            data = train_data,
                            method = "class",
                            control = rpart.control(maxdepth = 10,   # øg maks dybde
                                                    minsplit = 3,    # lavere min split
                                                    cp = 0.001))     # lavere kompleksitet
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
  ##### The best acc tree model can be found in xGmodelling line 420 #####
  # it is due to the tree model being trained on full data.
  
  
  ##### Loop 1 #####
  # Storage for the best model
  best_accuracy <- 0
  best_model <- NULL
  best_combination <- NULL
  
  # Iterate over all possible combinations of x_variables
  for (i in 1:length(x_variables)) {
    combinations <- combn(x_variables, i, simplify = FALSE)
    
    for (combo in combinations) {
      
      # Create the formula
      variables <- as.formula(paste("shot.isGoal ~", paste(combo, collapse = " + ")))
      
      # Train the model
      tree_model_train <- rpart(variables,
                                data = train_data,
                                method = "class",
                                control = rpart.control(minsplit = 4)) 
      
      # Predict on test data
      tree_test <- predict(tree_model_train, test_data, type = "class")
      
      # Calculate accuracy
      tree_confusion <- confusionMatrix(as.factor(tree_test), as.factor(test_data$shot.isGoal))
      tree_acc <- tree_confusion$overall['Accuracy']
      
      # Check if this model is the best
      if (tree_acc > best_accuracy) {
        best_accuracy <- tree_acc
        best_model <- tree_model_train
        best_combination <- combo
      }
    }
  }
  
  # Output the best results
  cat("Best Accuracy:", round(best_accuracy, 4), "\n")
  cat("Best Combination of Variables:", paste(best_combination, collapse = ", "), "\n")
  
  # Plot the best model
  rpart.plot(best_model, type = 2, extra = 104, box.palette = "BuGn")
  
  ##### Loop 2 #####
  # Lager til de bedste modeller og resultater
  best_accuracy <- 0
  best_model <- NULL
  best_combination <- NULL
  best_method <- NULL
  
  # Iterate over alle mulige kombinationer af x-variabler
  for (i in 1:length(x_variables)) {
    combinations <- combn(x_variables, i, simplify = FALSE)
    
    for (combo in combinations) {
      
      # Lav formel for modellen
      variables <- as.formula(paste("shot.isGoal ~", paste(combo, collapse = " + ")))
      
      ## 1. Beslutningstræ Model
      tree_model_train <- rpart(variables,
                                data = train_data,
                                method = "class",
                                control = rpart.control(minsplit = 4))
      
      tree_test <- predict(tree_model_train, test_data, type = "class")
      tree_confusion <- confusionMatrix(as.factor(tree_test), as.factor(test_data$shot.isGoal))
      tree_acc <- tree_confusion$overall['Accuracy']
      
      # Tjek om dette er den bedste model
      if (tree_acc > best_accuracy) {
        best_accuracy <- tree_acc
        best_model <- tree_model_train
        best_combination <- combo
        best_method <- "Decision Tree"
      }
      
      ## 2. Random Forest Model
      rf_model <- randomForest(variables,
                               data = train_data,
                               ntree = 100,        # antal træer
                               mtry = min(3, length(combo)), # antal variabler pr. split
                               importance = TRUE)
      
      rf_test <- predict(rf_model, test_data, type = "class")
      rf_confusion <- confusionMatrix(as.factor(rf_test), as.factor(test_data$shot.isGoal))
      rf_acc <- rf_confusion$overall['Accuracy']
      
      if (rf_acc > best_accuracy) {
        best_accuracy <- rf_acc
        best_model <- rf_model
        best_combination <- combo
        best_method <- "Random Forest"
      }
      
      ## 3. Gradient Boosting Model
      gbm_model <- gbm(variables,
                       data = train_data,
                       distribution = "bernoulli",
                       n.trees = 100,
                       interaction.depth = 3,
                       shrinkage = 0.1,
                       cv.folds = 5,
                       verbose = FALSE)
      
      gbm_pred <- predict(gbm_model, test_data, n.trees = 100, type = "response")
      gbm_class <- ifelse(gbm_pred > 0.5, TRUE, FALSE)
      gbm_confusion <- confusionMatrix(as.factor(gbm_class), as.factor(test_data$shot.isGoal))
      gbm_acc <- gbm_confusion$overall['Accuracy']
      
      if (gbm_acc > best_accuracy) {
        best_accuracy <- gbm_acc
        best_model <- gbm_model
        best_combination <- combo
        best_method <- "Gradient Boosting"
      }
    }
  }
  
  # Output af den bedste model
  cat("Best Accuracy:", round(best_accuracy, 4), "\n")
  cat("Best Combination of Variables:", paste(best_combination, collapse = ", "), "\n")
  cat("Best Method:", best_method, "\n")
  
  # Hvis den bedste model er et beslutningstræ, plotter vi det
  if (best_method == "Decision Tree") {
    rpart.plot(best_model, type = 2, extra = 104, box.palette = "BuGn")
  }
  
  
  
  
  tree_acc <- tree_confusion$overall['Accuracy']
  goal_summary$tree_acc <- tree_acc
  # check for Overfitting
          # calculate accuracy on training data
          train_pred <- predict(tree_model, train_data, type = "class")
          train_accuracy <- mean(train_pred == train_data$shot.isGoal)
          
          # calculate accuracy on test data
          test_pred <- predict(tree_model, test_data, type = "class")
          test_accuracy <- mean(test_pred == test_data$shot.isGoal)
          
          # display the results
          cat("Training Accuracy:", round(train_accuracy, 4), "\n")
          cat("Test Accuracy:", round(test_accuracy, 4), "\n")
          
          # calculate accuracy on training data
          train_pred <- predict(tree_model, train_data, type = "class")
          train_accuracy <- mean(train_pred == train_data$shot.isGoal)
          
          # calculate accuracy on test data
          test_pred <- predict(tree_model, test_data, type = "class")
          test_accuracy <- mean(test_pred == test_data$shot.isGoal)
          
          # display the results
          cat("Training Accuracy:", round(train_accuracy, 4), "\n")
          cat("Test Accuracy:", round(test_accuracy, 4), "\n")

  # ????Make more complex //// Boosting / Random forest  ??????
          library(randomForest)
          
          # train a random forest model with the same features
          rf_model <- randomForest(shot.isGoal ~ shot_angle + shot_distance, 
                                   data = train_data, 
                                   ntree = 100,        # number of trees
                                   mtry = 2,           # number of features to consider per split
                                   importance = TRUE)   # to view feature importance
          
          # predict on test data
          rf_pred <- predict(rf_model, test_data, type = "class")
          
          # evaluate accuracy
          rf_accuracy <- mean(rf_pred == test_data$shot.isGoal)
          print(rf_accuracy)
          
          # view feature importance
          importance(rf_model)
          
  #Heat maps

# evaluation
# Mean absolute error for our xG vs WyScout
          # abs is used, so that over/undershooting (direction) doesnt change the overall avg
          mae_tree <- mean(abs(allshot_xG$xG - allshot_xG$shot.isGoal))
          mae_original <- mean(abs(allshot_xG$shot.xg - allshot_xG$shot.isGoal))
          # see the mean absolute error between ours and wyscout
          print(mae_tree, 4)
          print(mae_original, 4)
          
          # residual sum of squares (rss)
          rss_tree <- sum((allshot_xG$xG - allshot_xG$shot.isGoal)^2)
          rss_original <- sum((allshot_xG$shot.xg - allshot_xG$shot.isGoal)^2)
          cat("Tree Model - RSS:", round(rss_tree, 4), "MSE:", round(mse_tree, 4), "\n")

                    # mean squared error (mse)
          mse_tree <- mean((allshot_xG$xG - allshot_xG$shot.isGoal)^2)
          mse_original <- mean((allshot_xG$shot.xg - allshot_xG$shot.isGoal)^2)
          cat("Original xG Model - RSS:", round(rss_original, 4), "MSE:", round(mse_original, 4), "\n")
          
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
# Roc curce?

             