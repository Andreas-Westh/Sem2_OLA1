# split data (maybe also should be in data_exploration) 
  # data exloration within the 2

# made for all, dutch and poland

# start with glm
  # make with training
  # same validation as for tree
# correlation (maybe should be in data_exploration)
  # checking for multicollinearity

#tree model
  # make with training
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
# confusion matrix
  # is it better than baseline
# Roc curce?