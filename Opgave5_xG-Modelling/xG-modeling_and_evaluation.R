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
          
# confusion matrix
  # is it better than baseline
# Roc curce?