library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(ggplot2)
library(ggsoccer)
library(mltools)
library(ranger)


# Make this for polish, dutch and total!!!!!
# TEST ALL FOR WITHOUT DURATION



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
cv_folds <- trainControl(method = "cv", number = 5)
train_index <- createDataPartition(y = allshot_xG$shot.isGoal,
                                   # times = x
                                   p = 0.8,
                                   list = FALSE)# createDataPartition helps unbalanced datasets maintain a similar ratio of goals

train_data <- allshot_xG[train_index,]
test_data <- allshot_xG[-train_index,]

# for seasons instead
#train_data <- allshot_2122
#test_data <- allshot_2223

##### Checking out the split data #####
table(train_data$shot.isGoal)
table(test_data$shot.isGoal)

prop.table(table(train_data$shot.isGoal))
prop.table(table(test_data$shot.isGoal))
# Very close 

##### Pltos #####
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
# Definér tuning gitteret kun med mtry
rf_grid <- expand.grid(mtry = c(2, 4, 6, 8))

# Definér kontrolparametre for træningen
train_control <- trainControl(method = "cv", number = 5)

# Træn Random Forest-modellen med cross-validation
rf_tune <- train(variables, data = train_data, method = "rf",
                  trControl = train_control, 
                  tuneGrid = rf_grid,
                  ntree = 1000)  # Angiv ntree separat


  # Udskriv den bedste model baseret på CV
  print(rf_tune$bestTune)
  

  #### ntree loop ####
  ntree_values <- c(500, 1000, 5000, 10000)
  mtry_best <- as.numeric(rf_tune$bestTune$mtry)
  
  results <- data.frame(ntree = ntree_values, AUC = NA)
  
  for (i in seq_along(ntree_values)) {
    rf_temp <- randomForest(variables, data = train_data, ntree = ntree_values[i], mtry = mtry_best)
    
    pred_probs <- predict(rf_temp, test_data, type = "prob")[, "TRUE"]
    auc_score <- auc(roc(test_data$shot.isGoal, pred_probs))
    
    results$AUC[i] <- auc_score
  }
  
  print(results)
  
  rf_model <- randomForest(variables, 
                           data = train_data,
                           ntree = results$ntree[1],      
                           mtry = mtry_best, 
                           importance = TRUE)
  varImpPlot(rf_model)
  # Forudsig sandsynligheder fra modellen
  rf_test <- predict(rf_model, test_data, type = "prob")[, "TRUE"]
  
  #### best threshold loop ####
  # Mulige thresholds at teste
  thresholds <- seq(0.1, 1.0, by = 0.01)
  
  # Gem Accuracy for hver threshold
  threshold_results <- data.frame(Threshold = thresholds, Accuracy = NA)
  
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    
    # Konverter sandsynligheder til klasser
    pred_class <- ifelse(rf_test > t, "TRUE", "FALSE")
    
    # Beregn Accuracy
    acc_score <- mean(pred_class == test_data$shot.isGoal)
    
    # Gem resultater
    threshold_results$Accuracy[i] <- round(acc_score, 5)
    best_threshold <- threshold_results$Threshold[which.max(threshold_results$Accuracy)]
  }
  # 0.79
  
  rf_preds <- ifelse(rf_test > 0.79, "TRUE", "FALSE")
  
  #### Evaluate accuracy af ####
  rf_confusion <- confusionMatrix(as.factor(rf_preds), as.factor(test_data$shot.isGoal))
  rf_confusion
  
  allshot_xG$xG <- predict(rf_model, allshot_xG, type = "prob")[, "TRUE"]
  rf_xG_preds <- ifelse(allshot_xG$xG > 0.79, "TRUE", "FALSE")
  confusion_total <- confusionMatrix(as.factor(rf_xG_preds), as.factor(allshot_xG$shot.isGoal))
  confusion_total
  
  ##### Gini Impurity & Index#####
  importance(rf_model)
  varImpPlot(rf_model)
  
  allshot_xG$Gini_Index <- 1 - (allshot_xG$xG^2 + (1 - allshot_xG$xG)^2)
  mean_gini_index <- mean(allshot_xG$Gini_Index)
  print(mean_gini_index)
  # 0.1111 good, 0.5 is guess, 0 is perfect
  
  ##### MSE #####
  allshot_xG$shot.isGoal_numeric <- ifelse(allshot_xG$shot.isGoal == "TRUE", 1, 0)
  mse_tree <- mean((allshot_xG$xG - allshot_xG$shot.isGoal_numeric)^2)
  mse_tree
  
  ##### Entropi #####
  # https://www.analyticsvidhya.com/blog/2020/11/entropy-a-key-concept-for-all-data-science-beginners/
  rf_model_entropy <- ranger(variables, 
                             data = train_data, 
                             num.trees = results$ntree[1], 
                             mtry = mtry_best, 
                             importance = "permutation", 
                             splitrule = "extratrees", 
                             probability = TRUE) 
  
  allshot_xG$xG_entropy <- predict(rf_model_entropy, data = allshot_xG)$predictions[, "TRUE"]
  
  allshot_xG$Entropy <- -(allshot_xG$xG_entropy * log2(ifelse(allshot_xG$xG_entropy == 0, 1e-10, allshot_xG$xG_entropy)) +
                            (1 - allshot_xG$xG_entropy) * log2(ifelse(1 - allshot_xG$xG_entropy == 0, 1e-10, 1 - allshot_xG$xG_entropy)))
  mean_entropy <- mean(allshot_xG$Entropy)
  print(mean_entropy)

  

    ##### For WyScout xG #####
    allshot_xG$Gini_Index_Wyscout <- 1 - (allshot_xG$shot.xg^2 + (1 - allshot_xG$shot.xg)^2)
    mean_gini_index_wyscout <- mean(allshot_xG$Gini_Index_Wyscout)
    print(mean_gini_index_wyscout)
  
    allshot_xG$Entropy_Wyscout <- -(allshot_xG$shot.xg * log2(ifelse(allshot_xG$shot.xg == 0, 1e-10, allshot_xG$shot.xg)) +
                              (1 - allshot_xG$shot.xg) * log2(ifelse(1 - allshot_xG$shot.xg == 0, 1e-10, 1 - allshot_xG$shot.xg)))
    mean_entropy_Wyscout <- mean(allshot_xG$Entropy_Wyscout)
    print(mean_entropy_Wyscout)
    
    mse_wyscout <- mean((allshot_xG$shot.xg - allshot_xG$shot.isGoal_numeric)^2)
    mse_wyscout
    
    wyscout_preds <- ifelse(allshot_xG$shot.xg > 0.79, "TRUE", "FALSE")
    wyscout_confusion <- confusionMatrix(as.factor(wyscout_preds), as.factor(allshot_xG$shot.isGoal))
    wyscout_confusion
    
  
  
    
    
    #### ERROR !!!Find the best depth for a single tree-model using Gini Index ####
    depth_range <- 20
    gini_train <- numeric(depth_range)
    gini_test <- numeric(depth_range)
    gini_cv <- numeric(depth_range)
    
    mse_train <- numeric(depth_range)
    mse_test <- numeric(depth_range)
    mse_cv <- numeric(depth_range)
    
    # Create amount of folds for CV
    folds <- createFolds(train_data$shot.isGoal, k = 5, list = TRUE)  
    
    for (i in 1:depth_range) {
      tree_loop <- rpart(variables,
                         data = train_data,
                         method = "class",
                         control = rpart.control(maxdepth = i, cp = 0.01))  
      
      # Predict probabilities
      loop_train <- predict(tree_loop, newdata = train_data, type = "prob")[, "TRUE"]
      loop_test <- predict(tree_loop, newdata = test_data, type = "prob")[, "TRUE"]
      
      # Convert shot.isGoal to numeric (0/1)
      goal_numeric_train <- ifelse(train_data$shot.isGoal == "TRUE", 1, 0)
      goal_numeric_test <- ifelse(test_data$shot.isGoal == "TRUE", 1, 0)
      
      # Compute Gini Index for train and test sets
      gini_train[i] <- mean(1 - (loop_train^2 + (1 - loop_train)^2))
      gini_test[i] <- mean(1 - (loop_test^2 + (1 - loop_test)^2))
      
      # Compute MSE for train and test sets
      mse_train[i] <- mean((loop_train - goal_numeric_train)^2)
      mse_test[i] <- mean((loop_test - goal_numeric_test)^2)
      
      # Cross-validation for Gini & MSE
      gini_folds <- numeric(5)
      mse_folds <- numeric(5)
      
      for (f in 1:5) {
        train_idx <- setdiff(seq_len(nrow(train_data)), folds[[f]])
        val_idx   <- folds[[f]]
        
        tree_cv <- rpart(variables,
                         data = train_data[train_idx, ],
                         method = "class",
                         control = rpart.control(maxdepth = i, cp = 0.01))
        
        loop_val <- predict(tree_cv, newdata = train_data[val_idx, ], type = "prob")[, "TRUE"]
        goal_numeric_val <- ifelse(train_data$shot.isGoal[val_idx] == "TRUE", 1, 0)
        
        gini_folds[f] <- mean(1 - (loop_val^2 + (1 - loop_val)^2))
        mse_folds[f] <- mean((loop_val - goal_numeric_val)^2)
      }
      
      gini_cv[i] <- mean(gini_folds)
      mse_cv[i] <- mean(mse_folds)
    }
    
    # Find the best depth based on Cross-Validation
    best_depth_gini <- which.min(gini_cv)
    best_depth_mse <- which.min(mse_cv)
    
    cat("Best depth based on Gini Index:", best_depth_gini, "\n")
    cat("Best depth based on MSE:", best_depth_mse, "\n")
    
    
    # Find the best depth (lowest CV Gini Index)
    best_depth <- which.min(gini_cv)
    
    # Print results
    cat("Best tree depth based on Gini Index:", best_depth, "\n")
  
    # Gini Index plot
    depth_results_gini <- data.frame(
      maxdepth = rep(1:depth_range, 3),
      Value = c(gini_train, gini_test, gini_cv),
      Type = rep(c("Training", "Test", "Cross-Validation"), each = depth_range)
    )
    
    ggplot(depth_results_gini, aes(x = maxdepth, y = Value, color = Type)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = 1:depth_range) +
      labs(title = "Optimal trædybde baseret på Gini Index",
           x = "Tree Depth",
           y = "Gini Index") +
      theme_minimal()
  
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
  
  #### Find the best depth for a single tree-model using MSE ####
  depth_range <- 7
  mse_train <- numeric(depth_range)
  mse_test <- numeric(depth_range)
  mse_cv <- numeric(depth_range)
  
  # Create amount of folds for CV
  set.seed(123)
  folds <- createFolds(train_data$shot.isGoal, k = 10, list = TRUE)  # Using k = 10 for stability
  
  for (i in 1:depth_range) {
    tree_loop <- rpart(variables,
                       data = train_data,
                       method = "class",
                       control = rpart.control(maxdepth = i, cp = 0.005))  # Lower cp to allow more natural splits
    
    # Predict probabilities
    loop_train <- predict(tree_loop, newdata = train_data, type = "prob")[, "TRUE"]
    loop_test <- predict(tree_loop, newdata = test_data, type = "prob")[, "TRUE"]
    
    # Convert shot.isGoal to numeric (0/1)
    goal_numeric_train <- ifelse(train_data$shot.isGoal == "TRUE", 1, 0)
    goal_numeric_test <- ifelse(test_data$shot.isGoal == "TRUE", 1, 0)
    
    # Compute MSE for train and test sets
    mse_train[i] <- mean((loop_train - goal_numeric_train)^2)
    mse_test[i] <- mean((loop_test - goal_numeric_test)^2)
    
    # Cross-validation for MSE
    mse_folds <- numeric(10)
    
    for (f in 1:10) {
      train_idx <- setdiff(seq_len(nrow(train_data)), folds[[f]])
      val_idx   <- folds[[f]]
      
      tree_cv <- rpart(variables,
                       data = train_data[train_idx, ],
                       method = "class",
                       control = rpart.control(maxdepth = i, cp = 0.005))
      
      loop_val <- predict(tree_cv, newdata = train_data[val_idx, ], type = "prob")[, "TRUE"]
      goal_numeric_val <- ifelse(train_data$shot.isGoal[val_idx] == "TRUE", 1, 0)
      
      mse_folds[f] <- mean((loop_val - goal_numeric_val)^2)
    }
    
    mse_cv[i] <- mean(mse_folds)
  }
  
  # Find the best depth based on the lowest Cross-Validation MSE
  best_depth_mse <- which.min(mse_cv)
  best_mse_value <- min(mse_cv)
  
  # Print results
  cat("Best tree depth based on Cross-Validation MSE:", best_depth_mse, "\n")
  cat("Lowest Cross-Validation MSE:", round(best_mse_value, 6), "\n")
  
  
  # Store results in a dataframe
  depth_results_mse <- data.frame(
    maxdepth = rep(1:depth_range, 3),
    MSE = c(mse_train, mse_test, mse_cv),
    Type = rep(c("Training", "Test", "Cross-Validation"), each = depth_range)
  )
  
  # Plot MSE across tree depths
  ggplot(depth_results_mse, aes(x = maxdepth, y = MSE, color = Type)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:depth_range) +
    labs(title = "Optimal Tree Depth Based on Cross-Validation MSE",
         x = "Tree Depth",
         y = "MSE") +
    theme_minimal()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
                       control = rpart.control(maxdepth = i, cp = 0))  
    
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
  
#### Singular tree-model ####
  singular_tree <- rpart(variables,
                      data = train_data,
                      method = "class",
                      control = rpart.control(maxdepth = best_depth_mse, cp = 0.005))
  rpart.plot(singular_tree, type = 2, extra = 104, box.palette = "BuGn")
  singular_tree$variable.importance
  
  
  
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
  
  
  
  #### Save RDS for Shiny ####
  saveRDS(allshot_xG,"allshot_xG.rds")
  saveRDS(rf_model,"rf_model_simple.rds")  
  