# Procent of shots is goal
isGoal_df <- as.data.frame(allshot$shot.isGoal)
colnames(isGoal_df) <- "isGoal"
goal_percentage <- mean(isGoal_df$isGoal) * 100
print(goal_percentage)





# Difference between Poland, Dutch and total
#