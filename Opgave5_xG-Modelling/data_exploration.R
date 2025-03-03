# Procent of shots is goal
isGoal_df <- as.data.frame(allshot$shot.isGoal)
colnames(isGoal_df) <- "isGoal"
goal_percentage <- mean(isGoal_df$isGoal) * 100
print(goal_percentage)





# Difference between Poland, Dutch and total
#


xG_players <- allshot_xG %>% group_by(player.name) %>% summarize(xG = mean(shot.xg))




# X-variables
# 1 Bodypart
bodypart_xG <- allshot_xG %>%
  group_by(shot.bodyPart, shot.isGoal) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(shot.bodyPart) %>%
  mutate(percent = count / sum(count) * 100)

bodypart_xG$percent <- round(bodypart_xG$percent,1)

ggplot(bodypart_xG, aes(x = shot.bodyPart, y = percent, fill = shot.isGoal)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Head or others has the best goal success ratio",
       x = "Body Part",
       y = "Percentage of goal success ratio",
       fill = "Is Goal") +
  theme_minimal()
# maybe make boolean right/left
# and also legs / other


# 2 Angle
## define goalpost positions
goalpost_1x <- 100
goalpost_1y <- 37

goalpost_2x <- 100
goalpost_2y <- 63

## calculate angle
allshot_xG$shot_angle <- abs(
  atan2(goalpost_2y - allshot_xG$possession.endLocation.y, goalpost_2x - allshot_xG$possession.endLocation.x) - 
    atan2(goalpost_1y - allshot_xG$possession.endLocation.y, goalpost_1x - allshot_xG$possession.endLocation.x)
)

# convert to degrees
allshot_xG$shot_angle_degrees <- allshot_xG$shot_angle * (180 / pi)

hist(allshot_xG$shot_angle_degrees)

# define goal parameters
goal_width <- 7.32  # width of the goal in meters
goal_center_y <- 50  # y-coordinate of the goal center
goal_x <- 100        # x-coordinate of the goal line

# calculate shot angle using the provided formula
allshot_xG <- allshot_xG %>%
  mutate(
    x = abs(goal_x - possession.endLocation.x),  # distance to goal line
    y = abs(possession.endLocation.y - goal_center_y),  # lateral distance from goal center
    shot_angle = atan((goal_width * x) / (x^2 + y^2 - (goal_width / 2)^2)) * 180 / pi
  )

# visualize the new angle distribution
hist(allshot_xG$shot_angle, breaks = 30, main = "Shot Angle Distribution", xlab = "Angle (degrees)")



### Dot prodokt
# goal post positions
goalpost1_x <- 100
goalpost1_y <- 37

goalpost2_x <- 100
goalpost2_y <- 63

# calculate the angle using the dot product method and store as shot_angle_dp
allshot_xG <- allshot_xG %>%
  mutate(
    # vectors to goalposts
    vector1_x = goalpost1_x - possession.endLocation.x,
    vector1_y = goalpost1_y - possession.endLocation.y,
    
    vector2_x = goalpost2_x - possession.endLocation.x,
    vector2_y = goalpost2_y - possession.endLocation.y,
    
    # dot product of the vectors
    dot_product = vector1_x * vector2_x + vector1_y * vector2_y,
    
    # magnitudes of the vectors
    magnitude1 = sqrt(vector1_x^2 + vector1_y^2),
    magnitude2 = sqrt(vector2_x^2 + vector2_y^2),
    
    # calculate the angle using the dot product formula
    shot_angle_dp = acos(dot_product / (magnitude1 * magnitude2)) * 180 / pi
  )

# visualize the new angle distribution
hist(allshot_xG$shot_angle_dp, breaks = 30, 
     main = "Shot Angle Distribution (Dot Product Method)", 
     xlab = "Angle (degrees)")



# define goal parameters
goal_width <- 7.32  # width of the goal in meters
goal_center_y <- 50  # center of the goal
goal_x <- 100        # goal line x-coordinate

# calculate the shot angle using the geometry of shooting method
allshot_xG <- allshot_xG %>%
  mutate(
    x = abs(goal_x - possession.endLocation.x),  # distance to goal line
    y = abs(possession.endLocation.y - goal_center_y),  # lateral distance from goal center
    
    # calculate the goal angle using the geometry method
    shot_angle_geom = atan2(goal_width * x, 
                            x^2 + y^2 - (goal_width / 2)^2) * 180 / pi
  )
# filter shots taken from the penalty spot (90, 50)
hist(allshot_xG$shot_angle_geom)

# define goal parameters
goal_width <- 11.43  # width of the goal in meters
goal_center_y <- 50  # center of the goal
goal_x <- 100        # goal line x-coordinate

# calculate the shot angle using the geometry of shooting method
allshot_xG <- allshot_xG %>%
  mutate(
    x = abs(goal_x - possession.endLocation.x),  # distance to goal line
    y = abs(possession.endLocation.y - goal_center_y),  # lateral distance from goal center
    
    # calculate the goal angle using the geometry method
    shot_angle_geom = atan2(goal_width * x, 
                            x^2 + y^2 - (goal_width / 2)^2) * 180 / pi
  )
# filter shots taken from the penalty spot (90, 50)
tmp_penalty_shots <- allshot %>%
  filter(possession.endLocation.x == 90, possession.endLocation.y == 50) %>%
  select(possession.endLocation.x, possession.endLocation.y, shot_angle_geom)
hist(allshot$shot_angle)

#### Duration ####
hist(allshot_xG$possession.duration, breaks = 50, col = "gray", main = "Histogram of possession duration")
summary(allshot_xG$possession.duration)
quantile(allshot_xG$possession.duration, probs = seq(0, 1, 0.1), na.rm = TRUE)
table(cut(allshot_xG$possession.duration, breaks = seq(0, max(allshot_xG$possession.duration), by = 10)))
allshot_xG$possession_duration_cat <- cut(
  allshot_xG$possession.duration,
  breaks = c(-Inf, 3.5, 9, 17, 35, Inf),
  labels = c("Meget kort", "Kort", "Mellem", "Lang", "Meget lang"),
  right = TRUE
)


#### Y er KUN 100, ligesom x, de er normaliceret, der er ikek taget højde for at y er længere end x
# vinkel var correct


# Player posision
# Evt reduce into less


# 3 Distance
allshot_xG$shot_distance <- sqrt((100 - allshot_xG$possession.endLocation.x)^2 + 
                                   (50 - allshot_xG$possession.endLocation.y)^2)
# 4 Minute
# 5 Assist type
# 6 assist posision/length
#.7 Possession duration
# 8 Possesion start?
# 9 From set piece 
# 10 From counter or transition 
#
#
#