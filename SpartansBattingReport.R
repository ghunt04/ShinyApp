# Load required packages
library(tidyverse)
library(lubridate)

# Read in Trackman data
trackman <- read.csv("Master_Fall_2022.csv")


#### Convert columns to appropriate data types
#data$Player <- as.factor(data$Player)
#data$Swing <- as.integer(data$Swing)
#data$Bat_Speed <- as.numeric(data$Bat.Speed..mph.)
#data$Attack_Angle <- as.numeric(data$Attack.Angle..deg.)
#data$Time_to_Contact <- as.numeric(data$Time.to.Contact..sec.)
#data$Peak_Hand_Speed <- as.numeric(data$Peak.Hand.Speed..mph.)
#data$Blast_Factor <- as.numeric(data$Blast.Factor)
#data$On_Plane <- as.numeric(data$On.Plane....)
#data$Power <- as.numeric(data$Power..kW.)
#data$Peak_Bat_Speed <- as.numeric(data$Peak.Bat.Speed..mph.)
#data$Body_Rotation <- as.numeric(data$Body.Rotation....)
#data$Vertical_Bat_Angle <- as.numeric(data$Vertical.Bat.Angle..deg.)
#data$Woba <- as.numeric(data$Season.Woba)

#trackman$ExitVelo <- trackman$exit


print(nrow(subset(trackman, ExitSpeed > 100)))
# Clean the data
trackman_clean <- trackman %>%
  select(PlayResult, Angle, ExitSpeed, HitSpinRate, Direction, KorBB, TaggedPitchType) %>%
  filter(!is.na(PlayResult) & !is.na(ExitSpeed) & !is.na(Angle) & !is.na(HitSpinRate) & !is.na(Direction) & !is.na(KorBB)) %>%
  mutate(hit_type = if_else(PlayResult == "Single" | PlayResult == "Double" | PlayResult == "Triple" | PlayResult == "Home Run", "Hit", if_else(PlayResult != "Undefined", "Out", if_else(KorBB == "Walk", "Walk", "StrikeOut")))) %>%
  mutate(hit_type = factor(hit_type, levels = c("Out", "Hit", "Walk", "StrikeOut")))

# Calculate metrics
metrics <- trackman_clean %>%
  group_by(hit_type) %>%
  summarize(
    Exit_Speed = mean(ExitSpeed),
    Launch_Angle = mean(Angle),
    spinrate = mean(HitSpinRate),
    direction = mean(Direction),
  )
metrics

#### Create visualizations ####

# Create the histogram
ggplot(trackman_clean, aes(x = TaggedPitchType, y = ExitSpeed)) +
  geom_histogram(stat = "identity", position = "dodge", fill = "blue", alpha = 0.6) +
  labs(x = "Tagged Pitch Type", y = "Exit Speed") +
  ggtitle("Exit Speed Distribution by Tagged Pitch Type")

ggplot(trackman_clean, aes(x = launch_angle, y = launch_speed, color = hit_type)) +
  geom_point(alpha = 0.5) +
  labs(title = "Launch Angle vs. Launch Speed",
       x = "Launch Angle (degrees)",
       y = "Launch Speed (mph)") +
  theme_minimal()

ggplot(metrics, aes(x = hit_type, y = avg_launch_speed, fill = hit_type)) +
  geom_col() +
  labs(title = "Average Launch Speed by Hit Type",
       x = "Hit Type",
       y = "Average Launch Speed (mph)") +
  theme_minimal()
