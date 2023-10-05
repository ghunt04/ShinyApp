# Load required packages
library(ggplot2)
library(dplyr)
library(gridExtra)

# Function to calculate accuracy
calculate_accuracy <- function(df) {
  df <- df[df$PitchCall == "BallCalled" | df$PitchCall == "StrikeCalled", ]
  correct_called_balls <- sum(df$PitchCall == "BallCalled" & (df$PlateLocSide < -0.7083 | df$PlateLocSide > 0.7083 | df$PlateLocHeight < 1.5 | df$PlateLocHeight > 3.5))
  correct_called_strikes <- sum(df$PitchCall == "StrikeCalled" & df$PlateLocSide >= -0.7083 & df$PlateLocSide <= 0.7083 & df$PlateLocHeight >= 1.5 & df$PlateLocHeight <= 3.5)
  
  total_balls <- sum(df$PlateLocSide < -0.7083 | df$PlateLocSide > 0.7083 | df$PlateLocHeight < 1.5 | df$PlateLocHeight > 3.5)
  total_strikes <- sum(df$PlateLocSide >= -0.7083 & df$PlateLocSide <= 0.7083 & df$PlateLocHeight >= 1.5 & df$PlateLocHeight <= 3.5)
  
  ball_accuracy <- correct_called_balls / total_balls
  strike_accuracy <- correct_called_strikes / total_strikes
  return(list(ball_accuracy = ball_accuracy, strike_accuracy = strike_accuracy,
              correct_balls = correct_called_balls, total_balls = total_balls,
              correct_strikes = correct_called_strikes, total_strikes = total_strikes))
}

# Function to generate strike zone plot for incorrect called balls
plot_incorrect_balls <- function(df) {
  incorrect_calls <- df %>%
    filter(PitchCall == "BallCalled" & (PlateLocSide >= -0.7083 & PlateLocSide <= 0.7083 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5))
  
  p <- ggplot(incorrect_calls, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_point(color = "green") +
    annotate("rect", xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", size = 1) +
    annotate("rect", xmin = -0.8305, xmax = 0.8305, ymin = 1.3775, ymax = 3.6225,
             fill = NA, color = "#555555", size = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(-3, 3), ylim = c(0, 5)) +
    labs(x = "Horizontal Position", y = "Vertical Position") +
    ggtitle("Incorrect Called Balls") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Function to generate strike zone plot for incorrect called strikes
plot_incorrect_strikes <- function(df) {
  incorrect_calls <- df %>%
    filter(PitchCall == "StrikeCalled" & (PlateLocSide < -0.7083 | PlateLocSide > 0.7083 | PlateLocHeight < 1.5 | PlateLocHeight > 3.5))
  
  p <- ggplot(incorrect_calls, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_point(color = "red") +
    annotate("rect", xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", size = 1) +
    annotate("rect", xmin = -0.8305, xmax = 0.8305, ymin = 1.3775, ymax = 3.6225,
             fill = NA, color = "#555555", size = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(-3, 3), ylim = c(0, 5)) +
    labs(x = "Horizontal Position", y = "Vertical Position") +
    ggtitle("Incorrect Called Strikes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Function to generate pie chart
plot_accuracy_pie <- function(accuracy, title, pitch) {
  df <- data.frame(
    variable = c("Correct", "Incorrect"),
    accuracy = c(accuracy, 1 - accuracy)
  )
  
  color <- ifelse(pitch == "strike", "green", ifelse(pitch == "both", "gold", "red"))
  p <- ggplot(df, aes(x = "", y = accuracy, fill = variable)) +
    geom_bar(stat = "identity", width = 1, color = NA) +
    coord_polar(theta = "y") +
    labs(fill = NULL) +
    scale_fill_manual(values = c("Correct" = color, "Incorrect" = "white"), guide = FALSE) +
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = -0.75, y = 0, label = paste0(scales::percent(accuracy)),
             vjust = 0.4, hjust = 0.45, size = 8)
  
  return(p)
}


# Load Trackman data
trackman_data <- read.csv("Master_Fall_2022.csv")

# Calculate accuracy
accuracy <- calculate_accuracy(trackman_data)

# Generate plots and pie charts
incorrect_balls_plot <- plot_incorrect_balls(trackman_data)
incorrect_strikes_plot <- plot_incorrect_strikes(trackman_data)
strike_accuracy_pie <- plot_accuracy_pie(accuracy$strike_accuracy, "In-Zone Accuracy", "strike")
ball_accuracy_pie <- plot_accuracy_pie(accuracy$ball_accuracy, "Out-Zone Accuracy", "ball")
overall_accuracy <- (accuracy$correct_balls+accuracy$correct_strikes)/(accuracy$total_balls+accuracy$total_strikes)
overall_accuracy_pie <- plot_accuracy_pie(overall_accuracy, "Overall Accuracy", "both")
# Arrange plots and pie charts in a grid
grid.arrange(arrangeGrob(incorrect_balls_plot, incorrect_strikes_plot, ncol = 2),
             arrangeGrob(strike_accuracy_pie, overall_accuracy_pie, ball_accuracy_pie, ncol = 3),
             ncol = 1, top = "Accuracy Visualization",
             heights = c(2, 1.5))



