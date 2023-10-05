# Load the required libraries
library(ggplot2)
library(gridExtra)
library(scales)

# Load the data from the provided file
pitch_data <- read.csv("Master_Fall_2022.csv")
pitch_data <- pitch_data[pitch_data$TaggedPitchType != "Undefined", ]

# Define colors for each pitch type
pitch_colors <- c("Fastball" = "red", "Slider" = "blue", "Curveball" = "green", "ChangeUp" = "orange",
                  "Cutter" = "purple", "Sinker" = "pink", "Knuckle-Curve" = "yellow")

# Create a list to store the plots
plots_list <- list()

# Plot a graphs for each pitcher
pitchers <- unique(pitch_data$Pitcher)
for (pid in pitchers) {
  pid_data <- pitch_data[pitch_data$Pitcher == pid, ]
  
  # Plot a histogram of pitch speeds
  velo_plot <- ggplot(pid_data, aes(x = RelSpeed, fill = TaggedPitchType)) +
    geom_histogram(binwidth = 1) +
    scale_fill_manual(values = pitch_colors) +
    labs(title = "Pitch Speeds by Pitch Type",
         x = "Pitch Speed (mph)",
         y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.3))
  
  # Plot a graph of spin rates
  spinrate_plot <- ggplot(pid_data, aes(x = Extension, y = SpinRate, color = TaggedPitchType)) +
    geom_point() +
    scale_color_manual(values = pitch_colors) +
    labs(title = "Extension vs. Spin Rate",
         x = "Extension (ft)",
         y = "Spin Rate (rpm)")+
    theme(plot.title = element_text(hjust = 0.3))
  
  # Plot a graph of vertical/horizontal break
  vertical_break_plot <- ggplot(pid_data, aes(x = TaggedPitchType, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_boxplot() +
    scale_color_manual(values = pitch_colors) +
    labs(title = "Vertical Break by Pitch Type",
         x = "Pitch Type",
         y = "Vertical Break (in)")+
    theme(plot.title = element_text(hjust = 0.3))
  
  horizontal_break_plot <- ggplot(pid_data, aes(x = TaggedPitchType, y = HorzBreak, color = TaggedPitchType)) +
    geom_boxplot() +
    scale_color_manual(values = pitch_colors) +
    labs(title = "Horizontal Break by Pitch Type",
         x = "Pitch Type",
         y = "Horizontal Break (in)")+
    theme(plot.title = element_text(hjust = 0.3))
  
  # Plot a graph of release points
  release_plot <- ggplot(pid_data, aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    geom_point() +
    scale_color_manual(values = pitch_colors) +
    labs(title = "Release Position",
         x = "Horizontal Location (ft)",
         y = "Vertical Location (ft)",
         color = "Pitch Type")+
    theme(plot.title = element_text(hjust = 0.3))
  
  # Plot a graph of the pitch location
  strike_zone_plot <- ggplot(pid_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    geom_point(size = 2) +
    scale_color_manual(values = pitch_colors) +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
    labs(title = "Strike Zone Plot",
         x = "X Location",
         y = "Y Location",
         color = "Pitch Type") +
    theme(plot.title = element_text(hjust = 0.3)) + xlim(-2.75, 2.75) + ylim(-0.25, 4.75)
  
  # Plot a pitch location heat map
  total_obs <- nrow(pid_data)
  pitch_heat_map <- ggplot(pid_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_bin2d(binwidth = c(0.5,0.5), aes(fill = round(..count../total_obs*100))) +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
    scale_fill_gradient(low = "white", high = "red", name = "Count (%)") +
    labs(title = "Pitch Location Heat Map",
         x = "X Location",
         y = "Y Location",
         fill = "Count") +
    theme(plot.title = element_text(hjust = 0.3)) + xlim(-2.75, 2.75) + ylim(-0.25, 4.75)
  
  # Plot a graph of pitch type usage
  pitch_usage <- ggplot(pid_data, aes(x = TaggedPitchType, fill = TaggedPitchType)) +
    geom_bar() +
    scale_fill_manual(values = pitch_colors) +
    labs(title = "Pitch Type Usage",
         x = "Pitch Type",
         y = "Count") +
    theme(plot.title = element_text(hjust = 0.3))
  
  # Add the plots to the list of plots
  plots_list[[length(plots_list)+1]] <- velo_plot
  plots_list[[length(plots_list)+1]] <- spinrate_plot
  plots_list[[length(plots_list)+1]] <- vertical_break_plot
  plots_list[[length(plots_list)+1]] <- horizontal_break_plot
  plots_list[[length(plots_list)+1]] <- release_plot
  plots_list[[length(plots_list)+1]] <- strike_zone_plot
  plots_list[[length(plots_list)+1]] <- pitch_heat_map
  plots_list[[length(plots_list)+1]] <- pitch_usage
  
  # Combine the plots using grid.arrange()
  grid.arrange(grobs = plots_list, ncol = 1, layout_matrix = rbind(c(1,2), c(3,4), c(5,6), c(7,8)),
               bottom = NULL, 
               top = paste("Pitching Analysis for pitcher", pid),
               left = NULL, right = NULL)
  
  # Empty the lists
  plots_list <- list()
}
