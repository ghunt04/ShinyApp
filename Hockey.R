library(tidyverse)
library(ggplot2)
library(ggforce)
library(grid)
library(gridExtra)
library(plotly)
library(corrplot)
library(dplyr)
library(tidyr)

data <- read.csv("HockeyData.csv")

# Remove rows with NA values in the 'Quadrant' column
data <- data[!is.na(data$Quadrant), ]
data <- data[data$Name == "Ch. Krygier", ]
data <- data[data$Period == "2", ]

# Setting up colour values
hockey_red <- "#FF0000" 
  hockey_blue <- "#0000FF" 
    hockey_light_blue <- "#CCF5FF"
      
    # Function to calculate the color based on the number (ranging from 0 to 1)
    get_dynamic_color <- function(number) {
      # Convert the number to a transparency level (alpha value) between 00 and FF in hexadecimal
      alpha_value <- sprintf("%02X", round(number * 255))
      # Set the color as a darker green and apply the transparency
      color <- paste0("#006400", alpha_value)  # Dark green ("#006400") with the calculated transparency
      return(color)
    }
    
    nhl_full_rink_plot <- function () {
      quad_1 <- sum(data$Quadrant == 1) / nrow(data)  # Top quadrant
      quad_2 <- sum(data$Quadrant == 2) / nrow(data)  # Top-Middle quadrant
      quad_3 <- sum(data$Quadrant == 3) / nrow(data)  # Bottom-Middle quadrant
      quad_4 <- sum(data$Quadrant == 4) / nrow(data)  # Bottom quadrant
      ggplot() +
        
        # Adding code to highlight quadrants
        geom_rect(aes(xmin = -100, xmax = 100, ymin = 21.25, ymax = 42.5), fill = get_dynamic_color(quad_1)) +      # Bottom-Right quadrant
        geom_rect(aes(xmin = -100, xmax = 100, ymin = 0, ymax = 21.25), fill = get_dynamic_color(quad_2)) +     # Bottom-Left quadrant
        geom_rect(aes(xmin = -100, xmax = 100, ymin = -21.25, ymax = 0), fill = get_dynamic_color(quad_3)) +   # Top-Right quadrant
        geom_rect(aes(xmin = -100, xmax = 100, ymin = -42.5, ymax = -21.25), fill = get_dynamic_color(quad_4)) +  # Top-Left quadrant
        
        # Adding the color scale on the right side of the graph
        #scale_fill_gradient(name = "Quadrant Proportion", low = "#00640000", high = "#006400FF", 
        #                    breaks = seq(0, 1, by = 0.2), labels = scales::percent_format(scale = 1)) +
        
        # Hash marks in T-R/B-R/T-L/B-R order, groups of four
        geom_tile(aes(x = 66.125, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 66.125, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 71.875, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 71.875, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 66.125, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 66.125, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 71.875, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = 71.875, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -66.125, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -66.125, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -71.875, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -71.875, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -66.125, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -66.125, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -71.875, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
        geom_tile(aes(x = -71.875, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
        
        # Centre line
        geom_tile(aes(x = 0, y = 0, width = 1, height = 85), fill = hockey_red) + # Centre line
        
        # Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
        geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), colour = "#FF99B4", fill = "#FF99B4", size = 0) + # Centre dot with unique red
        geom_circle(aes(x0 = 69, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Top-Right
        geom_circle(aes(x0 = 69, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Bottom-Right
        geom_circle(aes(x0 = -69, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Top-Left
        geom_circle(aes(x0 = -69, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Bottom-Left
        
        geom_circle(aes(x0 = 20.5, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Top-Right
        geom_circle(aes(x0 = 20.5, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Bottom-Right
        geom_circle(aes(x0 = -20.5, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Top-Left
        geom_circle(aes(x0 = -20.5, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Bottom-Left
        
        # Ells surrounding faceoff dots
        geom_tile(aes(x = 65, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Top-Right
        geom_tile(aes(x = 73, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 65, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 73, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 66.92, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = 71.08, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = 66.92, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = 71.08, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        
        geom_tile(aes(x = 65, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Bottom-Right
        geom_tile(aes(x = 73, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 65, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 73, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 66.92, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = 71.08, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = 66.92, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = 71.08, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        
        geom_tile(aes(x = -65, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Top-Left
        geom_tile(aes(x = -73, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -65, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -73, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -66.92, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = -71.08, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = -66.92, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = -71.08, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        
        geom_tile(aes(x = -65, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Bottom-Left
        geom_tile(aes(x = -73, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -65, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -73, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -66.92, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = -71.08, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = -66.92, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        geom_tile(aes(x = -71.08, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
        
        # Referee crease
        geom_arc(aes(x0 = 0, y0 = -42.5, start = -pi / 2, end = pi / 2, r = 10), colour = hockey_red) +
        
        # Left goalie crease
        geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8), fill = hockey_light_blue) +
        geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), fill = hockey_light_blue, colour = hockey_light_blue, size = 1 / 12) + # manually adjusted arc
        geom_tile(aes(x = -86.75, y = -4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = -86.75, y = 4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
        geom_arc(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), colour = hockey_red, size = 2 / 12) + # manually adjusted arc
        geom_tile(aes(x = -85, y = 3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
        geom_tile(aes(x = -85, y = -3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
        
        # Right goalie crease
        geom_tile(aes(x = 86.75, y = 0, width = 4.5, height = 8), fill = hockey_light_blue) +
        geom_arc_bar(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), fill = hockey_light_blue, colour = hockey_light_blue, size = 1 / 12) + # manually adjusted arc
        geom_tile(aes(x = 86.75, y = -4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
        geom_tile(aes(x = 86.75, y = 4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
        geom_arc(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), colour = hockey_red, size = 2 / 12) + # manually adjusted arc
        geom_tile(aes(x = 85, y = 3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
        geom_tile(aes(x = 85, y = -3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
        
        # Goalie nets placed as rectangles
        geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Left # with grey fills
        geom_tile(aes(x = 90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
        
        # Trapezoids
        geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(10.92, 11.08, 7.08, 6.92)), fill = hockey_red) + # Left
        geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = hockey_red) + # Left 
        geom_polygon(aes(x = c(100, 100, 89, 89), y = c(10.92, 11.08, 7.08, 6.92)), fill = hockey_red) + # Right
        geom_polygon(aes(x = c(100, 100, 89, 89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = hockey_red) + # Right
        
        # Lines
        geom_tile(aes(x = -25.5, y = 0, width = 1, height = 85), fill = hockey_blue) + # Left Blue line
        geom_tile(aes(x = 25.5, y = 0, width = 1, height = 85),  fill = hockey_blue) + # Right Blue line
        geom_tile(aes(x = -89, y = 0, width = 2 / 12, height = 73.50), fill = hockey_red) + # Left goal line (73.5 value is rounded from finding intersect of goal line and board radius)
        geom_tile(aes(x = 89, y = 0, width = 2 / 12, height = 73.50), fill = hockey_red) + # Right goal line
        
        # Borders as line segments - plotted last to cover up line ends, etc.
        geom_line(aes(x = c(-72, 72), y = c(42.5, 42.5))) + # Top
        geom_line(aes(x = c(-72, 72), y = c(-42.5, -42.5))) + # Bottom
        geom_line(aes(x = c(-100, -100), y = c(-14.5, 14.5))) + # Left
        geom_line(aes(x = c(100, 100), y = c(-14.5, 14.5))) + # Right
        geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
        geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
        geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
        geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
        
        # Zone description
        annotate("text", -1.5, 31.875, label = paste("ZONE 1  ", round(quad_1*100,1), "%"), size = 6, fontface = "bold") +
        annotate("text", -1.5, 10.625, label = paste("ZONE 2  ", round(quad_2*100,1), "%"), size = 6, fontface = "bold") +
        annotate("text", -1.5, -10.625, label = paste("ZONE 3  ", round(quad_3*100,1), "%"), size = 6, fontface = "bold") +
        annotate("text", -1.5, -31.875, label = paste("ZONE 4  ", round(quad_4*100,1), "%"), size = 6, fontface = "bold") +
        
        labs(title = "Zone Entry Position") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
        ) +
        
        # Fixed scale for the coordinate system  
        coord_fixed()
    }
    
    # Calculate the count of each entry type
    entry_counts <- table(data$EntryType)
    
    # Calculate the percentage of each entry type relative to the total entries
    total_entries <- length(data$EntryType)
    entry_percentages <- (entry_counts / total_entries) * 100
    
    # Convert entry_percentages data to a data frame for plotting
    entry_type_data <- data.frame(EntryType = names(entry_percentages), EntryPercentage = entry_percentages)
    
    #### ENTRIES LEADING TO SHOTS #####
    # Filter the data and only keep rows where FinalPlay is SGF, SGR, or G
    shots_data <- data[data$FinalPlay %in% c("SGF", "SGR", "G"), ]
    
    # Calculate the count of each entry type in the filtered data
    shot_counts <- table(shots_data$EntryType)
    
    # Calculate the percentage of each entry type relative to the total entries
    shots_percentages <- (shot_counts / total_entries) * 100
    
    # Convert the table to a data frame
    shot_percentages_df <- as.data.frame(shots_percentages)
    
    # Check if 'chip' is present in the data frame
    if (!'chip' %in% shot_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "chip", Freq = 0))
    }
    # Check if 'dump' is present in the data frame
    if (!'dump' %in% shot_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "dump", Freq = 0))
    }
    # Check if 'carry' is present in the data frame
    if (!'carry' %in% shot_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "carry", Freq = 0))
    }
    # Check if 'pass' is present in the data frame
    if (!'pass' %in% shot_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "pass", Freq = 0))
    }
    
    # Convert entry_percentages data to a data frame for plotting
    shot_type_data <- data.frame(EntryType = shot_percentages_df$Var1, ShotPercentage = shot_percentages_df$Freq)
    
    ##### ENTRIES LEADING TO GOALS #####
    # Filter the data and only keep rows where FinalPlay is SGF, SGR, or G
    goal_data <- data[data$FinalPlay %in% c("G"), ]
    
    # Calculate the count of each entry type in the filtered data
    goal_counts <- table(goal_data$EntryType)
    
    # Calculate the percentage of each entry type relative to the total entries
    goal_percentages <- (goal_counts / total_entries) * 100
    
    # Convert the table to a data frame
    goal_percentages_df <- as.data.frame(goal_percentages)
    
    # Check if 'chip' is present in the data frame
    if (!'chip' %in% goal_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "chip", Freq = 0))
    }
    # Check if 'dump' is present in the data frame
    if (!'dump' %in% goal_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "dump", Freq = 0))
    }
    # Check if 'carry' is present in the data frame
    if (!'carry' %in% goal_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "carry", Freq = 0))
    }
    # Check if 'pass' is present in the data frame
    if (!'pass' %in% goal_percentages_df$Var1) {
      # If not present, add it with a frequency of 0
      goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "pass", Freq = 0))
    }
    
    # Convert entry_percentages data to a data frame for plotting
    goal_type_data <- data.frame(EntryType = goal_percentages_df$Var1, ZGoalPercentage = goal_percentages_df$Freq)
    
    
    # Merge the three data frames based on 'EntryType'
    total_data <- merge(entry_type_data, shot_type_data, by = "EntryType")
    total_data <- merge(total_data, goal_type_data, by = "EntryType")
    
    total_data_long <- tidyr::pivot_longer(total_data, cols = c("EntryPercentage.Freq", "ShotPercentage", "ZGoalPercentage"),
                                           names_to = "Category", values_to = "Percentage")
    
    # Custom colors
    all_entries_color <- "#1f77b4"   # Replace with the desired color code for "All entries"
      shots_on_goal_color <- "#ff7f0e" # Replace with the desired color code for "Shots on Goal"
        goal_color <- "#2ca02c"   # Replace with the desired color code for "Goals"
          
        # Plot the bar graph
        entry_type_bar_plot <- ggplot(total_data_long, aes(x = EntryType, y = Percentage, fill = Category)) +
          geom_bar(stat = "identity", position = "dodge", color = "black") +
          labs(title = "Entry Outcomes",
               x = "Entry Type",
               y = "Percentage",
               fill = "Category") +
          scale_fill_manual(values = c("EntryPercentage.Freq" = all_entries_color, "ShotPercentage" = shots_on_goal_color, "ZGoalPercentage" = goal_color),
                            name = "Category",
                            labels = c("All entries", "Shots on Goal", "Goal")) +
          theme_minimal() + 
          theme(      
            plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
          )
        
        hockey_rink_plot <- nhl_full_rink_plot()

        #####  Entry Type By Period   #####
        # Calculate the count of each entry type for each period
        entry_counts_per_period <- table(data$EntryType, data$Period)
        
        # Calculate the percentage of each entry type for each period relative to the total entries in that period
        entry_percentages_per_period <- prop.table(entry_counts_per_period, margin = 2) * 100
        
        # Convert entry_percentages_per_period data to a data frame for plotting
        entry_type_period_data <- as.data.frame(entry_percentages_per_period)
        
        # Rename the columns for better visualization
        colnames(entry_type_period_data) <- c("EntryType", "Period", "Percentage")
        
        entry_type_period_data$EntryType <- gsub("chip", "zchip", entry_type_period_data$EntryType)
        
        # Plot the bar graph for entry outcomes with periods on the x-axis
        entry_type_period_bar_plot <- ggplot(entry_type_period_data, aes(x = Period, y = Percentage, fill = EntryType)) +
          geom_bar(stat = "identity", position = "dodge", color = "black") +
          labs(title = "Entry Type by Period",
               x = "Period",
               y = "Percentage",
               fill = "Entry Type") +
          scale_fill_manual(values = c("zchip" = "#FF5733", "dump" = "#33FFC1", "carry" = "#FFD933", "pass" = "#3366FF"),
                            labels = c("zchip" = "chip"),
                            name = "Entry Type") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
          )
        
        #####  Entries By Period  #####
        # Calculate the count of each entry type for each outcome (G, SGF, SGR)
        entry_counts_per_outcome <- table(data$Period, data$FinalPlay)
        
        # If the data doesn't contain some outcome in any period, add a column of zeros
        if (!("G" %in% colnames(entry_counts_per_outcome))) {
          entry_counts_per_outcome <- cbind(entry_counts_per_outcome, G = 0)
        }
        if (!("SGF" %in% colnames(entry_counts_per_outcome))) {
          entry_counts_per_outcome <- cbind(entry_counts_per_outcome, SGF = 0)
        }
        if (!("SGR" %in% colnames(entry_counts_per_outcome))) {
          entry_counts_per_outcome <- cbind(entry_counts_per_outcome, SGR = 0)
        }
        
        # Sum of all entry counts for each period
        all_entries_totals <- rowSums(entry_counts_per_outcome)
        
        # Calculate the total sum of all entries
        total_sum <- sum(all_entries_totals)
        
        # Calculate the percentage of each entry type for each period
        all_entries_totals <- all_entries_totals / total_sum * 100
        
        # Filter the data to include only "G", "SGF", and "SGR" outcomes
        
        new_entry_counts_per_outcome <- entry_counts_per_outcome[, c("G", "SGF", "SGR")]
        
        if (!is.null(nrow(new_entry_counts_per_outcome))){
          shots_on_goal_totals <- rowSums(new_entry_counts_per_outcome)
        } else{
          shots_on_goal_totals <- sum(new_entry_counts_per_outcome)
        }
        shots_on_goal_totals <- shots_on_goal_totals / total_sum *100
        
        # Filter the data to include only "G" outcomes
        
        if (!is.null(nrow(new_entry_counts_per_outcome))){
          goal_totals <- new_entry_counts_per_outcome[, c("G")]
        } else{
          goal_totals <- new_entry_counts_per_outcome["G"]
        }
        goal_totals <- goal_totals / total_sum *100
      
        if (length(unique(data$Period)) == 1)
        {
          per = unique(data$Period)
        } else {
          per = c(unique(data$Period))
        }
        # Create a data frame with the desired format
        combined_totals <- data.frame(Period = per, ALL = all_entries_totals, SOG = shots_on_goal_totals, ZG = goal_totals)
        reshaped_combined_totals <- gather(combined_totals, EntryType, EntryCount, -Period)
        
        # Plot the bar graph for entry outcomes with outcomes on the x-axis
        entry_type_outcome_bar_plot <- ggplot(reshaped_combined_totals, aes(x = Period, y = EntryCount, fill = EntryType)) +
          geom_bar(stat = "identity", position = "dodge", color = "black") +          
          labs(title = "Entries by Period",
               x = "Period",
               y = "Count",
               fill = "Entry Type") +
          scale_fill_manual(values = c("ALL" = "#FF5733", "SOG" = "#3366FF", "ZG" = "#33CC33"),
                            name = "Entry Type",
                            labels = c("All Entries", "Shots on Goal", "Goals")) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
          ) +
          scale_x_continuous(breaks = per)
        
        
        # Combine the two bar charts in a grid layout
        row2_plot <- grid.arrange(entry_type_bar_plot, entry_type_outcome_bar_plot, ncol = 2)
        
        # Then, arrange the first plot (hockey_rink_plot) alone in the first row along with the second row of plots
        end_plot <- grid.arrange(hockey_rink_plot, row2_plot, entry_type_period_bar_plot, ncol = 1, heights = c(1, 1, 1))
        