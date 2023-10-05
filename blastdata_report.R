library(ggplot2)
library(gridExtra)
library(plotly)
library(corrplot)

data <- read.csv("BlastData.csv")
#data <- data[data$Player == "Ryan Arena",]
#data <- data[data$Player == "Alex Reynolds",]
# Convert columns to appropriate data types
data$Player <- as.factor(data$Player)
data$Swing <- as.integer(data$Swing)
data$Bat_Speed <- as.numeric(data$Bat.Speed..mph.)
data$Attack_Angle <- as.numeric(data$Attack.Angle..deg.)
data$Time_to_Contact <- as.numeric(data$Time.to.Contact..sec.)
data$Peak_Hand_Speed <- as.numeric(data$Peak.Hand.Speed..mph.)
data$Blast_Factor <- as.numeric(data$Blast.Factor)
data$On_Plane <- as.numeric(data$On.Plane....)
data$Power <- as.numeric(data$Power..kW.)
data$Peak_Bat_Speed <- as.numeric(data$Peak.Bat.Speed..mph.)
data$Body_Rotation <- as.numeric(data$Body.Rotation....)
data$Vertical_Bat_Angle <- as.numeric(data$Vertical.Bat.Angle..deg.)
data$Woba <- as.numeric(data$Season.Woba)

data <- data[data$Bat_Speed > 50, ]

# Function to calculate and format correlation coefficient
get_corr_text <- function(x, y) {
  corr_value <- cor(x, y)
  corr_text <- paste("Correlation:", round(corr_value, 2))
  return(corr_text)
}

# Create a plot object for Bat_Speed vs. Power
plot1 <- plot(data$Bat_Speed, data$Power, pch = 16, axes = FALSE, ylim = c(ceiling((min(data$Power)-1)), ceiling((max(data$Power)+1))),
              xlab = "Bat Speed (mph)", ylab = "Power (kW)", main = "Bat Speed vs. Power and Peak Hand Speed")
points(data$Bat_Speed, data$Power, pch = 16, col = "black")
abline(lm(data$Power ~ data$Bat_Speed), lty = 2)  # Add dashed trendline
axis(2, ylim = c(ceiling((min(data$Power)-1)), ceiling((max(data$Power)+1))), col = "black", las = 1)
box()

# Add background lines
grid(col = "gray", lty = "dashed")

# Add a second plot on the same graph for Bat_Speed vs. Peak_Hand_Speed
par(new = TRUE)
plot(data$Bat_Speed, data$Peak_Hand_Speed, pch = 16, xlab = "", ylab = "", ylim = c((min(data$Peak_Hand_Speed)-(0.5*(min(data$Peak_Hand_Speed)))), (max(data$Peak_Hand_Speed)+1)),
     col = "red", yaxt = "n")
points(data$Bat_Speed, data$Peak_Hand_Speed, pch = 16, col = "red")
abline(lm(data$Peak_Hand_Speed ~ data$Bat_Speed), lty = 2, col = "red")  # Add dashed trendline
axis(4, ylim = c((min(data$Peak_Hand_Speed)-(0.5*(min(data$Peak_Hand_Speed)))), (max(data$Peak_Hand_Speed)+1)), col = "red", col.axis = "red", las = 1)
mtext("Peak Hand Speed (mph)", side = 4, col = "red", line = 2.5)

# Draw the Bat Speed axis
axis(1, pretty(range(data$Bat_Speed), 10))

# Add legend
legend("topleft", legend = c("Power", "Peak Hand Speed"),
       text.col = c("black", "red"), pch = 16, col = c("black", "red"))

# Add correlations
text(min(data$Bat_Speed)+(0.8*(max(data$Bat_Speed)-min(data$Bat_Speed))), (0.7*min(data$Peak_Hand_Speed)), get_corr_text(data$Bat_Speed, data$Power))
text(min(data$Bat_Speed)+(0.7*(max(data$Bat_Speed)-min(data$Bat_Speed))), max(data$Peak_Hand_Speed)+0.5, get_corr_text(data$Bat_Speed, data$Peak_Hand_Speed))

# Assign the plot to a variable
bat_speed_peak_hand_speed_power <- plot1

scatter_3d <- plot_ly(
  data,
  x = ~Bat_Speed,
  y = ~Power,
  z = ~Blast_Factor,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    color = ~Blast_Factor,
    colorscale = list(c(0, 1), c("blue", "red")),
    colorbar = list(title = "Blast Factor")
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Bat Speed (mph)"),
      yaxis = list(title = "Power (kW)"),
      zaxis = list(title = "Blast Factor")
    )
  )

scatter_3d


# Create correlation matrix
cor_matrix <- cor(data[, c("Bat_Speed", "Attack_Angle", "Time_to_Contact", "Peak_Hand_Speed", "Blast_Factor", "On_Plane", "Power", "Peak_Bat_Speed", "Body_Rotation", "Vertical_Bat_Angle")])

# Remove underscores from labels
colnames(cor_matrix) <- gsub("_", " ", colnames(cor_matrix))
rownames(cor_matrix) <- gsub("_", " ", rownames(cor_matrix))

# Create correlation matrix
corrplot(cor_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45,
         pch.col = "black", addCoef.col = "black", number.cex = 0.8, number.digits = 2,
         title = "Correlation Matrix", mar = c(0,0,1,0))



# Calculate average Blast Factor, Woba for each player
averages <- aggregate(cbind(Blast_Factor, Woba) ~ Player, data, function(x) mean(x, na.rm = TRUE))


averages <- averages[averages$Blast_Factor >= 75,]

# Fit a quadratic trendline
model <- glm(Woba ~ poly(Blast_Factor, 2, raw = TRUE), data = averages)

# Plot histogram with colors by Player and trendline
histogram <- ggplot(averages, aes(x = Blast_Factor, y = Woba)) +
  geom_bar(stat = "identity", width = 1.5, aes(fill = Player)) +
  labs(x = "Average Blast Factor", y = "Player Woba", title = "Average Blast Factor vs. Woba") +
  theme(plot.title = element_text(hjust = 0.5))

# Add quadratic trendline
histogram + geom_smooth(method = "glm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "black", linetype = "dashed") +
  annotate("text", x = min(averages$Blast_Factor) + 0.2 * diff(range(averages$Blast_Factor)), y = max(model$fitted.values),
           label = paste("y =", round(coef(model)[1], 2), "-", abs(round(coef(model)[2], 2)), "x +", round(coef(model)[3], 4), "x^2"))



