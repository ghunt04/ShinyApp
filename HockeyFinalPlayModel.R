# Load required libraries
library(rpart)

hockey_data <- read.csv("HockeyData.csv")

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # For reproducibility
index <- sample(1:nrow(hockey_data), 0.8 * nrow(hockey_data))
train_data <- hockey_data[index, ]
test_data <- hockey_data[-index, ]

# Create the linear regression model
model <- lm(FinalPlay ~ Entry + Period + Name + Number + EntryType + Quadrant + TimeRem + PassComplete, data = train_data)

# Make predictions on the test set
predictions <- predict(model, test_data)

# Convert predicted values to binary based on a threshold (e.g., 0.5)
threshold <- 0.5
predicted_classes <- ifelse(predictions >= threshold, "Win", "Lose")

# Confusion matrix
conf_matrix <- table(predicted_classes, test_data$FinalPlay)
print(conf_matrix)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))
