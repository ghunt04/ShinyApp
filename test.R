library(rpart)
library(recipes)
library(caret)

hockey_data <- read.csv("HockeyData.csv")
# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # For reproducibility
index <- caret::createDataPartition(hockey_data$FinalPlay, p = 0.8, list = FALSE)
train_data <- hockey_data[index, ]
test_data <- hockey_data[-index, ]

# Create the decision tree model
model <- rpart(FinalPlay ~ Entry + Period + Name + Number + EntryType + Quadrant + TimeRem + PassComplete, data = train_data)

# Make predictions on the test set
predictions <- predict(model, test_data, type = "class")

# Confusion matrix
conf_matrix <- table(predictions, test_data$FinalPlay)
print(conf_matrix)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

