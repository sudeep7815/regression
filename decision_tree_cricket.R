library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)



dataset <- tibble(
  Outlook = factor(c('sunny', 'sunny', 'overcast', 'rainy', 'rainy', 'rainy', 'overcast', 'sunny', 'sunny', 'rainy', 'sunny', 'overcast', 'overcast', 'rainy', 'sunny', 'overcast', 'rainy', 'sunny', 'sunny', 'rainy', 'overcast', 'rainy', 'sunny', 'overcast', 'sunny', 'overcast', 'rainy', 'overcast')),
  Temperature = c(85.0, 80.0, 83.0, 70.0, 68.0, 65.0, 64.0, 72.0, 69.0, 75.0, 75.0, 72.0, 81.0, 71.0, 81.0, 74.0, 76.0, 78.0, 82.0, 67.0, 85.0, 73.0, 88.0, 77.0, 79.0, 80.0, 66.0, 84.0),
  Humidity = c(85.0, 90.0, 78.0, 96.0, 80.0, 70.0, 65.0, 95.0, 70.0, 80.0, 70.0, 90.0, 75.0, 80.0, 88.0, 92.0, 85.0, 75.0, 92.0, 90.0, 85.0, 88.0, 65.0, 70.0, 60.0, 95.0, 70.0, 78.0),
  Wind = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
  Cricket = factor(c('No', 'No', 'Yes', 'Yes', 'Yes', 'No', 'Yes', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'No', 'No', 'Yes', 'Yes', 'No', 'No', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'No', 'Yes'))
)


# Step 2: Train a Decision Tree
tree_model <- rpart(Cricket ~ Outlook + Temperature + Humidity + Wind, data = dataset, 
                    method = "class", 
                    control = rpart.control(minsplit = 2, cp = 0.01, maxdepth = 3))

# Step 3: Plot the tree
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree for Playing")


# Train the Random Forest model
rf_model <- randomForest(
  Cricket ~ Outlook + Temperature + Humidity + Wind,
  data = dataset,
  ntree = 100,        # Number of trees
  mtry = 2,           # Number of variables randomly sampled at each split
  importance = TRUE   # Track variable importance
)

# View model summary
print(rf_model)

# Variable Importance Plot
varImpPlot(rf_model)


new_data <- data.frame(
  Outlook = factor("sunny", levels = levels(dataset$Outlook)),
  Temperature = 72,
  Humidity = 90,
  Wind = TRUE
)

# Step 2: Predict using the trained model
rf_prediction <- predict(rf_model, newdata = new_data, type = "class")

tree_model_prediction <- predict(tree_model, newdata = new_data, type = "class")

# Step 3: Print prediction result
print(rf_prediction)


