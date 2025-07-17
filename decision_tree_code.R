# Load libraries
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)



#Iris data set
iris %>% slice(sample(1:150, 130)) -> train_set

iris %>% slice(sample(1:150, 10)) -> test_set 

species_model <- rpart(Species ~ ., 
                       data = train_set,
                       method = "class",
                       control = rpart.control(minsplit = 2, cp = 0.01, maxdepth = 3))
species_model

rpart.plot(species_model)

varImp(species_model)  %>%
  arrange(Overall) -> importances


preds <- predict(species_model, newdata = test_set, type = "class")
preds

confusionMatrix(test_set$Species, preds)

# Sample data with 8 rows (just enough for 6 nodes)

data <- tibble(
  age = sample(20:50, 30, replace = TRUE),
  income = sample(seq(20000, 100000, by = 5000), 30, replace = TRUE),
  city = factor(sample(c("Delhi", "Mumbai", "Chennai", "Bangalore"), 30, replace = TRUE)),
  gender = factor(sample(c("Male", "Female"), 30, replace = TRUE)),
  bought = factor(sample(c(0, 1), 30, replace = TRUE))
)

# Fit the classification tree using all predictors
#numeric: method = "anova"
tree_model <- rpart(
  bought ~ .,
  data = data,
  method = "class",
  control = rpart.control(minsplit = 2, cp = 0.01, maxdepth = 10)
)
printcp(tree_model)

varImp(tree_model)  %>%
  arrange((Overall)) -> importances

# Plot the decision tree
rpart.plot(tree_model, main = "Decision Tree with City and Gender")


