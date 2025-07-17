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
