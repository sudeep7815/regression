
library(randomForest)



regression_data <- tibble(
  age = sample(20:60, 50, replace = TRUE),
  income = sample(seq(20000, 100000, by = 5000), 50, replace = TRUE),
  city = factor(sample(c("Delhi", "Mumbai", "Chennai", "Bangalore"), 50, replace = TRUE)),
  gender = factor(sample(c("Male", "Female"), 50, replace = TRUE)),
  spend = round(runif(50, min = 1000, max = 10000))  # numeric target
)


reg_tree <- rpart(spend ~ age + income + city + gender,
                  data = regression_data, 
                  method = "anova")


rf_reg <- randomForest(
  spend ~ age + income + city + gender,
  data = regression_data,
  ntree = 10,
  mtry = 2,
  importance = TRUE
)

print(rf_reg)



new_data <- tibble(
  age = c(30, 45),
  income = c(50000, 80000),
  city = factor(c("Delhi", "Chennai"), levels = levels(regression_data$city)),
  gender = factor(c("Female", "Male"), levels = levels(regression_data$gender))
)

predict(reg_tree, newdata = new_data)   # From decision tree
predict(rf_reg, newdata = new_data)   

importance(rf_reg)
# %IncMSE: How badly the model performs without that variable
# IncNodePurity: How much the variable helps in splitting the data cleanly

tree_df <- getTree(rf_reg, k = 1, labelVar = TRUE)

# View structure (data.frame of rules)
head(tree_df)




