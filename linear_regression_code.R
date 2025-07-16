library(tidyverse)
library(broom) #for model summary
library(modelr) #for model metrics

#create data
data <- tibble(
  ads = c(10,15,20,25),
  sales = c(100,130,190,233)
)

#fit linear model
model<-lm(sales ~ ads,data = data)

#tidy model summary
model %>% tidy()
model %>% glance()

#predict and calculate residuals

data_with_preds <- data %>% 
  add_predictions(model) %>% 
  mutate(residual = sales - pred)

# calculate mse
mse <- data_with_preds %>% 
  summarise(mse = mean(residual^2))

print(mse)

#predict new  values

new_data <- tibble(ads = c(12,18))

predictions <- predict(model,newdata = new_data)

pr_df <- bind_cols(new_data,predicted_sales = predictions)


#plot

data %>% 
  ggplot(aes(x = ads,y = sales))+
  geom_point(color = "blue",size = 3)+
  geom_smooth(method = "lm",se = FALSE,color = "red")+
  geom_point(data = pr_df,aes(x = ads, y = predicted_sales),size = 5,color = "green")+
  labs(
    title = "Linear Regression Example(Tidyverse)",
    x = "Ad Budget",
    y = "Sales"
  )






