library(broom)
library(modelr)
library(tidyverse)
library(plotly)


value <- read.csv("10_Property_stolen_and_recovered.csv") 
colnames(value)

model <- lm(Value_of_Property_Recovered ~ Value_of_Property_Stolen, data = value)
#Tidy model summary
model %>% tidy()
model %>% glance()

value_with_preds <- cases %>% 
  add_predictions(model) %>% 
  mutate(residual = Value_of_Property_Recovered - pred)

mse <- value_with_preds %>% 
  summarise(mse = mean(residual^2))

print(mse)

new_data <- tibble(Value_of_Property_Stolen = c(12,18))

predictions <- predict(model, newdata = new_data)

pr_df <- bind_cols(new_data, predicted_recovery = predictions)

#plot
cases %>% 
  ggplot(aes(Value_of_Property_Stolen, Value_of_Property_Recovered , color = Area_Name))+
  geom_point( size = 4)+
  geom_smooth(method = "lm", se = F, color = "pink")+
  geom_point(data = pr_df, aes(Value_of_Property_Stolen, predicted_recovery), size= 5, color = "red")+
  labs(
    title = "Linear Regreesion Example(Tidyverse)",
    x= "Properties stolen",
    y= "Properties recovered"
    
  ) -> lp


ggplotly(lp)

