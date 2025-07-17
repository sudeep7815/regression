library (tidyverse)
library(broom)
library(modelr)
library(janitor)

read_csv("sales_marketing_data.csv") -> data 
clean_names(data) -> data1
colnames(data1)
model <-lm (sales~tv+ direct + social_media ,data = sales_data)

model %>% tidy()
model %>% glance()

data1 %>% 
  ggplot(aes(x=tv+ direct + social_media, y = sales))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Linear Regression ",
    y= "sales"
  )

