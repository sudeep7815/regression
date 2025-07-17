library(tidyverse)
library(janitor)
library(plotly)


data <- data.frame(
  age = sample(20:60, 100, replace = TRUE),
  income = sample(20000:100000, 100, replace = TRUE)
)


kmeans_model <- kmeans(data, centers = 3)  # 3 clusters

data$cluster <- as.factor(kmeans_model$cluster)

ggplot(data, aes(x = age, y = income, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "k-Means Clustering (k = 3)") +
  theme_minimal()


kmeans_model <- kmeans(data, centers = 3, nstart = 25, iter.max = 50)  # 3 clusters

kmeans_model$iter

data$cluster <- as.factor(kmeans_model$cluster)

ggplot(data, aes(x = age, y = income, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "k-Means Clustering (k = 3)") +
  theme_minimal()


kmeans_model$tot.withinss

# Compute WCSS for k = 1 to 10 within-cluster sum of squares (WCSS) 
# Pre-allocate a vector to store WCSS values
wcss <- numeric(0)

# Loop over k from 1 to 10
for (k in 1:10) {
  km_model <- kmeans(data[, c("age", "income")], centers = k)
  wcss[k] <- km_model$tot.withinss
}

wcss_df<- tibble(k = 1:10,
                 wcss = wcss)

ggplot(wcss_df, aes(x = k, y = wcss)) +
  geom_line(color = "steelblue") +
  geom_point(size = 2) +
  labs(title = "Elbow Method to Choose Optimal k",
       x = "Number of Clusters (k)",
       y = "WCSS (Within-Cluster Sum of Squares)") +
  theme_minimal()

#Mall data 2D

"Mall_Customers_spendings.csv" %>% 
  read_csv() %>% 
  clean_names() -> malldf

mall_model <- kmeans(malldf[, c("annual_income_k", "spending_score_1_100")], centers = 5)

malldf$cluster <- as.factor(mall_model$cluster)

ggplot(malldf, aes(x = annual_income_k, y = spending_score_1_100, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "k-Means Clustering (k = 5)") +
  theme_minimal()

wcss <- numeric(0)

# Loop over k from 1 to 10
for (k in 1:10) {
  km_model <- kmeans(malldf[, c("annual_income_k", "spending_score_1_100")], centers = k)
  wcss[k] <- km_model$tot.withinss
}

ggplot(wcss_df, aes(x = k, y = wcss)) +
  geom_line(color = "steelblue") +
  geom_point(size = 2) +
  labs(title = "Elbow Method to Choose Optimal k",
       x = "Number of Clusters (k)",
       y = "WCSS (Within-Cluster Sum of Squares)") +
  theme_minimal()


#Mall data 3D

"Mall_Customers_spendings.csv" %>% 
  read_csv() %>% 
  clean_names() -> malldf

mall_model <- kmeans(malldf[, c("age","annual_income_k", "spending_score_1_100")], centers = 5)

malldf$cluster <- as.factor(mall_model$cluster)


plot_ly(malldf, x = ~age, y = ~annual_income_k, z = ~spending_score_1_100,
        color = ~cluster, colors = c('red', 'green', 'blue'),
        type = "scatter3d",
        mode = "markers") %>%
  layout(title = "3D k-Means Clustering",
         scene = list(
           xaxis = list(title = "Age"),
           yaxis = list(title = "Income"),
           zaxis = list(title = "Spending Score")
         ))

ggplot(malldf, aes(x = annual_income_k, y = spending_score_1_100, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "k-Means Clustering (k = 5)") +
  theme_minimal()

wcss <- numeric(0)

# # Loop over k from 1 to 10
# for (k in 1:10) {
  km_model <- kmeans(malldf[, c("annual_income_k", "spending_score_1_100")], centers = k)
   wcss[k] <- km_model$tot.withinss
 }
# 
# ggplot(wcss_df, aes(x = k, y = wcss)) +
#   geom_line(color = "steelblue") +
#   geom_point(size = 2) +
#   labs(title = "Elbow Method to Choose Optimal k",
#        x = "Number of Clusters (k)",
#        y = "WCSS (Within-Cluster Sum of Squares)") +
#   theme_minimal()



