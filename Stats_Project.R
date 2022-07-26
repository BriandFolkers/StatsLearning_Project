###libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(FNN)
library(tidyr)
library(readr)
library(class)


###data wrangling
train <- select(training_Upsampled_data, -c(X1, Liability.Assets.Flag)) %>% 
  rename(Bankrupt = Bankrupt.) %>% 
  mutate(across(c(Bankrupt),factor)) %>% na.omit()

test <- select(test_original, -c(X1, Liability.Assets.Flag)) %>% 
  rename(Bankrupt = Bankrupt.) %>% 
  mutate(across(c(Bankrupt),factor)) %>% na.omit()






### Logistic Regression
#logistic regression model with all variables
glm <- glm(Bankrupt~ ., data = train, family = "binomial")
summary(glm)

#glm$fitted.values






### KNN Classification



##Elbow Method for finding the optimal number of clusters
set.seed(1)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




### KNN Classification
#set seed
set.seed(1)

#perform K-nearest neighbor
#K = 1
my_knn <- knn(train, test, train$Bankrupt, k = 5)

table_knn <- table(my_knn, test$Bankrupt)

accuracy_test <- sum(diag(table_knn))/ sum(table_knn)

accuracy_test
#plot the accuracy for different values of K
M = 150 # the number of possible values of K
K <- seq(1, M)
accuracy_test <- rep(0,M)
for (i in 1:M)
{
  my_knn <- knn(train, test, train$Bankrupt, k = K[i])
  table_knn <- table(my_knn, test$Bankrupt)
  accuracy_test[i] <- sum(diag(table_knn))/ sum(table_knn)
}
max(accuracy_test)
accuracy_test <- as.data.frame(accuracy_test)
ggplot(accuracy_test, 
       aes(x = K, y = accuracy_test)) + 
  geom_line(col = "red")



