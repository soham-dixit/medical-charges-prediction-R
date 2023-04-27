setwd("D:/Degree/SEM4/DS CP")
library(caret)
library(dplyr)
library(GGally)  

train <- read.csv("train.csv", stringsAsFactors=TRUE)
test <- read.csv("test.csv", stringsAsFactors=TRUE)
glimpse(train)

train[duplicated(train), ] # check duplicates in train dataset


colSums(is.na(train)) # check missing values
colSums(is.na(test))  # check missing values

summary(train)

ggplot(data = train, aes(x = charges)) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Distribution of Charges")


for (col in c('gender', 'region', 'children', 'smoker')) {
  plot <- ggplot(data = train,
                 aes_string(x = col, y = 'charges', group = col, fill = col)) + 
    geom_boxplot(show.legend = FALSE) + 
    ggtitle(glue::glue("Boxplot of Medical Charges per {col}"))
  print(plot)
}

#correlations between features as follows

ggcorr(train %>% mutate_if(is.factor, as.numeric), label = TRUE)

model1 <- train(charges ~ ., data = train, method = "lm")
model2 <- train(charges ~ ., data = train, method = "svmRadial")
model3 <- train(charges ~ ., data = train, method = "rpart")

lm_pred = predict(model1, newdata=test)
svm_pred = predict(model2, newdata = test)
rpart_pred = predict(model3, newdata = test)


# Combine predictions using weighted average
ensemble_pred <- (0.5 * lm_pred) + (0.5 * svm_pred)

# Calculate RMSE of ensemble predictions
rmse <- sqrt(mean((test$charges - ensemble_pred)^2))
cat(sprintf("Ensemble RMSE: %.3f\n", rmse))

model_list <- list(model1 = model1, model2 = model2)
saveRDS(model_list, file = "ensemble_model.rds")

