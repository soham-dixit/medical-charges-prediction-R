setwd("D:/Degree/SEM4/DS CP")

library(dplyr)          # data wrangling
library(ggplot2)        # graphing
library(caret)          # machine learning functions
library(MLmetrics)      # machine learning metrics
library(car)            # VIF calculation
library(lmtest)         # linear regression model testing
library(GGally)         # correlation plot
library(jsonlite)

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

ggplot(data = train, aes(x = charges, fill = smoker)) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Distribution of Charges per Smoking Category")

# analyze the medical charges by age, bmi and children according to the smoker factor.

for (feat in c('age', 'bmi', 'children')) {
  plot <- ggplot(data = train, aes_string(x = feat, y = 'charges', group = 'smoker', fill = 'smoker', col = 'smoker')) + 
    geom_jitter() + 
    geom_smooth(method = 'lm') +
    ggtitle(glue::glue("Charges vs {feat}"))  
  print(plot)
}

#correlations between features as follows

ggcorr(train %>% mutate_if(is.factor, as.numeric), label = TRUE)

true <- 10000
pred <- seq(from = 1000, to = 19000, length.out = 181)
x <- pred - true
rmse <- (x ^ 2) ^ 0.5
rmsle <- ((log(pred) - log(true)) ^ 2) ^ 0.5

par(mfrow = c(1, 2))
plot(x = x, y = rmse, 
     type = "l", 
     main = "Root Mean Squared Error", 
     xlab = "Error (prediction - actual)", ylab = "RMSE")
plot(x = x, y = rmsle, 
     type = "l", 
     main = "Root Mean Squared Logarithmic Error", 
     xlab = "Error (prediction - actual)", ylab = "RMSLE")


# Linear regression

temp <- lm(charges ~ ., data = train)
step(temp)

lm_all <- lm(formula = charges ~ age + bmi + children + smoker, data = train)
y_pred <- predict(lm_all, test)
mae <- MAE(y_pred, test$charges)
rmse <- RMSE(y_pred, test$charges)

y_pred[y_pred <=0]

eps <- 1e-10 # small constant value
rmsle <- RMSLE(pmax(y_pred[-149], eps), test$charges[-149])
lin_reg <- cbind("MAE" = mae, "RMSE" = rmse, "RMSLE" = rmsle)
lin_reg
summary(lm_all)

accuracy_mae <- (1 - (mae / mean(test$charges))) * 100
accuracy_rmse <- (1 - (rmse / mean(test$charges))) * 100

# print results
cat("Accuracy (MAE):", round(accuracy_mae, 2), "%\n")
cat("Accuracy (RMSE):", round(accuracy_rmse, 2), "%\n")


# Save the model to a file
saveRDS(lm_all, file = "medical_charges_model.rds")