## Built and run the model

## As seen in the EDA there is linear relationship among the numeric variables selected. Some of them are linear dependent on other, what should be seen in our model.

library(MASS)

## Load the train data:

xtrain <- read.csv("data/xtrain.csv")
ytrain <- read.csv("data/ytrain.csv")

train  <- cbind(ytrain, xtrain)
names(train)  <- c("tip_amount",names(xtrain))

## 1. multilineal regression model

## 1.1 model: use all the variables except the total amount that depends on the dependent variable 'tip'.

train <- train[, -4]

lm  <- lm(tip_amount~. ,data=train)
 
##confidence  <- confint(lm)

## coefficients of the important variables

lm$coefficients

## 1.2 test

xtest  <- read.csv("data/xtest.csv")
xtest <- xtest[,-3] ## remove the total amount
ytest  <- read.csv("data/ytest.csv")

predictions  <- predict(lm, xtest, interval="prediction")

results  <- cbind(ytest, predictions)

## Normal distribution of residuals.

histogram(lm$residuals)

## RMSE
RMSE_lm <- sqrt(mean((results$x - results$fit)^2))

## 2. Random forest

library(randomForest)

## 2.1 model

rf_model <- randomForest(tip_amount ~., data = train,  importance = TRUE, ntree=300)
##saveRDS(rf_model, file = "output/rf_model.rds")
 
importance(rf_model)[,1]

## 2.2 test

rf_predict <- predict(rf_model,xtest)

results  <- cbind(ytest, rf_predict)

plot(results$x~results$rf_predict, xlab='predicted', ylab='test')
abline(0,1, col='red')

## RMSE

RMSE_rf <- sqrt(mean((results$x-results$rf_predict)^2))


##

err  <- round((abs(predictions_rf$x-predictions_rf$rf_predict)/predictions_rf$x)*100, digits=1)
histogram(err)
