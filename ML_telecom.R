library(C50)
data(churn)


str(churnTrain)
str(churnTest)

predictors <- names(churnTrain)[names(churnTrain)!="churn"]
set.seed(1)

library(caret)
library(rpart)
library(randomForest)

inTrain <- createDataPartition(y=churnTrain$churn,p=0.75,list=FALSE)
subtrain <- churnTrain[inTrain,]
subtest <- churnTrain[-inTrain,]

# brute force ML, without data preprocessing, accuracy 92-93 % 

RPmdl <- rpart(churn ~ ., data=subtrain, method="class")
pred1 <- predict(RPmdl, subtest, type = "class")
qplot(churn, pred1, data=subtest, geom = c("boxplot", "jitter"), main = "predicted vs. observed in validation data", xlab = "True Customers", ylab = "Predicted Customers")
confusionMatrix(pred1,subtest$churn)

RFmdl <- randomForest(churn ~ ., data=subtrain, method="class")
pred2 <- predict(RFmdl, subtest, type = "class")
qplot(churn, pred2, data=subtest, geom = c("boxplot", "jitter"), main = "predicted vs. observed in validation data",xlab = "True Customers", ylab = "Predicted Customers")
confusionMatrix(pred2,subtest$churn)


prediction <- predict(RFmdl, churnTest, type="class")
confusionMatrix(prediction,churnTest$churn)


# let's preprocess data 

numerics <- c("account_length", "total_day_calls", "total_night_calls")

# find means and stds 

procValues <- preProcess(churnTrain[,numerics],method = c("center","scale","YeoJohnson"))

# use predicted methods to make adjustements: centered, scaled and transformed

trainScaled <- predict(procValues, churnTrain[,numerics])
testScaled <- predict(procValues, churnTest[,numerics])

# use a Boosted Tree algorithm for training

library(gbm)

# turn char response into numerical 

forGBM <- churnTrain
forGBM$churn <- ifelse(forGBM$churn == "yes", 1, 0)

# fit boosted tree model 

gbmFit <- gbm(formula = churn ~ ., distribution = "bernoulli", data = forGBM, n.trees = 2000, interaction.depth = 7, shrinkage = 0.01, verbose = FALSE)

# in order to make the best choice of the hyperparameters we can use train to perform resampling 

gbmTune <- train(x = churnTrain[,predictors], y = churnTrain$churn, method = "gbm")

# grid <- expand.grid(.interaction.depth = seq(1, 7, by = 2), .n.trees = seq(100, 1000, by = 50), .shrinkage = c(0.01, 0.1))
ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
gbmTune <- train(x = churnTrain[,predictors], y = churnTrain$churn, method = "gbm", metric = "ROC", trControl = ctrl)


