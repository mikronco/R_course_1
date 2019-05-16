# CV for time series analysis  

library(caret)
library(lubridate)
library(randomForest)


economics <- ggplot2::economics

head(economics)
names(economics)

plot(economics$pce,economics$unemploy)
plot(economics$date,economics$unemploy)

# percentage of unemployed population from 1967 to 2015

plot(economics$date, economics$unemploy / economics$pop)

# objective: construct a model to forecast unemployment in the next 12 months 

# transform dates in months, i.e. register month only for each date in economics  

economics$month <- as.factor(month(economics$date))

# sampling 432 seeds of length 5, filled with integers up to 1000

set.seed(123)
seeds <- vector(mode = "list", length = 527)
for(i in 1:527) seeds[[i]] <- sample.int(1000, 5)
seeds[[528]] <- sample.int(1000, 1)

#  set up the computer for parallel processing

library(doParallel)
registerDoParallel(cores=2)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              seeds = seeds)


# we construct 5 different models, for those that require hyperparameter tuning we do 5 repetitions 

tuneLength.num <- 5

glmnet.mod <- train(unemploy ~ . - date,
                    data = economics,
                    method = "glmnet",
                    family = "gaussian",
                    trControl = myTimeControl,
                    tuneLength=tuneLength.num)

pois.mod <- train(unemploy ~ . - date,
                  data = economics,
                  method = "glmnet",
                  family = "poisson",
                  trControl = myTimeControl,
                  tuneLength=tuneLength.num)

lm.mod <- train(unemploy ~ . - date,
                data = economics,
                method = "lm",
                trControl = myTimeControl,
                tuneLength=tuneLength.num)

rpart.mod <- train(unemploy ~ . - date,
                   data = economics,
                   method = "rpart",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

# cross validation 

resamps <- resamples(list(glmnet = glmnet.mod,
                          glmnet.pois = pois.mod,
                          lm = lm.mod,
                          rpart=rpart.mod))
resamps

ss <- summary(resamps)

knitr::kable(ss[[3]]$Rsquared)

library(lattice)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Rsquared")

library(earth)

plotmo(lm.mod$finalModel)


