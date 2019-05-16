library(ISLR)
library(boot)
set.seed(1)

# randomly select a subset of 196 obs from the data set of 392 obs in total 

train1 = sample(392,196)

set.seed(2)

train2= sample(392,196)

# linear fit

fit1l = lm(mpg ~ horsepower, data = Auto, subset = train1 )
fit2l = lm(mpg ~ horsepower, data = Auto, subset = train2 )

fit1q = lm(mpg ~ poly(horsepower,2), data = Auto, subset = train1 )
fit2q = lm(mpg ~ poly(horsepower,2), data = Auto, subset = train2 )

fit1c = lm(mpg ~  poly(horsepower,3), data = Auto, subset = train1 )
fit2c = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train2 )


# estimate the MSE on test set: it changes for different resampling of the training set, and also depends on the model (linear, quadratic, cubic)

attach(Auto)
MSE1l = mean((mpg - predict(fit1l,Auto))[-train1]^2)
MSE2l = mean((mpg - predict(fit2l,Auto))[-train2]^2)

MSE1q = mean((mpg - predict(fit1q,Auto))[-train1]^2)
MSE2q = mean((mpg - predict(fit2q,Auto))[-train2]^2)

MSE1c = mean((mpg - predict(fit1c,Auto))[-train1]^2)
MSE2c = mean((mpg - predict(fit2c,Auto))[-train2]^2)

# we can use cv.glm() function to perform cross validation and choose best model 

# LOOCV: 

cverr = rep(0,5)

for(i in 1:5){
  
  glmfit = glm(mpg~poly(horsepower,i),data = Auto)
  cverr[i] = cv.glm(Auto,glmfit)$delta[1]

}

plot(1:5,cverr,type = "l", xlab = "order", ylab="MSE")

# k-fold CV:

set.seed(3)

cverrk = rep(0,10)

for(i in 1:10){
  
  glmfit = glm(mpg~poly(horsepower,i),data = Auto)
  cverrk[i] = cv.glm(Auto,glmfit,K=10)$delta[1]
  
}

plot(1:10,cverrk,type = "l", xlab = "order", ylab="MSE")

# BOOTSTRAP: 

# Ex1: estimate alpha in the Portfolio dataset 

alpha = function(data,index){
  
  X = data$X[index]
  Y = data$Y[index]
  return (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
  
}

# we can estimate alpha using bootstrap (i.e. a sampled subset of data with replacement)

set.seed(1)
alphab = alpha(Portfolio,sample(100,100,replace = T))
alphat = alpha(Portfolio,1:100)

# let's perform bootstrap analysis by performing the above steps 1000 times using the function boot()

boot(Portfolio, alpha, R = 1000)

# Ex2: estimate slope and intercept of mpg as function of horsepower in the Auto dataset 

bfun = function(data,index){
  
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))

}

set.seed(7)
boot(Auto,bfun,R=1000)
