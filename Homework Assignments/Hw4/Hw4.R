set.seed(1)
x1 <- rnorm(200)
x2 <- 4 * x1^2 + 1 + rnorm(200)
y <- as.factor(c(rep(1,100), rep(-1,100)))
x2[y==1] <- x2[y==1] + 3
x2[y==-1] <- x2[y==-1] - 3
plot(x1[y==1], x2[y==1], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x1[y==-1], x2[y==-1], col = "blue")
dat <- data.frame(x1,x2,y)

#2a
set.seed(1)
n <-nrow(dat)
train <-sample(1:n, 0.8 * n)
test <- sample(1:n, 0.2 * n)

#2b
library(e1071)
set.seed(1)
tune.out <- tune(svm,
              y~., data=dat[train,],
              kernel="linear",
              ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)
plot(tune.out$best.model, data = dat[train,])

set.seed(1)
tune.out.poly <- tune(svm,
                 y~., data=dat[train,],
                 kernel="poly",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                 degree=c(2,3)))
summary(tune.out.poly)
plot(tune.out.poly$best.model, data = dat[train,])

set.seed(1)
tune.out.rad <- tune(svm,
                      y~., data=dat[train,],
                      kernel="radial",
                      ranges=list(cost=c(0.1,1,10,100,1000),
                      gamma=c(0.5,1,2,3,4)))
summary(tune.out.rad)
plot(tune.out.rad$best.model, data = dat[train,])

#3b
mean(predict(tune.out$best.model) != y[train])
mean(predict(tune.out.poly$best.model) != y[train])
mean(predict(tune.out.rad$best.model) != y[train])

mean(predict(tune.out$best.model, newdata = dat[-train,]) != y[-train])
mean(predict(tune.out.poly$best.model, newdata = dat[-train,]) != y[-train])
mean(predict(tune.out.rad$best.model, newdata = dat[-train,]) != y[-train])

#4a
library(ISLR)
data("Auto")
Auto$mpg01 <- as.factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))
Auto$mpg <- NULL

#4b
set.seed(1)
tune.out <- tune(svm,
                 mpg01~., data = Auto,
                 kernel="linear",
                 ranges=list(cost=c(0.01,0.1,1,5,10,100)))
summary(tune.out)
print(tune.out$best.parameters)
mean(predict(tune.out$best.model) != Auto$mpg01)
print(tune.out$best.performance)

#4c
set.seed(1)
tune.out.poly <- tune(svm,
                 mpg01~., data = Auto,
                 kernel="poly",
                 ranges=list(cost=c(0.01,0.1,1,5,10,100),
                 degree = c(2,3,4)))
summary(tune.out.poly)
print(tune.out.poly$best.parameters)
mean(predict(tune.out.poly$best.model) != Auto$mpg01)
print(tune.out.poly$best.performance)

#4d
set.seed(1)
tune.out.rad <- tune(svm,
                      mpg01~., data = Auto,
                      kernel="radial",
                      ranges=list(cost=c(0.01,0.1,1,5,10,100),
                      gamma = c(0.01,0.1,1,5)))
summary(tune.out.rad)
print(tune.out.rad$best.parameters)
mean(predict(tune.out.rad$best.model) != Auto$mpg01)
print(tune.out.rad$best.performance)

#5a
library(ISLR)
set.seed(1)
dataOJ <- sample(1:nrow(OJ), 800)
trainOJ <- OJ[dataOJ,]
testOJ <- OJ[-dataOJ,]

#5b
set.seed(1)
tune.out <- tune(svm,
                 Purchase~., data = trainOJ,
                 kernel="linear",
                 ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10)))
summary(tune.out)
mean(predict(tune.out$best.model) != trainOJ$Purchase)
test.pred <- predict(tune.out$best.model,testOJ)
mean(testOJ$Purchase != test.pred)

#5c
set.seed(1)
tune.out.poly <- tune(svm,
                 Purchase~., data = trainOJ,
                 kernel="poly",
                 ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10),
                 degree = 3))
summary(tune.out.poly)
mean(predict(tune.out.poly$best.model) != trainOJ$Purchase)
test.pred <- predict(tune.out.poly$best.model,testOJ)
mean(testOJ$Purchase != test.pred)

#5d
set.seed(1)
tune.out.rad <- tune(svm,
                      Purchase~., data = trainOJ,
                      kernel="radial",
                      ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10),
                      gamma = 2))
summary(tune.out.rad)
mean(predict(tune.out.rad$best.model) != trainOJ$Purchase)
test.pred <- predict(tune.out.rad$best.model,testOJ)
mean(testOJ$Purchase != test.pred)

#6a
set.seed(1)
data.iris <- sample(nrow(iris), 0.8 * nrow(iris))
train.iris <- iris[data.iris,]
test.iris <- iris[-data.iris,]

#6b
set.seed(1)
tune.out <- tune(svm,
                 Species~., data = train.iris,
                 kernel="linear",
                 ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10)))
summary(tune.out)
print(tune.out$best.parameters)

set.seed(1)
tune.out.poly <- tune(svm,
                 Species~., data = train.iris,
                 kernel="poly",
                 ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10),
                 degree=c(2,3)))
summary(tune.out.poly)
print(tune.out.poly$best.parameters)

set.seed(1)
tune.out.rad <- tune(svm,
                      Species~., data = train.iris,
                      kernel="radial",
                      ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10),
                      gamma=c(0.25,0.5,1,2,3,4)))
summary(tune.out.rad)
print(tune.out.rad$best.parameters)

#6c
mean(predict(tune.out$best.model) != train.iris$Species)
mean(predict(tune.out.poly$best.model) != train.iris$Species)
mean(predict(tune.out.rad$best.model) != train.iris$Species)

test.pred <- predict(tune.out$best.model,test.iris)
mean(test.iris$Species != test.pred)
test.pred <- predict(tune.out.poly$best.model,test.iris)
mean(test.iris$Species != test.pred)
test.pred <- predict(tune.out.rad$best.model,test.iris)
mean(test.iris$Species != test.pred)

#2a
data("USArrests")
pr.out = prcomp(USArrests, scale = T)
pr.var=pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve

#2b
pr.out$rotation
USArrests.scale = scale(USArrests)
sumValue = colSums((USArrests.scale %*% pr.out$rotation)^2)
sumValue
sum(sumValue)
sumValue/sum(sumValue)

#7a
library(MASS)
data(Boston)
str(Boston)
Boston <- Boston[,-4]
str(Boston)

#7b
Boston <- Boston[,-13]
str(Boston)

#7c
pr <- prcomp(Boston, scale = T)
summary(pr)

#7d
pr.var <- pr$sdev^2
pr.var / sum(pr.var)

cpr <- cumsum(pr.var / sum(pr.var))
pc <- which(cpr > 0.8)[1]
cat(pc,"\n")