plot(NA, NA, type = "n", xlim = c(-7,5), ylim = c(-5,7),
     asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
symbols(c(-1), c(2), circles = c(4), add = TRUE, inches = FALSE)
text(c(5), c(2), "> 16")
text(c(-1), c(5), "16 >=")
text(c(-1), c(-1), ">= 4")
text(c(-1), c(2), "< 4")
points(0,0)
points(-1,1)
points(2,2)
points(3,4)

X1 <- c(1, 3, 4, 2, 4, 4, 1, 1, 0.5)
X2 <- c(4, 4, 3, 2, 2, 4, 2, 3, 4)
Y <- c("blue", "red", "red", "blue", "red", "red", "blue", "blue", "red")
plot(X1, X2, col = Y, xlim = c(0, 5), ylim = c(0, 6))
abline(8, -2)
abline(6, -2, lty = "dashed")
abline(10, -2, lty = "dashed")
abline(8.4, -2.2, col = "purple", lty = "dashed")

library(MASS)
data("Boston")
medv01 <- rep(0, length(Boston$medv))
medv01[Boston$medv > median(Boston$medv)] <- 1
Boston = data.frame(Boston, medv01)
Boston$medv = NULL

s1 <- Boston[,1:13]
s1[,1:6] <- NULL
s1[,2:3] <- NULL
s1[,3:4] <- NULL

s2 <- Boston[,1:13]
s2[,2:11] <- NULL

s3 <- Boston[,1:13]

library(class)
set.seed(1)
for(K in c(1, 5, 7))
{
  set.seed(1)
  knn.pred <- knn.cv(train = s1, cl = Boston$medv01, k = K)
  print(table(knn.pred, Boston$medv01))
  print(mean(knn.pred != Boston$medv01))
}


set.seed(1)
for(K in c(1, 5, 7))
{
  set.seed(1)
  knn.pred <- knn.cv(train = s2, cl = Boston$medv01, k = K)
  print(table(knn.pred, Boston$medv01))
  print(mean(knn.pred != Boston$medv01))
}

set.seed(1)
for(K in c(1, 5, 7))
{
  set.seed(1)
  knn.pred <- knn.cv(train = s3, cl = Boston$medv01, k = K)
  print(table(knn.pred, Boston$medv01))
  print(mean(knn.pred != Boston$medv01))
}

s1 <- scale(s1)
set.seed(1)
for(K in c(1, 5, 7))
{
  set.seed(1)
  knn.pred <- knn.cv(train = s1, cl = Boston$medv01, k = K)
  print(table(knn.pred, Boston$medv01))
  print(mean(knn.pred != Boston$medv01))
}

s2 <- scale(s2)
set.seed(1)
for(K in c(1, 5, 7))
{
  set.seed(1)
  knn.pred <- knn.cv(train = s2, cl = Boston$medv01, k = K)
  print(table(knn.pred, Boston$medv01))
  print(mean(knn.pred != Boston$medv01))
}

s3 <- scale(s3)
set.seed(1)
for(K in c(1, 5, 7))
{
  set.seed(1)
  knn.pred <- knn.cv(train = s2, cl = Boston$medv01, k = K)
  print(table(knn.pred, Boston$medv01))
  print(mean(knn.pred != Boston$medv01))
}

library(ISLR)
data("Auto")
Auto$mpg01 <- as.factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))
Auto$mpg <- NULL

library(e1071)
set.seed(1)
svm.tune <- tune(svm, mpg01 ~ ., data = Auto, kernel = "linear", 
              ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(svm.tune)

bestmod = svm.tune$best.model
pred <- predict(bestmod)
table(predict = pred, truth = Auto$mpg01)

plot(bestmod, data = Auto, horsepower ~ acceleration)
plot(bestmod, data = Auto, cylinders ~ weight)
plot(bestmod, data = Auto, displacement ~ horsepower)

set.seed(1)
train <- sample(nrow(OJ), 800)
trainOJ <- OJ[train, ]
testOJ <- OJ[-train, ]

linearOJ <- svm(Purchase ~ ., data = trainOJ,
              kernel = "linear", cost = 0.01)
summary(linearOJ)

mean(predict(linearOJ, trainOJ) != trainOJ$Purchase)
mean(predict(linearOJ, testOJ) != testOJ$Purchase)

svm.tune <- tune(svm, Purchase ~ ., data = trainOJ, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10)))
summary(svm.tune)

bestmod = svm.tune$best.model
table(predict(bestmod), truth = trainOJ$Purchase)
mean(predict(bestmod) != trainOJ$Purchase)
mean(predict(bestmod, testOJ) != testOJ$Purchase)