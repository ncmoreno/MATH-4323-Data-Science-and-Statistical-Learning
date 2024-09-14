library(ISLR)
View(Auto)
mpg01 <- rep(0, length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)] <- 1
new_df_auto <- data.frame(Auto, mpg01)

rm(new_df_auto)

par(mfrow = c(2, 4))
boxplot(cylinders ~ mpg01, new_df_auto)
boxplot(displacement ~ mpg01, new_df_auto)
boxplot(weight ~ mpg01, new_df_auto)
boxplot(acceleration ~ mpg01, new_df_auto)
boxplot(year ~ mpg01, new_df_auto)
boxplot(origin ~ mpg01, new_df_auto)

par(mfrow = c(1,1))
plot(cylinders ~ mpg01, new_df_auto)

set.seed(1)
N <- nrow(new_df_auto)
train <- sample(1:N, 0.7*N)
my_feature <- c("displacement", "horsepower", "weight", "year")
train.X <- new_df_auto[train, my_feature]
test.X <- new_df_auto[-train, my_feature]
train.y <- new_df_auto$mpg01[train]
test.y <- new_df_auto$mpg01[-train]

library(class)
knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 1)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 3)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 5)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 7)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 9)
mean(knn.predict != test.y)

train.X_scaled <- scale(train.X)
test.X_scaled <- scale(test.X, center = attr(train.X_scaled, "scaled:center"),
                                scale = attr(train.X_scaled, "scaled:scale"))

knn.predict <- knn(train = train.X_scaled, test = test.X_scaled, cl = train.y, k = 1)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X_scaled, test = test.X_scaled, cl = train.y, k = 3)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X_scaled, test = test.X_scaled, cl = train.y, k = 5)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X_scaled, test = test.X_scaled, cl = train.y, k = 7)
mean(knn.predict != test.y)

knn.predict <- knn(train = train.X_scaled, test = test.X_scaled, cl = train.y, k = 9)
mean(knn.predict != test.y)


for(K in c(1, 3, 10)){
  set.seed(1)
  knn.predict <- knn.cv(train = new_df_auto[,c("displacement", "horsepower", "weight", "year")],
                     cl = new_df_auto$mpg01, k = K)
print(mean(knn.predict != new_df_auto$mpg01))
}

for(K in c(1, 3, 10)){
  set.seed(1)
  knn.predict <- knn.cv(train = scale(new_df_auto[,c("displacement", "horsepower", "weight", "year")]),
                        cl = new_df_auto$mpg01, k = K)
  print(mean(knn.predict != new_df_auto$mpg01))
}