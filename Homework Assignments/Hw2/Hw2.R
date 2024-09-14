library(mlbench)
data(Ionosphere)
View(Ionosphere)

plot(Ionosphere[1:10])
plot(Ionosphere[20:30])
summary(Ionosphere)

new_df <- Ionosphere[,-2]
dim(new_df)

library(class)
train.X <- new_df[,-34]
test.X <- new_df[,-34]
train.y <- new_df$Class
test.y <- new_df$Class
set.seed(1)
knn.predict <- knn(train = train.X,
                   test = test.X,
                   cl = train.y,
                   k = 1)
mean(knn.predict != test.y)

table(knn.predict, test.y)

set.seed(4323)
n <- nrow(new_df)
train <- sample(1:n, 0.7*n)
train.X <- new_df[train, -34]; train.y <- new_df[train, "Class"]
test.X <- new_df[-train, -34]; test.y <- new_df[-train, "Class"]
set.seed(4323)
knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 3)
mean(knn.predict != test.y)

table(knn.predict, test.y)

set.seed(4323)
knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 5)
mean(knn.predict != test.y)

set.seed(4323)
knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 7)
mean(knn.predict != test.y)

Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
new_df_auto <- data.frame(mpg01 = Auto$mpg01, Auto[, -1])

par(mfrow = c(2, 4))
boxplot(cylinders ~ mpg01, new_df_auto)
boxplot(displacement ~ mpg01, new_df_auto)
boxplot(weight ~ mpg01, new_df_auto)
boxplot(acceleration ~ mpg01, new_df_auto)
boxplot(year ~ mpg01, new_df_auto)
boxplot(origin ~ mpg01, new_df_auto)

plot(horsepower ~ mpg01, new_df_auto)

new_df_auto2 <- new_df_auto[, -c(2, 8:10)]

set.seed(1)
N <- nrow(new_df_auto2)
train <- sample(1:N, 0.7*N)
my_feature <- c("displacement", "horsepower", "weight", "acceleration", "year")
train.X <- new_df_auto2[train, -1]
train.y <- new_df_auto2[train, "mpg01"]
test.X <- new_df_auto2[-train, -1]
test.y <- new_df_auto2[-train, "mpg01"]

knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 1)
mean(knn.predict != test.y)

set.seed(4323)
n <- nrow(new_df)
train <- sample(1:n, 0.7*n)
train.X <- new_df[train, -34]; train.y <- new_df[train, "Class"]
test.X <- new_df[-train, -34]; test.y <- new_df[-train, "Class"]
set.seed(4323)
knn.predict <- knn(train = train.X, test = test.X, cl = train.y, k = 3)
mean(knn.predict != test.y)