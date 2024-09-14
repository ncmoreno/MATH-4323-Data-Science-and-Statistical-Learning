#1a
X <- cbind(c(6, 5, 4, 1, 1, 0), c(4, 2, 3, 2, 1, 0))
plot(X)

#1b
RNGkind(sample.kind = "default")
set.seed(2)
labels <- sample(2, nrow(X), replace = T)
labels

Xlabeled <- cbind(X, labels)
Xlabeled

#1c
croids <- aggregate(Xlabeled, by = list(Xlabeled[,3]), FUN = mean)
croids

croid1 <- as.matrix(centroids[1, 2:3])
croid2 <- as.matrix(centroids[2, 2:3])
croid1
croid2

#1d
dist <- as.matrix(dist(rbind(X, croid1, croid2)))
dist <- dist[-c(nrow(dist), nrow(dist) - 1), c(nrow(dist) - 1, nrow(dist))]
rownames(dist) <- 1:nrow(X)
dist

#1e
library(factoextra)
initial_croids <- as.matrix(croids[,2:3])
km <- kmeans(X, centers = initial_croids)
km$cluster
km$iter

#1f
plot(X, col = cluster_labels, pch = 16)
points(croid1, col = "red", pch = 3, cex = 2)
points(croid2, col = "red", pch = 3, cex = 2)

#3a
set.seed(1)
x <- matrix(rnorm(20*2*5), ncol = 2)
x[21:40, 1] <- x[21:40, 1] + 6
x[41:60, 2] <- x[41:60, 2] + 6
x[61:80, 1] <- x[61:80, 1] - 6
x[81:100, 2] <- x[81:100, 2] - 6
x <- scale(x)
plot(x, xlab = "X1", ylab = "X2", main = "Simulated Data")

#3b
library(factoextra)
km.res4 <- eclust(data.frame(x), FUNcluster = "kmeans", k = 4, nstart = 50)
km.res4$withinss
km.res4$tot.withinss

km.res5 <- eclust(data.frame(x), FUNcluster = "kmeans", k = 5, nstart = 50)
km.res5$withinss
km.res5$tot.withinss

km.res6 <- eclust(data.frame(x), FUNcluster = "kmeans", k = 6, nstart = 50)
km.res6$withinss
km.res6$tot.withinss

#3d
kclust <- sapply(1:10, function(k) {
  km.res <- eclust(data.frame(x), FUNcluster = "kmeans", k = k, nstart = 50)
  print(paste0("K = ", k, ": ", km.res$tot.withinss))
})

kclust <- sapply(1:10, function(k) {
  km.res <- eclust(data.frame(x), FUNcluster = "kmeans", k = k, nstart = 50)
  km.res$tot.withinss
})

elbow <- fviz_nbclust(x, FUNcluster = kmeans, method = "wss")
elbow

#3e
gap <- fviz_nbclust(x, FUNcluster = kmeans, nstart = 50, method = "gap_stat")
gap

#4a
data("iris")
df_iris <- iris[,c(1:4)]
kclust <- sapply(1:10, function(k) {
  km.res <- eclust(df_iris, FUNcluster = "kmeans", k = k, nstart = 50)
  km.res$tot.withinss
})

elbow <- fviz_nbclust(df_iris, FUNcluster = kmeans, method = "wss")
elbow

#4b
gap <- fviz_nbclust(df_iris, FUNcluster = kmeans, nstart = 50, method = "gap_stat")
gap

#4c
table(iris$Species)

#4d
scaled_df_iris <- scale(df_iris)
scaled_gap <- fviz_nbclust(scaled_df_iris, FUNcluster = kmeans, nstart = 50, method = "gap_stat")
scaled_gap

#4e
table(iris$Species)

#4f
km.res <- eclust(scaled_df_iris, FUNcluster = "kmeans", k = 2, nstart = 50)
iris$Species
iris$cluster <- km.res$cluster
table(iris$cluster)
table(iris$Species)

#5a
ch10_df <- t(Ch10Ex11)
ch10_df

#5b
kclust <- sapply(1:10, function(k) {
  km.res <- eclust(ch10_df, FUNcluster = "kmeans", k = k, nstart = 50)
  km.res$tot.withinss
})

elbow <- fviz_nbclust(ch10_df, FUNcluster = kmeans, method = "wss")
elbow

#5c
gap <- fviz_nbclust(ch10_df, FUNcluster = kmeans, nstart = 50, method = "gap_stat")
gap

#5e
km.res <- eclust(ch10_df, FUNcluster = "kmeans", k = 2, nstart = 50)
ch10_df$cluster <- km.res$cluster
table(ch10_df$cluster)