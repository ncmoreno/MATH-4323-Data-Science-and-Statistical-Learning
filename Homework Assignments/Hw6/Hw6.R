#1a
mat <- as.dist(matrix(c(0, .4, .75, .3,
               .4, 0, .5, .8,
               .75, .5, 0, .45,
               .3, .8, .45, 0), ncol = 4))
mat
mat.complete = hclust(as.dist(mat),method = "complete")
plot(mat.complete)

#1b
mat.single = hclust(as.dist(mat),method = "single")
plot(mat.single)

#1e
mat.complete = hclust(as.dist(mat),method = "complete")
plot(mat.complete, labels = c(2, 1, 4, 3))

#4a
x <- USArrests
comp = hclust(dist(x), method = "complete")
plot(comp)

#4b
clust3 <- cutree(comp, 3)
clust3
plot(clust3)

#4c
x_scaled<- scale(x)
comp_scaled <- hclust(dist(x_scaled), method = "complete")
plot(comp_scaled)

#4d
clust4 <- cutree(comp_scaled, 4)
clust4
plot(clust4)

#4e
x[1]
x[2]
x[3]
x[4]

#5b
ch10_df <- t(Ch10Ex11)
comp = hclust(dist(ch10_df), method = "complete")
plot(comp)

sing = hclust(dist(ch10_df), method = "single")
plot(sing)

avg <- hclust(dist(ch10_df), method = "average")
plot(avg)
