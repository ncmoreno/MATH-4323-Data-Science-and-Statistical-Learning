# Pokemon with stats

# Load the dataset to R Studio
Pokemon <- read.csv("C:/Users/chase/Downloads/Pokemon.csv", header=FALSE, comment.char="#")
View(Pokemon)


# --- Initial exploratory analysis ---


# See the Structure of the dataset
str(Pokemon)

# See the first 5 data values
head(Pokemon)

# Summary of the dataset
summary(Pokemon)

# Check for missing values
sapply(Pokemon, function(x) sum(is.na(x)))

# See what are the type1 of Pokemon type are there
table(Pokemon$V3)

# See what are the type2 of Pokemon type are there
table(Pokemon$V4)

# There 386 missing values so we need to fix it
# Replace empty strings in Type 2 with "None"
Pokemon$V4 <- ifelse(Pokemon$V4== "", "None", Pokemon$V4)

# Try again
table(Pokemon$V4)

# See how many Generations are there
table(Pokemon$V12)

# See how many Legendary Pokemon
table(Pokemon$V13)

# See the Legendary Pokemon by Type1
table(Pokemon$V3, Pokemon$V13)

# See the strongest Pokemon by the Sum of all stats
Pokemon[which.max(Pokemon$V5),]

# See the weakest Pokemon by the sum of all stats
Pokemon[which.min(Pokemon$V5),]


# --- Data Analysis ---


library(ggplot2)
#install.packages("reshape2")
library(reshape2)

# Select the features that we are going to use
features <- c("V6", "V7","V8","V9", "V10", "V11")
features_data <- Pokemon[, features]

# See head
head(features_data)

# OK lets rename these columns to make it easy to read
rename <- c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed")

# Rename the columns in features_data
colnames(features_data) <- rename

# See head again
head(features_data)

# Create the correlation matrix
cor_mat <- cor(features_data)

# Reshape the correlation matrix
cor_mat_melt <- melt(cor_mat)

# Create a heatmap of the correlation matrix
ggplot(data = cor_mat_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#0066CC", high = "#CCE5FF") +
  theme_minimal() +
  labs(title = "The Correlation Matrix") +
  geom_text(aes(label = round(value, 4)), color = "black")


# Scale the data using min-max scaling way
library(scales)
scaled_data <- as.data.frame(sapply(features_data,
                  function(x) rescale(x, to = c(0, 1))))

# Call to return first 5 data values
head(scaled_data)

# KMeans

# Find the optimal k value using the elbow method
library(factoextra)
kclust <- sapply(1:10, function(k) {
  km.model <- eclust(scaled_data, FUNcluster = "kmeans", k = k, 
                     nstart = 50)})
elbow <- fviz_nbclust(scaled_data, FUNcluster = kmeans, method = "wss")
elbow

# Find the silhouette Coef. for all k values
k.max <- 10
km.silh <- numeric(k.max)
for (k in 2:10){
  km.silh[k] <- eclust(scaled_data, FUNcluster = "kmeans", k = k, graph = 0,
                         nstart = 50)$silinfo$avg.width
}
plot(km.silh, type = "b", pch = 19, col = 4)

# Silhouette Coef. for k = 2
km.model <- eclust(scaled_data, FUNcluster = "kmeans", k = 2, nstart = 50)
km.model$silinfo


# Hierarchical Clustering

# Calculate the silhouette Coef. for single linkage
hc.sing = eclust(scaled_data, FUNcluster = "hclust", hc_method = "single")
print(hc.sing$silinfo$avg.width)

# Calculate the silhouette Coef. for complete linkage
hc.comp = eclust(scaled_data, FUNcluster = "hclust", hc_method = "complete")
print(hc.comp$silinfo$avg.width)

# Calculate the silhouette Coef. for average linkage
hc.avg = eclust(scaled_data, FUNcluster = "hclust", hc_method = "average")
print(hc.avg$silinfo$avg.width)


# 3d plot the model and summary
# KMeans
# Plot the model
km.model <- eclust(scaled_data, FUNcluster = "kmeans", k = 2, nstart = 50)

# Extract Defense and Sp.Def columns from scaled_data
def_SpDef <- scaled_data[, c("Defense", "Sp. Def")]

# Add the clusters to def_SpDef
def_SpDef$cluster <- as.factor(km.model$cluster)

# Plot the scatterplot of Defense vs. Sp.Def
ggplot(def_SpDef, aes(x = Defense, y = Sp.Def, color = cluster)) +
  geom_point()

# Extract HP and Attack columns from scaled_data
hp_atk <- scaled_data[, c("HP", "Attack")]

# Add the clusters to hp_atk
hp_atk$cluster <- as.factor(km.model$cluster)

# Plot the scatterplot of HP vs. Attack
ggplot(hp_atk, aes(x = HP, y = Attack, color = cluster)) +
  geom_point()

# Summary
summary(km.model)

# Hierarchical clustering
# Plot the dendrogram using average linkage
hc.avg <- hclust(dist(scaled_data), method = "average")
plot(hc.avg)

# Summary
summary(hc.avg)

