##### Clustering Techniques - Sample Problem

### Bank customer data

### K-Means Clustering method

## Import the data set

data <- read.csv(file.choose())

### Data pre-processing
head(data)

colSums(cbind(is.na(data)))

summary(data)

#### convert data into a dataframe

data_df <- as.data.frame(data[,3:7])

## Calculate Hopkins Statistics
install.packages("clustertend")
library(clustertend)

install.packages("factoextra")
library(factoextra)

res <- get_clust_tendency(data_df, n= nrow(data_df)-1, graph = FALSE)

res$hopkins_stat

### The H value is 0.0920

#### Scaling of the data set

data_scale <- scale(data_df)

data_scale

newdata <- data_scale

newdata

#### K-Means Clustering

## Step 1 - Elbow diagram to determine the Kvalue

### use map_dbl to run any models with varying values of k centers

install.packages('purrr')

library(purrr)

##calculate the total withinness for different k values

tot_withinss <- map_dbl(1:5,  function(k){
  model <- kmeans(x = newdata, centers = k)
  model$tot.withinss
})

## Generate a dataframe containing both k and tot_withiness

elbow_df <- data.frame(
  k = 1:5,
  tot_withinss = tot_withinss
)

## Plot the elbow plot

library(ggplot2)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:5)

### Based on the plot the ideal k value would be 3

### Step 2 - Use average silhouette width to detrmine the k value

install.packages('cluster')
library(cluster)

sil_width <- map_dbl(2:5,  function(k){
  model <- pam(x = newdata, k = k)
  model$silinfo$avg.width
})

# Generate a data frame containing both k and sil_width
sil_df <- data.frame(
  k = 2:5,
  sil_width = sil_width
)

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:5)

### Based on Silhouette width we decide  3 clusters

### Step 3 - Build K - means model

model_km <- kmeans(newdata, centers = 3)

### Step 4 - Extract the cluster assignment vector from the k means model

clust_km <- model_km$cluster

## Step 5 create a new data frame with cluster added to the old data frame

library(dplyr)
newdata_2 <- mutate(data, cluster = clust_km)

newdata_2

## Step 6 - Profiling of the clusters

count(newdata_2,cluster)

newdata_2[,3:8]%>%
  group_by(cluster)%>%
  summarise_all(funs(mean(.)))

### Step 7 - calculate the silhouette width for each observation

pam_km <- pam(newdata, k = 3)

pam_km$silinfo$widths

sil_plot <- silhouette(pam_km)

plot(sil_plot)

### As all the observations have a silhouette width greater than 0, they have been clusttered in the right clusters.