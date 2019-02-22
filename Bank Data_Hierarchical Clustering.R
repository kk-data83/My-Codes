##### Clustering Techniques - Sample Problem

### Bank customer data

### Hierarchical Clustering method

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

### Hierarchical Clustering 

## Step 1 - calculate the euclidean distance

dist_newdata <- dist(newdata, method = 'euclidean')

## Step 2 - Generate average linkage analysis

hc_newdata <- hclust(dist_newdata, method = 'average')

## Step 3 - create a denrogram object from thehclust variable

dend_newdata <- as.dendrogram(hc_newdata)

## Step 4 - plot the dendrogram
plot(dend_newdata)

## step 4 - color branches by cluster formed from the cut at a height of 3.25

install.packages('dendextend')
library(dendextend)
dend_colored <- color_branches(dend_newdata, h = 3.25)

plot(dend_colored)

## Step 5 - Extracting the clusters using cutree

cluster_assignement <- cutree(hc_newdata, k = 3)

print(cluster_assignement)

## Step 6 - Merging with the data frame
install.packages('dplyr')
library(dplyr)

cust_clusters <- mutate(data, cluster = cluster_assignement)

print(cust_clusters)

## Step 6 - Visualization using ggplot

install.packages('ggplot2')
library(ggplot2)

ggplot(cust_clusters, aes(x= Total_Credit_Cards, y = Avg_Credit_Limit, color = factor(cluster)))+ geom_point()

install.packages('dplyr')
library(dplyr)
count(cust_clusters, cluster)

print(cust_clusters[,c(2,8)])


### Step 7 - Profiling the clusters

install.packages('tidyr')
library(tidyr)

install.packages('dplyr')
library(dplyr)

count(newdata_2,cluster)

cust_clusters[,3:8]%>%
  group_by(cluster)%>%
  summarise_all(funs(mean(.)))
