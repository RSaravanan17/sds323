library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(cluster)
library(ggfortify)
library(clValid)

social = read.csv('./data/social_marketing.csv', header=TRUE)

summary(social)
head(social)

# Center and scale the data
S = social[,-1]
S = scale(S, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(S,"scaled:center")
sigma = attr(S,"scaled:scale")


intern <- clValid(S, nClust = 2:30, 
                  clMethods = c("hierarchical","kmeans","pam"), validation = "internal")


# Run k-means++ with 10 clusters and 100 starts
clust1 = kmeans(S, 10, nstart=100)
clust2 = kmeanspp(S, 10, nstart=100)

qplot(chatter, current_events, data=social, color=factor(clust2$cluster))
qplot(chatter, photo_sharing, data=social, color=factor(clust2$cluster))

# Gap statistic
social_gap = clusGap(S, FUN = kmeans, nstart = 25, K.max = 36, B = 5)
plot(social_gap)
social_gap

# Compare versus within-cluster and between-cluster average distances
cat("K-means within-cluster distances:", clust1$withinss)
cat("K-means++ within-cluster distances:", clust2$withinss)

cat("K-means total within-cluster distances:", clust1$tot.withinss)
cat("K-means++ total within-cluster distances:", clust2$tot.withinss)

cat("K-means between-cluster distances:", clust1$betweenss)
cat("K-means++ between-cluster distances:", clust2$betweenss)

##### PCA #####

# PCA of social data
PCAsocial = prcomp(S, scale=TRUE)

## variance plot
plot(PCAsocial)
summary(PCAsocial)

# first three PCs
round(PCAsocial$rotation[,1:3],2)
autoplot(PCAsocial)

##### Hierarchical clustering #####

# Form a pairwise distance matrix using the dist function
social_dist_matrix = dist(S, method='euclidean')

### Single (min) linkage
hier_social_single = hclust(social_dist_matrix, method='single')
plot(hier_social_single, cex=0.8)  # Plot the dendrogram

hier_cluster1_single = cutree(hier_social_single, k=15)  # Cut the trees into 15 clusters
summary(factor(hier_cluster1_single))

which(hier_cluster1_single == 1)  # Examine the cluster members

D_single = data.frame(social, z = hier_cluster1_single)

# plot proportion of red vs. white
ggplot(data = D_single) + 
  geom_bar(mapping = aes(x = z, y = 100), position="fill", stat='identity') +
  ggtitle("Proportion of Red and White Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Color of Wine")

# plot proportion of each quality
ggplot(data = D_single[order(D_single$quality), ]) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = quality), position="fill", stat='identity') +
  ggtitle("Proportion of Each Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Quality of Wine")


### Complete (max) linkage
hier_social_complete = hclust(wine_dist_matrix, method='complete')
plot(hier_social_complete, cex=0.8)

hier_cluster1_complete = cutree(hier_social_complete, k=13)
summary(factor(hier_cluster1_complete))

which(hier_cluster1_complete == 1)

D_complete = data.frame(social, z = hier_cluster1_complete)

# plot proportion of red vs. white
ggplot(data = D_complete) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = color), position="fill", stat='identity') +
  ggtitle("Proportion of Red and White Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Color of Wine")

# plot proportion of each quality
ggplot(data = D_complete[order(D_complete$quality), ]) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = quality), position="fill", stat='identity') +
  ggtitle("Proportion of Each Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Quality of Wine")

# bar plot of average quality by cluster
quality_mean_by_cluster = aggregate(D_complete[, 12], list(D_complete$z), mean)
quality_mean_by_cluster = rename(quality_mean_by_cluster, cluster = Group.1)
quality_mean_by_cluster = rename(quality_mean_by_cluster, avg_quality = x)
quality_mean_by_cluster$avg_quality <- round(quality_mean_by_cluster$avg_quality, digit=2)

ggplot(data = quality_mean_by_cluster) + 
  geom_bar(mapping = aes(x = reorder(cluster, -avg_quality), y = avg_quality), stat='identity') +
  geom_text(aes(x = reorder(cluster, -avg_quality), y = avg_quality, label = avg_quality), vjust = 2, color = "white", size = 2.5) +
  ggtitle("Average Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Average Quality of Wine")


### Average linkage
hier_wine_average = hclust(wine_dist_matrix, method='average')
plot(hier_wine_average, cex=0.8)

hier_cluster1_average = cutree(hier_wine_average, k=15)
summary(factor(hier_cluster1_average))

which(hier_cluster1_average == 1)

D_average = data.frame(wine, z = hier_cluster1_average)

# plot proportion of red vs. white
ggplot(data = D_average) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = color), position="fill", stat='identity') +
  ggtitle("Proportion of Red and White Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Color of Wine")

# plot proportion of each quality
ggplot(data = D_average[order(D_average$quality), ]) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = quality), position="fill", stat='identity') +
  ggtitle("Proportion of Each Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Quality of Wine")


### Centroid linkage
hier_wine_centroid = hclust(wine_dist_matrix, method='centroid')
plot(hier_wine_centroid, cex=0.8)

hier_cluster1_centroid = cutree(hier_wine_centroid, k=15)
summary(factor(hier_cluster1_centroid))

which(hier_cluster1_centroid == 1)

D_centroid = data.frame(wine, z = hier_cluster1_centroid)

# plot proportion of red vs. white
ggplot(data = D_centroid) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = color), position="fill", stat='identity') +
  ggtitle("Proportion of Red and White Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Color of Wine")

# plot proportion of each quality
ggplot(data = D_centroid[order(D_centroid$quality), ]) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = quality), position="fill", stat='identity') +
  ggtitle("Proportion of Each Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Quality of Wine")


