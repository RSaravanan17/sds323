library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(cluster)

wine = read.csv('./data/wine.csv', header=TRUE)

summary(wine)
head(wine)

# Center and scale the data
X = wine[,-(12:13)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")


##### K-means #####

# Run k-means with 10 clusters and 100 starts
while (TRUE) {
  tryCatch({
    clust1 = kmeans(X, 5, nstart=100)
    break
  }, error = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  }, warning = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  })
}

# What are the clusters?
clust1$center  # not super helpful
for (i in 1:10) {
  cat("\n\nAverage Data of Cluster", i, ":\n\n")
  print(clust1$center[i,]*sigma + mu)
}

# Which wine bottles are in which clusters?
for (i in 1:10) {
  cat("\n\nWine Bottles in Cluster", i, ":\n\n")
  print(which(clust1$cluster == i))
}

# A few plots with cluster membership shown
# qplot is in the ggplot2 library
qplot(alcohol, pH, data=wine, color=factor(clust1$cluster))
qplot(alcohol, residual.sugar, data=wine, color=factor(clust1$cluster))
qplot(alcohol, fixed.acidity, data=wine, color=factor(clust1$cluster))
qplot(pH, fixed.acidity, data=wine, color=factor(clust1$cluster))
qplot(volatile.acidity, fixed.acidity, data=wine, color=factor(clust1$cluster))

D_k = data.frame(wine, z = clust1$cluster)

# plot proportion of red vs. white
ggplot(data = D_k) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = color), position="fill", stat='identity') +
  ggtitle("Proportion of Red and White Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Color of Wine")

# plot proportion of each quality
ggplot(data = D_k[order(D_k$quality), ]) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = quality), position="fill", stat='identity') +
  ggtitle("Proportion of Each Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Quality of Wine")


##### K-means++ #####

# Run k-means++ with 10 clusters and 100 starts
while (TRUE) {
  tryCatch({
    clust2 = kmeanspp(X, k=5, nstart=100)
    break
  }, error = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  }, warning = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  })
}

D_kpp = data.frame(wine, z = clust2$cluster)

# plot proportion of red vs. white
ggplot(data = D_kpp) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = color), position="fill", stat='identity') +
  ggtitle("Proportion of Red and White Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Color of Wine")

# plot proportion of each quality
ggplot(data = D_kpp[order(D_kpp$quality), ]) + 
  geom_bar(mapping = aes(x = z, y = 1, fill = quality), position="fill", stat='identity') +
  ggtitle("Proportion of Each Quality of Wine in Each Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Quality of Wine")

# Elbow plot for k=5 to k=15
while (TRUE) {
  tryCatch({
    k_grid = seq(5, 15, by=1)
    SSE_grid = foreach (k = k_grid, .combine='c') %do% {
      cat(k, "")
      cluster_k = kmeanspp(X, k, nstart=25)
      cluster_k$tot.withinss
    }
    plot(k_grid, SSE_grid)
    break
  }, error = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  })
}

# CH index plot for k=5 to k=15
while (TRUE) {
  tryCatch({
    N = nrow(wine)
    CH_grid = foreach(k = k_grid, .combine='c') %do% {
      cat(k, "")
      cluster_k = kmeanspp(X, k, nstart=25)
      W = cluster_k$tot.withinss
      B = cluster_k$betweenss
      CH = (B / W) * ((N - k) / (k - 1))
      CH
    }
    plot(k_grid, CH_grid)
    break
  }, error = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  })
}

asdf = 0
# Gap statistic
while (TRUE) {
  tryCatch({
    print(asdf)
    asdf = asdf + 1
    wine_gap_kpp = clusGap(X, FUN = kmeanspp, nstart = 25, K.max = 20, B = 10)
    break
  }, error = function(w) {
    cat("\n")
    print(w)
    cat("\n")
  })
}
plot(wine_gap_kpp)
wine_gap_kpp

wine_gap_k = clusGap(X, FUN = kmeans, nstart = 25, K.max = 20, B = 10)
plot(wine_gap_k)
wine_gap_k


# Compare versus within-cluster and between-cluster average distances
cat("K-means total within-cluster distances:", clust1$tot.withinss)
cat("K-means++ total within-cluster distances:", clust2$tot.withinss)

cat("K-means between-cluster distances:", clust1$betweenss)
cat("K-means++ between-cluster distances:", clust2$betweenss)


##### Hierarchical clustering #####

# Form a pairwise distance matrix using the dist function
wine_dist_matrix = dist(X, method='euclidean')


### Single (min) linkage
hier_wine_single = hclust(wine_dist_matrix, method='single')
plot(hier_wine_single, cex=0.8)  # Plot the dendrogram

hier_cluster1_single = cutree(hier_wine_single, k=15)  # Cut the trees into 15 clusters
summary(factor(hier_cluster1_single))

which(hier_cluster1_single == 1)  # Examine the cluster members

D_single = data.frame(wine, z = hier_cluster1_single)

# plot proportion of red vs. white
ggplot(data = D_single) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = color), position="fill", stat='identity') +
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
hier_wine_complete = hclust(wine_dist_matrix, method='complete')
plot(hier_wine_complete, cex=0.8)

hier_cluster1_complete = cutree(hier_wine_complete, k=15)
summary(factor(hier_cluster1_complete))

which(hier_cluster1_complete == 1)

D_complete = data.frame(wine, z = hier_cluster1_complete)

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

# Gap statistic
cluster_gap_hier_single <- function(x, k) list(cluster=cutree(hclust(dist(x), method="single"), k=k))
wine_gap_hier_single = clusGap(X, FUN=cluster_gap_hier_single, K.max=10, B=5)
plot(wine_gap_hier_single)
wine_gap_hier_single

cluster_gap_hier_complete <- function(x, k) list(cluster=cutree(hclust(dist(x), method="complete"), k=k))
wine_gap_hier_complete = clusGap(X, FUN=cluster_gap_hier_complete, K.max=10, B=5)
plot(wine_gap_hier_complete)
wine_gap_hier_complete

cluster_gap_hier_average <- function(x, k) list(cluster=cutree(hclust(dist(x), method="average"), k=k))
wine_gap_hier_average = clusGap(X, FUN=cluster_gap_hier_average, K.max=10, B=5)
plot(wine_gap_hier_average)
wine_gap_hier_average

cluster_gap_hier_centroid <- function(x, k) list(cluster=cutree(hclust(dist(x), method="centroid"), k=k))
wine_gap_hier_centroid = clusGap(X, FUN=cluster_gap_hier_centroid, K.max=10, B=5)
plot(wine_gap_hier_centroid)
wine_gap_hier_centroid
