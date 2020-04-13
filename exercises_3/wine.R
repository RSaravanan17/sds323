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
clust1 = kmeans(X, 10, nstart=100)

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


##### K-means++ #####

# Run k-means++ with 10 clusters and 100 starts
clust2 = kmeanspp(X, k=10, nstart=100)

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

# Gap statistic
wine_gap = clusGap(X, FUN = kmeans, nstart = 25, K.max = 15, B = 10)
plot(wine_gap)
wine_gap


# Compare versus within-cluster and between-cluster average distances
clust1$withinss
clust2$withinss

clust1$tot.withinss
clust2$tot.withinss

clust1$betweenss
clust2$betweenss


##### Hierarchical clustering #####

# Form a pairwise distance matrix using the dist function
wine_dist_matrix = dist(X, method='euclidean')


### Single (min) linkage
hier_wine_single = hclust(wine_dist_matrix, method='single')
plot(hier_wine_single, cex=0.8)  # Plot the dendrogram

hier_cluster1_single = cutree(hier_wine_single, k=10)  # Cut the trees into 10 clusters
summary(factor(hier_cluster1_single))

which(hier_cluster1_single == 1)  # Examine the cluster members

D1 = data.frame(wine, z = hier_cluster1_single)
ggplot(D1) + geom_point(aes(x=fixed.acidity, y=pH, col=factor(z)))


### Complete (max) linkage
hier_wine_complete = hclust(wine_dist_matrix, method='complete')
plot(hier_wine_complete, cex=0.8)

hier_cluster1_complete = cutree(hier_wine_complete, k=10)
summary(factor(hier_cluster1_complete))

which(hier_cluster1_complete == 1)

D2 = data.frame(wine, z = hier_cluster1_complete)
ggplot(D2) + geom_point(aes(x=fixed.acidity, y=pH, col=factor(z)))


### Average linkage
hier_wine_average = hclust(wine_dist_matrix, method='average')
plot(hier_wine_average, cex=0.8)

hier_cluster1_average = cutree(hier_wine_average, k=10)
summary(factor(hier_cluster1_average))

which(hier_cluster1_average == 1)

D3 = data.frame(wine, z = hier_cluster1_average)
ggplot(D3) + geom_point(aes(x=fixed.acidity, y=pH, col=factor(z)))
