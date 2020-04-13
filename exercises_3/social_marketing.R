library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(cluster)

social = read.csv('./data/social_marketing.csv', header=TRUE)

summary(social)
head(social)

# Center and scale the data
S = social[-1]
S = scale(social, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(S,"scaled:center")
sigma = attr(S,"scaled:scale")

# Run k-means++ with 10 clusters and 100 starts
clust1 = kmeans(W, k=10, nstart=100)
clust2 = kmeanspp(W, k=10, nstart=100)

qplot(S, chatter, data=social, color=factor(clust2$cluster))

# Gap statistic
social_gap = clusGap(S, FUN = kmeans, nstart = 25, K.max = 15, B = 10)
plot(social_gap)
social_gap

# Compare versus within-cluster and between-cluster average distances
clust1$withinss
clust2$withinss

clust1$tot.withinss
clust2$tot.withinss

clust1$betweenss
clust2$betweenss
