library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(cluster)

social = read.csv('./data/social_marketing.csv', header=TRUE)

summary(social)
head(social)

# Center and scale the data
S = social[,-1]
S = scale(S, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(S,"scaled:center")
sigma = attr(S,"scaled:scale")



##### PCA #####

# PCA of social marketing data
PCAsocial = prcomp(S, scale=TRUE)

## variance plot
plot(PCAsocial)
summary(PCAsocial)

# first five PCs
rounded_PCs = round(PCAsocial$rotation[,1:5],2)  # first 5 PCs explain 40% of total variance
rounded_PCs > 0.2
rounded_PCs < -0.2

# merge quality and color to PC1, PC2, and PC3
social_features = social[,-1]
social_features = merge(social_features, PCAsocial$x[,1:5], by="row.names")

# plot with PC1 vs PC2
ggplot(social_features) + 
  geom_boxplot(aes(x=PC1, y=PC2, group=chatter))

# MARKET SEGMENTS:
## First PC is millenial and young adults without kids (no interest in parenting or school or family or religion) who are not interested in sports
## Second PC is female/artsy/pinteresty/aesthetic grouping
## Third PC is sports/fitness/health grouping not interested in travel, politics, news, or computers
## Fourth PC is college students into gaming (non-sports related games) who do not like going outside and being active and fit
## Fifth PC is college athletes into sports gaming and movies who don't like posting photos

# principal component regression: predicting some features from dataset
lm_chatter = lm(chatter ~ PC1 + PC2 + PC3 + PC4 + PC5, data=social_features)
#summary(lm_chatter)

ggplot(data = social_features) +
  geom_boxplot(aes(x=chatter, y=fitted(lm_chatter), group=chatter)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Chatter Tweets") +
  ylab("Predicted Number of Chatter Tweets")

ggplot(data = social_features) +
  geom_violin(aes(x=chatter, y=fitted(lm_chatter), group=chatter)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Chatter Tweets") +
  ylab("Predicted Number of Chatter Tweets")

lm_cooking = lm(cooking ~ PC1 + PC2 + PC3 + PC4 + PC5, data=social_features)
#summary(lm_cooking)

ggplot(data = social_features) +
  geom_boxplot(aes(x=cooking, y=fitted(lm_cooking), group=cooking)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Cooking Tweets") +
  ylab("Predicted Number of Cooking Tweets")

ggplot(data = social_features) +
  geom_violin(aes(x=cooking, y=fitted(lm_cooking), group=cooking)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Cooking Tweets") +
  ylab("Predicted Number of Cooking Tweets")

lm_fashion = lm(fashion ~ PC1 + PC2 + PC3 + PC4 + PC5, data=social_features)
#summary(lm_fashion)

ggplot(data = social_features) +
  geom_boxplot(aes(x=fashion, y=fitted(lm_fashion), group=fashion)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Fashion Tweets") +
  ylab("Predicted Number of Fashion Tweets")

ggplot(data = social_features) +
  geom_violin(aes(x=fashion, y=fitted(lm_fashion), group=fashion)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Fashion Tweets") +
  ylab("Predicted Number of Fashion Tweets")

lm_online_gaming = lm(online_gaming ~ PC1 + PC2 + PC3 + PC4 + PC5, data=social_features)
#summary(lm_online_gaming)

ggplot(data = social_features) +
  geom_boxplot(aes(x=online_gaming, y=fitted(lm_online_gaming), group=online_gaming)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Online Gaming Tweets") +
  ylab("Predicted Number of Online Gaming Tweets")

ggplot(data = social_features) +
  geom_violin(aes(x=online_gaming, y=fitted(lm_online_gaming), group=online_gaming)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Online Gaming Tweets") +
  ylab("Predicted Number of Online Gaming Tweets")

lm_college_uni = lm(college_uni ~ PC1 + PC2 + PC3 + PC4 + PC5, data=social_features)
#summary(lm_college_uni)

ggplot(data = social_features) +
  geom_boxplot(aes(x=college_uni, y=fitted(lm_college_uni), group=college_uni)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of College/University Tweets") +
  ylab("Predicted Number of College/University Tweets")

ggplot(data = social_features) +
  geom_violin(aes(x=college_uni, y=fitted(lm_college_uni), group=college_uni)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of College/University Tweets") +
  ylab("Predicted Number of College/University Tweets")

lm_sports_playing = lm(sports_playing ~ PC1 + PC2 + PC3 + PC4 + PC5, data=social_features)
#summary(lm_sports_playing)

ggplot(data = social_features) +
  geom_boxplot(aes(x=sports_playing, y=fitted(lm_sports_playing), group=sports_playing)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Sports Playing Tweets") +
  ylab("Predicted Number of Sports Playing Tweets")

ggplot(data = social_features) +
  geom_violin(aes(x=sports_playing, y=fitted(lm_sports_playing), group=sports_playing)) +
  ggtitle("Predicted Wine Quality from Principal Component Regression") +
  xlab("Actual Number of Sports Playing Tweets") +
  ylab("Predicted Number of Sports Playing Tweets")



##### K-means #####

# Run k-means++ with 10 clusters and 100 starts
while (TRUE) {
  tryCatch({
    clust1 = kmeans(S, 10, nstart=100)
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

while (TRUE) {
  tryCatch({
    clust2 = kmeanspp(S, 10, nstart=100)
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

clust1$size
clust2$size

ggplot(data = social) +
  geom_violin(aes(x = chatter, y = current_events, color=factor(clust2$cluster))) +
  ggtitle("Number of Chatter Tweets with Number of Current Event Tweets by Cluster") +
  xlab("Number of Chatter Tweets") +
  ylab("Number of Current Event Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = photo_sharing, y = travel, color=factor(clust2$cluster))) +
  ggtitle("Number of Photo Sharing Tweets with Number of Travel Tweets by Cluster") +
  xlab("Number of Photo Sharing Tweets") +
  ylab("Number of Travel Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = news, y = politics, color=factor(clust2$cluster))) +
  ggtitle("Number of News Tweets with Number of Politics Tweets by Cluster") +
  xlab("Number of News Tweets") +
  ylab("Number of Politics Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = sports_playing, y = sports_fandom, color=factor(clust2$cluster))) +
  ggtitle("Number of Sports Playing Tweets with Number of Sports Fandom Tweets by Cluster") +
  xlab("Number of Sports Playing Tweets") +
  ylab("Number of Sports Fandom Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = family, y = food, color=factor(clust2$cluster))) +
  ggtitle("Number of Family Tweets with Number of Food Tweets by Cluster") +
  xlab("Number of Family Tweets") +
  ylab("Number of Food Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = tv_film, y = photo_sharing, color=factor(clust2$cluster))) +
  ggtitle("Number of TV Film Tweets with Number of Photo Sharing Tweets by Cluster") +
  xlab("Number of TV Film Tweets") +
  ylab("Number of Photo Sharing Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = beauty, y = fashion, color=factor(clust2$cluster))) +
  ggtitle("Number of Beauty Tweets with Number of Fashion Tweets by Cluster") +
  xlab("Number of Beauty Tweets") +
  ylab("Number of Fashion Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = cooking, y = shopping, color=factor(clust2$cluster))) +
  ggtitle("Number of Cooking Tweets with Number of Shopping Tweets by Cluster") +
  xlab("Number of Cooking Tweets") +
  ylab("Number of Shopping Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = beauty, y = photo_sharing, color=factor(clust2$cluster))) +
  ggtitle("Number of Beauty Tweets with Number of Photo Sharing Tweets by Cluster") +
  xlab("Number of Beauty Tweets") +
  ylab("Number of Photo Sharing Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = personal_fitness, y = health_nutrition, color=factor(clust2$cluster))) +
  ggtitle("Number of Personal Fitness Tweets with Number of Health Nutrition Tweets by Cluster") +
  xlab("Number of Personal Fitness Tweets") +
  ylab("Number of Health Nutrition Tweets") +
  guides(fill=guide_legend(title="Cluster"))

ggplot(data = social) +
  geom_violin(aes(x = online_gaming, y = college_uni, color=factor(clust2$cluster))) +
  ggtitle("Number of Online Gaming Tweets with Number of College/University Tweets by Cluster") +
  xlab("Number of Online Gaming Tweets") +
  ylab("Number of College/University Tweets") +
  guides(fill=guide_legend(title="Cluster"))

qplot(chatter, current_events, data=social, color=factor(clust2$cluster))
qplot(chatter, photo_sharing, data=social, color=factor(clust2$cluster))

# Gap statistic
social_gap = clusGap(S, FUN = kmeans, nstart = 25, K.max = 15, B = 5)
plot(social_gap)
social_gap

# Compare versus within-cluster and between-cluster average distances
clust1$withinss
clust2$withinss

clust1$tot.withinss
clust2$tot.withinss

clust1$betweenss
clust2$betweenss


##### Hierarchical clustering #####

# Form a pairwise distance matrix using the dist function
social_dist_matrix = dist(S, method='euclidean')


### Complete (max) linkage
hier_social_complete = hclust(social_dist_matrix, method='complete')
#plot(hier_social_complete, cex=0.8)  # Plot the dendrogram

hier_social_cluster1_complete = cutree(hier_social_complete, k=15)  # Cut the trees into 15 clusters
summary(factor(hier_social_cluster1_complete))

#which(hier_social_cluster1_complete == 1)  # Examine the cluster members

D_complete_social = data.frame(social, z = hier_social_cluster1_complete)

# plot proportion of chatter by cluster
ggplot(data = D_complete_social[order(D_complete_social$chatter), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = chatter), position="fill", stat='identity') +
  ggtitle("Proportion of Chatter Tweets by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Chatter Tweets")

ggplot(data = D_complete_social[order(D_complete_social$photo_sharing), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = photo_sharing), position="fill", stat='identity') +
  ggtitle("Proportion of Photo Sharing by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Photo Sharing Tweets")

ggplot(data = D_complete_social[order(D_complete_social$cooking), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = cooking), position="fill", stat='identity') +
  ggtitle("Proportion of Cooking by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Cooking Tweets")

ggplot(data = D_complete_social[order(D_complete_social$fashion), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = fashion), position="fill", stat='identity') +
  ggtitle("Proportion of Fashion by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Fashion Tweets")

ggplot(data = D_complete_social[order(D_complete_social$beauty), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = beauty), position="fill", stat='identity') +
  ggtitle("Proportion of Beauty by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Beauty Tweets")

ggplot(data = D_complete_social[order(D_complete_social$shopping), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = shopping), position="fill", stat='identity') +
  ggtitle("Proportion of Shopping by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Shopping Tweets")

ggplot(data = D_complete_social[order(D_complete_social$sports_fandom), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = sports_fandom), position="fill", stat='identity') +
  ggtitle("Proportion of Sports Fandom by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Sports Fandom Tweets")

ggplot(data = D_complete_social[order(D_complete_social$personal_fitness), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = personal_fitness), position="fill", stat='identity') +
  ggtitle("Proportion of Personal Fitness by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Personal Fitness Tweets")


### Single (min) linkage
hier_social_single = hclust(social_dist_matrix, method='single')
#plot(hier_social_single, cex=0.8)  # Plot the dendrogram

hier_social_cluster1_single = cutree(hier_social_single, k=15)  # Cut the trees into 15 clusters
summary(factor(hier_social_cluster1_single))

which(hier_social_cluster1_single == 1)  # Examine the cluster members

D_single_social = data.frame(social, z = hier_social_cluster1_single)

# plot proportion of chatter by cluster
ggplot(data = D_single_social[order(D_single_social$chatter), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = chatter), position="fill", stat='identity') +
  ggtitle("Proportion of Chatter Tweets by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Chatter Tweets")


### Average linkage
hier_social_average = hclust(social_dist_matrix, method='average')
#plot(hier_social_average, cex=0.8)  # Plot the dendrogram

hier_social_cluster1_average = cutree(hier_social_average, k=15)  # Cut the trees into 15 clusters
summary(factor(hier_social_cluster1_average))

which(hier_social_cluster1_average == 1)  # Examine the cluster members

D_average_social = data.frame(social, z = hier_social_cluster1_average)

# plot proportion of chatter by cluster
ggplot(data = D_average_social[order(D_average_social$chatter), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = chatter), position="fill", stat='identity') +
  ggtitle("Proportion of Chatter Tweets by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Chatter Tweets")


### Centroid linkage
hier_social_centroid = hclust(social_dist_matrix, method='centroid')
#plot(hier_social_centroid, cex=0.8)  # Plot the dendrogram

hier_social_cluster1_centroid = cutree(hier_social_centroid, k=15)  # Cut the trees into 15 clusters
summary(factor(hier_social_cluster1_centroid))

which(hier_social_cluster1_centroid == 1)  # Examine the cluster members

D_centroid_social = data.frame(social, z = hier_social_cluster1_centroid)

# plot proportion of chatter by cluster
ggplot(data = D_centroid_social[order(D_centroid_social$chatter), ]) + 
  geom_bar(mapping = aes(x = z, y = 100, fill = chatter), position="fill", stat='identity') +
  ggtitle("Proportion of Chatter Tweets by Cluster") +
  xlab("Cluster") +
  ylab("Proportion of Chatter Tweets")
