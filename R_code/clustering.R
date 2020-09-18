# packages
library(purrr);library(cluster);library(gridExtra)
library(grid);library(NbClust);library(factoextra)
library(clue)

source('EDA.R')

# split dataset
set.seed(123)
smple = sample.int(n = nrow(data), size = floor(.8*(nrow(data))), replace = F)
train = data[smple,]
test = data[-smple,]


### Elbow Method ###
# create intra-cluster sum of square function
iss = function(k){
  kmeans(train[,3:5], k, iter.max = 100, algorithm = "Lloyd")$tot.withinss
}

k_values = 1:10
iss_values = sapply(k_values, iss)

plot(k_values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")


### Gap Statistic ###
set.seed(321)
stat_gap = clusGap(train[,3:5], FUNcluster = kmeans, nstart = 25,
                   K.max = 10, B = 100)
fviz_gap_stat(stat_gap)

# train model

k6 = kmeans(train[,3:5], 5, iter.max=100, nstart=50, algorithm="Lloyd")

# visualize
pcclust=prcomp(train[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

(g1 = ggplot(train[,3:5], aes(x = Income, y = Score)) + 
  geom_point(aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4",
                                "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering"))

kCols = function(vec){
  cols = rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
  }
digCluster = k6$cluster; dignm = as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col = kCols(digCluster), pch =19, xlab ="K-means", ylab="classes")
legend("bottomleft", unique(dignm), fill=unique(kCols(digCluster)))


# predict test data
predicted = cl_predict(k6, test[,3:5])
(g2 = g1 + geom_point(data = test[,3:5], aes(color = as.factor(predicted))))
