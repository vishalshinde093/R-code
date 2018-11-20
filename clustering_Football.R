football <- read.csv("C:/JS/Clustering/Song_Football.csv")
str(football)
summary(football)

song <- football[which(football$First_Name=="Song"),] # information abbout song
sam <- football[258:268,] # no. of rows near to song
#player similar to song in terms of passes
# outliers doesn't affect clustering there is no need to remove or impute the oulier
# If there are outlier they will automatically assign to a single cluster

# keeping only the numeric values
# for clustering we don't need 1st column and use col 2:8
football.c <- football[,2:8]
football.na <- na.omit(football.c)

# scaling ( ordering is same)
football.sc <- scale(football.na,center=TRUE,scale=TRUE)

# why negative signs= because of Z score
# weighting tackles by 3
tackle3 <- football.sc[,1]*3
football.w <- cbind(football.sc,tackle3) # adding the tackle3 to the dataset
football.new <- football.w[,2:8] # removing the tackle variable

#now performing the kmeans clustering
set.seed(1234)
results <- kmeans(football.new, 20)
results

# understanding the output
results$size #size of each cluster

strength <- results$betweenss/results$tot.withinss # betweenss=distance between cluster should be large
# tot.withinss=avg. distance within cluster should be small
# strength should be higher

cluster <- results$cluster

football.clust <- cbind(football,cluster) 

# finding out which cluster song belong to
which(football.clust$First_Name=="Song")
football.clust[264,]

simial <- football.clust[which(football.clust$cluster==4),]

# showcasing cluster wise output
tapply(football.clust$Tackles,football.clust$cluster,mean)
mean(football.clust$Tackles)

tapply(football.clust$passes,football.clust$cluster,mean)
mean(football.clust$passes)

tapply(football.clust$interception,football.clust$cluster,mean)
mean(football.clust$interception)

#plotting groups
library(ggplot2)
with(football.clust,qplot(Tackles,passes,colour=factor(cluster)))
#plot(football.clust[c("Tackles","passes")],col=football.clust$cluster)

# optimal number of cluster

library(NbClust)
set.seed(1234)
noculs <- NbClust(football.new,min.nc=2,max.nc=15,method="kmeans")
table(noculs$Best.n[1,])
barplot(table(noculs$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# Hierarchical Clustering
dist.eu <- dist(football.new,method="euclidean",p=2)
hc <- hclust(dist.eu,method="average")
hc
plclust(hc,ylab="Distance")

#even in heirarchical clustering we have to cut 
#the tree specifying the no of clusters

# only 5 cluster solution
clust5 <- cutree(hc,k=5)

# To cut based on the value of height
clust5 <- cutree(hc,h=5)

clust5
table(clust5)
length(clust5)
