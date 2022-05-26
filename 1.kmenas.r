newiris=iris
head(newiris)
pairs(newiris)
newiris$Species = NULL
newiris[1:5,]
kc=kmeans(newiris,3,10)
kc$cluster
table(iris$Species,kc$cluster)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=3)
library('factoextra')
library('tidyverse')
library('ggplot2')

fviz_cluster(kc,data=newiris)

k2=kmeans(newiris,2,25)
k3=kmeans(newiris,3,25)
k4=kmeans(newiris,4,25)
ggplot(aes(Sepal.Length, Sepal.Width, color = factor(cluster))) + geom_text()
fviz_cluster(k2,data=newiris)


p1 = fviz_cluster(k2,data=newiris, geom='point') + ggtitle('k=2')
p2 = fviz_cluster(k3,data=newiris, geom='point') + ggtitle('k=3')
p3 = fviz_cluster(k4,data=newiris, geom='point') + ggtitle('k=4')
p4 = fviz_cluster(k5,data=newiris, geom='point') + ggtitle('k=5')
library('gridExtra')
grid.arrange(p1,p2,p3,p4, nrow=2)

set.seed(125)

wss <- numeric(15)
wss
for (k in 1:15) 
  wss[k]=sum(kmeans(newiris,centers=k, nstart=25 ) $withinss )
wss

plot(1 : 15, wss, type="b", xlab="Number of Clusters " , 
     ylab="Within Sum of Squares" )

