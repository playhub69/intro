library (plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
getwd()
setwd("C:/Users/sahil/Desktop/msc")
grade_input = read.csv('W:/MSC(I.T)/PSIT201 - Big Data Analytics/PRACS/CLUSTERING/Data sets/grades_km_input.csv')
str(grade_input)
kmdata = as.matrix(grade_input[,2:4])
str(kmdata)
kmdata[1:5,]
wss = 15
for (k in 1:15)
  wss[k]=sum(kmeans(kmdata,centers=k,nstart=25)$withinss)
wss
plot(
  1:15,
  wss,
  type="b",
  xlab = "No. of clusters",
  ylab = "Withing Sum of Squares")
km = kmeans(kmdata,3, nstart=25)
km
df=grade_input[,2:4]
str(df)
km$cluster
df$cluster = factor(km$cluster)
str(df$cluster)
centers = as.data.frame(km$centers)
centers
ggplot(data=df, aes(x=English, y=Math, color=cluster ))+
  geom_point() + theme(legend.position="right")
library('factoextra')
library('tidyverse')
library('ggplot2')
fviz_cluster(km,data=kmdata, geom='point') + ggtitle('k=3')
g1= ggplot(data=df, aes(x=English, y=Math, color=cluster )) +
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, 
             aes(x=English,y=Math, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3,show.legend = FALSE)

g2 =ggplot(data=df, aes(x=English, y=Science, color=cluster )) +
  geom_point () +
  geom_point(data=centers,
             aes(x=English,y=Science, color=as.factor(c(1,2,3))),
             size=10, alpha=.3,show.legend = FALSE)
g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster )) +
  geom_point () +
  geom_point(data=centers,
             aes(x=Math,y=Science, color=as.factor(c(1,2,3))),
             size=10, alpha=0.3,show.legend = FALSE)

tmp = ggplot_gtable(ggplot_build(g1))
library('gridExtra')
grid.arrange(g1,g2,g3, nrow=3)

