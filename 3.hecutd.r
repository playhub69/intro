utilities = read.csv("W:/MSC(I.T)/PSIT201 - Big Data Analytics/PRACS/CLUSTERING/Data sets/utilities.csv")
str(utilities)
utilities
pairs(utilities[,2:8])
plot(Fuel_Cost ~ Sales,utilities)
with(utilities,text(Fuel_Cost ~ Sales,labels=Company, pos=1,cex=0.7))
utilities
z=utilities[,2:9]
z
m=apply(z,2,mean)
s=apply(z,2,sd)
m
s
z=scale(z,m,s)
distance = dist(z)
print(distance,digits=3)
hc.c=hclust(distance)
plot(hc.c)
plot(hc.c,labels = utilities$Company)
plot(hc.c,hang=-1)
hc.a=hclust(distance, method = "average")
plot(hc.a)
plot(hc.a,labels = utilities$Company)
plot(hc.a,hang=-1)
member.c=cutree(hc.c,3)
member.c
member.a=cutree(hc.a,3)
member.a
table(member.c, member.a)
aggregate(z, list(member.c), mean)
aggregate(utilities[,-c(1,1)], list(member.c), mean)
library(cluster)
plot(silhouette(cutree(hc.c,3),distance))
wss <- numeric(20)
for (k in 1:20) 
wss[k]=sum(kmeans(z,centers=k, nstart=25 ) $withinss )
wss
plot(1 : 20, wss, type="b", xlab="Number of Clusters " , ylab="Within Sum of Squares" )
kc = kmeans(z,3)
kc
member.a
member.c
kc$cluster
plot(Sales ~ D.Demand, utilities)
plot(Sales ~ D.Demand, utilities, col =kc$cluster)






