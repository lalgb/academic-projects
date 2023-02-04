# ------------ climate data clustering using unsupervised learning --------------------
climate<-read.csv("AB_Climate_Means.csv")

climatedata <- data.frame(climate[3:5],climate[7:8])

scatterplotMatrix(climatedata,smooth=FALSE, regLine= FALSE,diagonal = list(method="boxplot"),groups = climate$BIOME,
                  legend = list(coords="bottomright", pt.cex=0.3, cex=0.2),main="Scatterplot of climate data for different biomes")


# ------------ hierarchial clustering --------------------

dist(climatedata[,-6])

plot(hclust(dist(climatedata[,-6]),method="single"),labels=row.names(climatedata),xlab="Climate data",ylab="Distance", main="Single linkage")

abline(h=75)
cluster1<-cutree(hclust(dist(climatedata[,-6]),method="single"),h=75)
max(cluster)

plot(hclust(dist(climatedata[,-6]),method="complete"),labels=row.names(climatedata),xlab="Climate data",ylab="Distance", main="Complete linkage")

abline(h=350)
cluster2<-cutree(hclust(dist(climatedata[,-6]),method="complete"),h=350)
max(cluster)

nc<-1
row.names(climatedata)[cluster1==nc]
climate.clus1<-lapply(1:2,function(nc) {row.names(climatedata)[cluster1==nc]})
climate.clus1

cluster.mean1<-lapply(1:2,function(nc) {colMeans(climatedata[cluster1==nc,-1])})
cluster.mean1

nn <- 1
row.names(climatedata)[cluster2==nn]
climate.clus2<-lapply(1:2,function(nn) {row.names(climatedata)[cluster==nn]})
climate.clus2

cluster.mean2<-lapply(1:2,function(nn) {colMeans(climatedata[cluster==nn,-6])})
cluster.mean2

# ------------ K-means --------------------

rge1<-apply(climatedata,2,max)-apply(climatedata,2,min)

climatedatastd<-sweep(climatedata,2,rge1,FUN="/")

kmeans(climatedatastd,2)
sum(kmeans(climatedatastd,2)$withinss)

kmeans(climatedatastd,3)

n<-length(climatedatastd[,1])

wss<-numeric(10) 
wss[1]<-(n-1)*sum(apply(climatedatastd,2,var))
sum(kmeans(climatedatastd,1)$withinss)
set.seed(1234) 

for(i in 2:10) {
  W<-sum(kmeans(climatedatastd,i)$withinss)
  wss[i]<-W
}
wss
plot(1:10,wss,type="l",xlab="Number of groups",ylab="Within groups sum of squares",lwd=2,main="WSS plot of K means cluster")

kmeans(climatedatastd,5)
