
euclid <- function(points1, points2) {
  distanceMatrix=matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i]=sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}
K_means <- function(x,n, distFun, max){
  t=x
  t=as.matrix(t)
  centers=t[sample(nrow(t), n),]
  count=0
  repeat{
    dist=distFun(x, centers)
    clusters=apply(dist, 1, which.min)
    ncenters=apply(x, 2, tapply, clusters, mean)
    d=0
    for(i in 1:n)
      d=d+sqrt(sum((centers[i]-ncenters[i])^2))
    centers=ncenters
    count=count+1
    if (count>max) break
  }
  list(clusters=clusters, centers=centers)
}
head(iris)
x=iris
x$Sepal.Length=NULL
x$Sepal.Width=NULL
x$Species=NULL
r=K_means(x,3,euclid,20)
r$centers
r
plot(iris[c("Petal.Length", "Petal.Width")], col=r$cluster)
points(r$centers, col=1:3, pch=8, cex=2)