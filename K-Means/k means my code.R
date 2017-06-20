Kmeans <- function(x, n, min = 0, max) {
       centernumbers = sample(nrow(x), n)
       centers = matrix(nrow = n, ncol = ncol(x))
       centers = x[centernumbers, ]
       flag = 0
       repeat {
               distance=matrix(nrow=nrow(x),ncol=n)
               for(i in 1:nrow(x)){
                     for(j in 1:n){
                           distance[i,j]=t(x[i,]-centers[j,])%*%(x[i,]-centers[j,])
                     }
               }
               cluster=as.matrix(apply(distance,1,which.min))
               newcenters = matrix(0, nrow = n, ncol = ncol(x))
               count = matrix(0, nrow = n, ncol = 1)
               for (i in 1:nrow(x)) {
                      newcenters[cluster[i,1],] = newcenters[cluster[i,1],] + x[i,]
                      count[cluster[i, 1]] = count[cluster[i, 1]] + 1
               }
               for (i in 1:n) newcenters[i, ] = newcenters[i, ]/count[i]
               d = 0
               for (i in 1:n) d = d + t(centers[i,]-newcenters[i,])%*%(centers[i,]-newcenters[i,])
               centers = newcenters
               flag = flag + 1
               if (missing(min)){ if(flag > max)	break} 
               else	{if (d < min) break }
       }
       return(list(cluster = cluster, centers = centers,iterations=flag))
}
#x=as.matrix(read.table("D:\\sagatam das machine learning\\flame.txt"))[,c(1,2)] 
#x=as.matrix(read.table("D:\\sagatam das machine learning\\R15.txt"))[,c(1,2)]
#x = read.table("clustering.txt")
x=as.matrix(iris[,c(3,4)])
r = Kmeans(x,3,min=10^-5, max = 10)
#r$centers
r$iterations
plot(x, col = r$cluster, pch = 16)
points(r$centers, col = 1:3, pch = 8, cex = 2)
