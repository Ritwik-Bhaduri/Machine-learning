Kmeans <- function(x,n,alpha,min.gradient){
       centernumbers = sample(nrow(x), n)
       centers = matrix(nrow = n, ncol = ncol(x))
       centers = x[centernumbers, ]
       cluster = matrix(nrow=nrow(x),ncol=1)
       iter=0
       repeat{
              distance=matrix(nrow=nrow(x),ncol=n)
              for(i in 1:nrow(x)){
                    for(j in 1:n){
                          distance[i,j]=t(x[i,]-centers[j,])%*%(x[i,]-centers[j,])
                    }
              }
              cluster=as.matrix(apply(distance,1,which.min))
              gradient=newcenters = matrix(0, nrow = n, ncol = ncol(x))
              for(i in 1:nrow(x)){
                    gradient[cluster[i,1],]=gradient[cluster[i,1],]+x[i,]-centers[cluster[i,1],]
              }
              newcenters=centers+alpha*gradient
              centers=newcenters
              if(max(abs(gradient))<min.gradient) break;
              iter=iter+1
              #print(iter)
       }
       return(list(cluster = cluster, centers = centers,iteration=iter,gradient=gradient))
}

#x=as.matrix(read.table("D:\\sagatam das machine learning\\R15.txt"))[,c(1,2)]
x=as.matrix(iris[,c(3,4)])
r = Kmeans(x,3,alpha=0.02,min=10^-3)     #for iris take alpha=0.1 or 0.2 //not more than 0.3
#r$cluster
#r$centers
r$iteration
max(r$gradient)
plot(x, col = r$cluster, pch = 16)
points(r$centers, col = 1:3, pch = 8, cex = 2)