distance <- function(vector1, vector2) {
    d = 0
    for (i in 1:length(vector1)) d = d + (vector1[i] - vector2[i])^2
    sqrt(d)
}
euclid <- function(vector1, vector2) {
    distancematrix = matrix(nrow = nrow(vector1), ncol = nrow(vector2))
    for (i in 1:nrow(distancematrix)) 
	  for (j in 1:ncol(distancematrix)) distancematrix[i,j] = distance(vector1[i,],vector2[j,])
    return(distancematrix)
}
Kmeans <- function(x, n, min = 0, max) {
    x = as.matrix(x)
    centernumbers = sample(nrow(x), n)
    centers = matrix(nrow = n, ncol = ncol(x))
    centers = x[centernumbers, ]
    flag = 0
    repeat {
        dist = euclid(x, centers)
        cluster = matrix(nrow=nrow(x),ncol=1)
        for (i in 1:nrow(x)) cluster[i,1] = which.min(euclid(x, centers)[i,])
        newcenters = matrix(0, nrow = n, ncol = ncol(x))
        count = matrix(0, nrow = n, ncol = 1)
        for (i in 1:nrow(x)) {
            newcenters[cluster[i,1],] = newcenters[cluster[i,1],] + x[i,]
            count[cluster[i, 1]] = count[cluster[i, 1]] + 1
        }
        for (i in 1:n) newcenters[i, ] = newcenters[i, ]/count[i]
        d = 0
        for (i in 1:n) d = d + distance(centers[i, ], newcenters[i,])
        centers = newcenters
        flag = flag + 1
        if (missing(min)){ if(flag > max)	break} 
        else	{if (d < min) break }
    }
    return(list(cluster = cluster, centers = centers))
}
#x=as.matrix(read.table("D:\\sagatam das machine learning\\flame.txt"))[,c(1,2)] 
#x=as.matrix(read.table("D:\\sagatam das machine learning\\R15.txt"))[,c(1,2)]
#x = read.table("clustering.txt")
#x=volcano[,c(1,51)]
x=as.matrix(iris[,c(3,4)])
r = Kmeans(x,2,min=10^-3 , max = 10)
r$centers
r
plot(x, col = r$cluster, pch = 16)
points(r$centers, col = 1:3, pch = 8, cex = 2)