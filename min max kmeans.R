MaxMinKmeans <- function(x, n, Min=0,Max,pstep=pstep,maxp=maxp,Beta=Beta) {
      t=0
      p=0
      w=rep(1/n,n)
      centernumbers = sample(nrow(x), n)
      centers = matrix(nrow = n,ncol = ncol(x))
      centers = x[centernumbers, ]
      empty=FALSE
      repeat{
             t=t+1
             distance=matrix(nrow=nrow(x),ncol=n)
             for(i in 1:nrow(x)){
                   for(j in 1:n){
                         distance[i,j]=t(x[i,]-centers[j,])%*%(x[i,]-centers[j,])
                   }
                   }
             delta=matrix(0,nrow=nrow(x),ncol=n)
             for(i in 1:nrow(x)){
                   delta[i,which.min(w*distance[i,])]=1
                   }
             if(min(colSums(delta))<2){
                   empty=TRUE
                   p=p-pstep
                   if(p<0) return(NULL)
                   delta=DELTA
                   w=W
                   centers=centersOld
                   }
             centersOld=centers
             centers=t(delta)%*%x
             centers=centers/(colSums(delta))
             if(p<maxp & empty==FALSE){
                   p=p+pstep
                   DELTA=delta
                   W=w
                   }
             v=rowSums(t(delta)%*%distance)
             w=Beta*as.matrix(w)+(1-Beta)*as.matrix(v)^(1/(1-p))/sum(as.matrix(v)^(1/(1-p)))
             d = 0
             for (i in 1:n){
                   d = d + t(centers[i,]-centersOld[i,])%*%(centers[i,]-centersOld[i,])
                   }
             if (missing(Min)){ if(t > Max)	break} 
             else	{if (d < Min) break }
             print(t)        
             }
             cluster=c()
             for(i in 1:nrow(x)){
                   cluster[i]=sum(delta[i,]*seq(1:n))
             }
             
             return(list(cluster = cluster, centers = centers,iterations=t))
}
#x=as.matrix(read.table("D:\\sagatam das machine learning\\flame.txt"))[,c(1,2)] 
x=as.matrix(read.table("D:\\sagatam das machine learning\\R15.txt"))[,c(1,2)]
#x = read.table("clustering.txt")
#x=as.matrix(iris[,c(3,4)])
r = MaxMinKmeans(x,15,Min=10^-2,Max=20,pstep=0.1,maxp=1,Beta=0.6)
r$centers
r$iterations
mycolors=c("red","blue","green","yellow","black","skyblue","orange","purple","darkgreen","grey","goldenrod","violet","maroon","brown3","navyblue")
plot(x, col = mycolors[r$cluster], pch = 16)
points(r$centers, col = 1:15, pch = 8, cex = 2)
