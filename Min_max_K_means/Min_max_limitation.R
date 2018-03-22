# Min max fails on data if density of the clusters are very different from one another 


#DATA GENERATION:
      x1=rnorm(30,8,2)
	    y1=rnorm(30,10,2)
	    x2=rnorm(400,20,2)
	    y2=rnorm(400,10,2)
	    x=matrix(c(x1,x2,y1,y2),ncol=2,byrow=FALSE)
	    initial_centers=matrix(c(8,10,20,10),byrow=T,nrow=2)
	    #write.table(x, file="mymatrix.txt", row.names=FALSE, col.names=FALSE)	  #to print data in txt file


MaxMinKmeans <- function(x, n,centers, Min=0,Max,pstep=pstep,maxp=maxp,Beta=Beta) {
      t=0
      p=0
      w=rep(1/n,n)
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
             print(c(t,d))        
             }
             cluster=c()
             for(i in 1:nrow(x)){
                   cluster[i]=sum(delta[i,]*seq(1:n))
             }
             print(c(t,d))
             return(list(cluster = cluster, centers = centers,iterations=t))
}

r = MaxMinKmeans(x,2,centers=initial_centers,Min=10^-3,Max=30,pstep=0.05 , maxp=0.4,Beta=0.3)
r$centers
r$iterations
mycolors=c("red","blue")
plot(x, col = mycolors[r$cluster], pch = 16,cex=0.5)
points(r$centers, col = mycolors[c(1:2)], pch = 8, cex = 2)


