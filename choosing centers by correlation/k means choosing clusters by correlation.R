# x=as.matrix(read.table("D:\\sagatam das machine learning\\clustering.txt"))
   x=z
   n=8
   k=4
   
##Standard kmeans steps
   centernumbers = sample(nrow(x),n)
   centers = matrix(nrow = n, ncol = ncol(x))
   centers = x[centernumbers, ]
   distance=matrix(nrow=nrow(x),ncol=n)
   for(i in 1:nrow(x)){
   for(j in 1:n){
         distance[i,j]=t(x[i,]-centers[j,])%*%(x[i,]-centers[j,])
         }
   }
   r=cor(distance)
   plot(x,pch=16,col="grey")
   points(centers,pch=seq(1:8),cex=2)
   
   r
   sort(abs(r))[seq(1,64,by=2)]
   
##Gives pairs of points in different clusters 
   c=as.vector(which(abs(r)==sort(abs(r))[1],TRUE)[1,])
   i=3
   repeat{
          c=append(c,as.vector(which(abs(r)==sort(abs(r))[i],TRUE)[1,]))
          i=i+2
          if(i>64) break
         }
   matrix(c,ncol=2,byrow=T)
##p is membership matrix_ If p[i,j]=0 then i and j are in different cluster
   j=1
   p=matrix(1,nrow=n,ncol=n)
   for(j in 1:23){
         c=as.vector(which(abs(r)==sort(abs(r))[2*j-1],TRUE)[1,])
         p[c[1],c[2]]=0
         p[c[2],c[1]]=0
         }
   p
   #clustercheck(p,k)
   block_diagonalisation(p,p,n=nrow(p))
   
   
   

##Function to check if k points are present in different clusters
#   clustercheck <- function(p,k){
#         C=vector()
#         for(i in 1:nrow(p)){
#               if(sum(p[,i])<=nrow(p)-k+1) C=c(C,i)
#         }
#         p_new=p[C,C]
#         print(p_new)
#         if(nrow(p_new)==k){return(TRUE)} 
#         else if(nrow(p_new)<k) {return(FALSE)}
#         else {clustercheck (p_new,k)}                 
#   }

##Alternative method to generate c
#        library(dplyr)
#        library(reshape2)
#        d<-data.frame(distance)
#        d_cor <- as.matrix(cor(d))
#        d_cor_melt <- arrange(melt(d_cor),-abs(value))
#        d_cor_melt[seq(1,64,by=2),]

##Method to obtain he highest correlated variables
#        f=as.vector(which(abs(r)==sort(abs(r))[1225],TRUE)[1,])
#        i=3
#        repeat{
#              f=append(f,as.vector(which(abs(r)==sort(abs(r))[1226-i],TRUE)[1,]))
#              i=i+2
#              if(i>300) break
#        }
#        matrix(f,ncol=2,byrow=T)

#d=matrix(c,ncol=2,byrow=T)
#d[order(d[,1]),] 
