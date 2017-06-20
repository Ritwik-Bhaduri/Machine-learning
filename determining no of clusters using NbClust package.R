library(NbClust)

y=as.matrix(read.table("D:\\sagatam das machine learning\\R15.txt"))[,c(1,2)]

nb <- NbClust(y,diss=NULL,distance="euclidean",min.nc=2,max.nc=5,method="kmeans",index="all",alphaBeale=0.1)

nb