library(jpeg)
library(raster)
library(grid)
img <- readJPEG("113044.jpg") #reading he image in jpeg format
imgDm <- dim(img)  #obtaining dimensions of data

red <- raster(img[,,1])
green <- raster(img[,,2])
blue <- raster(img[,,3])

R=as.vector(red)
G=as.vector(green)
B=as.vector(blue)
img=as.vector(img)

x=as.matrix(cbind(R,G,B),nrow=imgDm[1],ncol=3) #creates a matrix with column representation of img and rgb channels
r=kmeans(x,10,iter.max = 20,nstart=20)
cluster=r$cluster
centers=r$centers

r <- matrix(nrow=imgDm[1],ncol=imgDm[2])
g <- matrix(nrow=imgDm[1],ncol=imgDm[2])
b <- matrix(nrow=imgDm[1],ncol=imgDm[2])

for(i in 1:imgDm[1]){
      for(j in 1:imgDm[2]){
            r[i,j]=centers[cluster[j+(i-1)*imgDm[2]],1]
            g[i,j]=centers[cluster[j+(i-1)*imgDm[2]],2]
            b[i,j]=centers[cluster[j+(i-1)*imgDm[2]],3]
      }
}

col <- rgb(r,g,b)
dim(col) <- dim(r)
grid.raster(col, interpolate=FALSE)
