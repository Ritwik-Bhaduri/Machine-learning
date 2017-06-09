x=c(0,1,2,3,4,10,11,12,13,14)
m1=seq(min(x)-20,max(x)+10,by=0.5)
m2=seq(min(x)-10,max(x)+20,by=0.5)
J <- function(x,m1,m2){
  m=c(m1,m1,m1,m1,m1,m2,m2,m2,m2,m2)
  d=t(x-m)%*%(x-m)
  return(d)
} 
cf=matrix(nrow=length(m1),ncol=length(m2))
for(i in 1:length(m1)){
      for(j in 1:length(m2)){
            cf[i,j]=J(x,m1[i],m2[j])
      }                                                               
}

persp(m1,m2,cf,phi=0,theta=0,col=colors()[184])