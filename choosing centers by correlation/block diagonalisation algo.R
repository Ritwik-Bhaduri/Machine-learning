l=(1:8)
block_diagonalisation <- function(p,q,alpha=1,n,l=1:n,cluster=list(),iter=1){      
       if(alpha==1){
             if(!identical(diag(p),seq(1,1,length=n))) return("Matrix not diagonalisable")
       }
       D1=vector()
       for(i in 1:nrow(q)){
             if(q[i,1]==1) D1=c(D1,i)
       }
       D2=seq(1:nrow(q))[!seq(1:nrow(q)) %in% D1]
       D3=c(D1,D2)
       cluster[[iter]]=l[alpha+D1-1]
       l[alpha:8]=l[alpha+D3-1]
       p[seq(alpha,n),seq(alpha,n)]=q[D3,D3]
       q=q[D2,D2]
       alpha=alpha+length(D1)
       if(alpha>=n) {return(list(matrix=p,cluster=cluster,l=l))
       }else return(block_diagonalisation(p,q,alpha,n,l,cluster,iter+1))
       
       
}                                                     

block_diagonalisation(p,p,n=nrow(p))
