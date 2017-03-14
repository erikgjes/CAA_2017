table.plot.function<-function(x){
  plot(table(colSums(x)),xaxt="n",xlim=c(0,max(colSums(x))),lwd=6,
       ylab="Number of Nodes",xlab="Number of Ties Per Node",yaxt="n")
  axis(1,labels=seq(from=0,to=max(colSums(x)),by=1),at=seq(from=0,to=max(colSums(x)),by=1))
  axis(2,labels=seq(from=0,to=max(table(colSums(x))),by=1),at=seq(from=0,to=max(table(rowSums(x))),by=1))
}

NetBoot.totals<-function(x,y,z){
  cent.cor=c()
  total.cor=NULL
  for (i in 1:y){
    for (j in 1:1000) {
      sampled.nodes <- sort(sample(1:nrow(x),size=nrow(x)-nrow(x)*(i/10),replace=F))
      removed.nodes <- setdiff(1:nrow(x),sampled.nodes)
      samp<-x[sampled.nodes,sampled.nodes]
      cent.samp<-z(samp)
      orig.cent.samp<-z(x)
      rev.cent.samp<-orig.cent.samp[c(-removed.nodes)]
      cent.cor[j]=cor(cent.samp,rev.cent.samp)
      total.cor[i]=mean(cent.cor,na.rm=T)
    }
  }
  #return(total.cor)
  plot(total.cor,type="n",xlim=c(1,y),ylim=c(0,1),xaxt="n",
       ylab="Correlation",xlab="Sampling Fraction")
  lines(total.cor,col="red",lwd=3)
  points(total.cor,col="red",pch=16)
  labs=seq(from=10,to=90,by=10)
  axis(side=1,labels=rev(labs),at=seq(from=1,to=9,by=1))
  return(total.cor)
}
