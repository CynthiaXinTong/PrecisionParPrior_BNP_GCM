for(ss in c(200,600)){
  resfolder <- paste('/scratch/x..b/BNPprior/n',ss,'/res',sep='')
  for(d in 1:4){
    for(m in 1:4){
      flag <- 0
      folder <- paste(resfolder,'/d',d,'m',m,sep='')
      setwd(folder)
      for(i in 1001:1500){
        fname <- paste('res-',i,'.txt',sep='')
        if (file.exists(fname)){
          sam.res<-read.table(fname)
          b0 <- sam.res[1,7]
          b1 <- sam.res[2,7]
          D11 <- sam.res[3,7]
          D22 <- sam.res[4,7]
          D12 <- sam.res[5,7]
          sigma2 <- sam.res[6,7]
          Ke <- sam.res[7,7]
          alpha <- sam.res[8,7]
          bprec <- sam.res[9,7]
          
          b0.gewek <- sum( b0<1.96 & b0>-1.96 )
          
          b1.gewek <- sum( b1<1.96 & b1>-1.96 )
          
          D11.gewek <- sum( D11<1.96 & D11>-1.96 )
          
          D22.gewek <- sum( D22<1.96 & D22>-1.96 )
          
          D12.gewek <- sum( D12<1.96 & D12>-1.96 )
          
          sigma2.gewek <- sum( sigma2<1.96 & sigma2>-1.96 )
          
          Ke.gewek <- sum( Ke<1.96 & Ke>-1.96 )
          
          alpha.gewek <- sum( alpha<1.96 & alpha>-1.96 )
          
          bprec.gewek <- sum( bprec<1.96 & bprec>-1.96 )
          
          all.gewek <-b0.gewek+b1.gewek+D11.gewek+D22.gewek+D12.gewek+alpha.gewek+bprec.gewek
          if(all.gewek==7){flag<-flag+1}
        }
      }#i
      
      setwd(resfolder)
      write.table(flag,"converge.txt",row.names=FALSE,col.names=FALSE,append=TRUE)
      }#m
  }#d
}#ss