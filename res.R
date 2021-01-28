for(ss in c(200,600)){
  resfolder <- paste('/scratch/x..b/BNPprior/n',ss,'/res',sep='')
  for(d in 1:4){
    for(m in 1:4){
      folder <- paste(resfolder,'/d',d,'m',m,sep='')
      setwd(folder)
      b0 <- NULL
      b1 <- NULL
      D11 <- NULL
      D22 <- NULL
      D12 <- NULL
      sigma2 <- NULL
      Ke <- NULL
      alpha <- NULL
      bprec <- NULL
      for(i in 1001:1500){
        fname <- paste('res-',i,'.txt',sep='')
        if (file.exists(fname)){
          sam.res<-read.table(fname)
          
          b0gewek <- sum( sam.res[1,7]<1.96 & sam.res[1,7]>-1.96 )
          b1gewek <- sum( sam.res[2,7]<1.96 & sam.res[2,7]-1.96 )
          D11gewek <- sum( sam.res[3,7]<1.96 & sam.res[3,7]>-1.96 )
          D22gewek <- sum( sam.res[4,7]<1.96 & sam.res[4,7]>-1.96 )
          D12gewek <- sum( sam.res[5,7]<1.96 & sam.res[5,7]>-1.96 )
          alphagewek <- sum( sam.res[8,7]<1.96 & sam.res[8,7]>-1.96 )
          bprecgewek <- sum( sam.res[9,7]<1.96 & sam.res[9,7]>-1.96 )
          
          all.gewek <-b0gewek+b1gewek+D11gewek+D22gewek+D12gewek+alphagewek+bprecgewek
          if(all.gewek==7){
            b0 <- rbind(b0,sam.res[1,])
            b1 <- rbind(b1,sam.res[2,])
            D11 <- rbind(D11,sam.res[3,])
            D22 <- rbind(D22,sam.res[4,])
            D12 <- rbind(D12,sam.res[5,])
            sigma2 <- rbind(sigma2,sam.res[6,])
            Ke <- rbind(Ke,sam.res[7,])
            alpha <- rbind(alpha,sam.res[8,])
            bprec <- rbind(bprec,sam.res[9,])
          }
        }
      }#i
      b0.bias <- mean(b0[,1])-6.2
      b0.rbias <- (mean(b0[,1])-6.2)/6.2*100
      b0.ase <- mean(b0[,2])
      b0.ese <- sd(b0[,1])
      b0.mse <- b0.bias^2+b0.ese^2
      b0.cr<-sum(b0[,5]<6.2 & b0[,6]>6.2)/nrow(b0)
      b0.power<-sum(b0[,5]>0 | b0[,6]<0 )/nrow(b0)
      b0.gewek <- sum( b0[,7]<1.96 & b0[,7]>-1.96 )/nrow(b0)
      b0.time <- apply(b0[,8:10],2,mean)
      b0.res <- c(mean(b0[,1]),b0.bias,b0.rbias,b0.ase,b0.ese,b0.mse,b0.cr,b0.power,b0.gewek,b0.time,nrow(b0))
      
      b1.bias <- mean(b1[,1])-0.3
      b1.rbias <- (mean(b1[,1])-0.3)/0.3*100
      b1.ase <- mean(b1[,2])
      b1.ese <- sd(b1[,1])
      b1.mse <- b1.bias^2+b1.ese^2
      b1.cr<-sum(b1[,5]<0.3 & b1[,6]>0.3)/nrow(b1)
      b1.power<-sum(b1[,5]>0 | b1[,6]<0 )/nrow(b1)
      b1.gewek <- sum( b1[,7]<1.96 & b1[,7]>-1.96 )/nrow(b1)
      b1.time <- apply(b1[,8:10],2,mean)
      b1.res <- c(mean(b1[,1]),b1.bias,b1.rbias,b1.ase,b1.ese,b1.mse,b1.cr,b1.power,b1.gewek,b1.time,nrow(b1))
      
      D11.bias <- mean(D11[,1])-1
      D11.rbias <- (mean(D11[,1])-1)/1*100
      D11.ase <- mean(D11[,2])
      D11.ese <- sd(D11[,1])
      D11.mse <- D11.bias^2+D11.ese^2
      D11.cr<-sum(D11[,5]<1 & D11[,6]>1)/nrow(D11)
      D11.power<-sum(D11[,5]>0 | D11[,6]<0 )/nrow(D11)
      D11.gewek <- sum( D11[,7]<1.96 & D11[,7]>-1.96 )/nrow(D11)
      D11.time <- apply(D11[,8:10],2,mean)
      D11.res <- c(mean(D11[,1]),D11.bias,D11.rbias,D11.ase,D11.ese,D11.mse,D11.cr,D11.power,D11.gewek,D11.time,nrow(D11))
      
      D22.bias <- mean(D22[,1])-0.1
      D22.rbias <- (mean(D22[,1])-0.1)/0.1*100
      D22.ase <- mean(D22[,2])
      D22.ese <- sd(D22[,1])
      D22.mse <- D22.bias^2+D22.ese^2
      D22.cr<-sum(D22[,5]<0.1 & D22[,6]>0.1)/nrow(D22)
      D22.power<-sum(D22[,5]>0 | D22[,6]<0 )/nrow(D22)
      D22.gewek <- sum( D22[,7]<1.96 & D22[,7]>-1.96 )/nrow(D22)
      D22.time <- apply(D22[,8:10],2,mean)
      D22.res <- c(mean(D22[,1]),D22.bias,D22.rbias,D22.ase,D22.ese,D22.mse,D22.cr,D22.power,D22.gewek,D22.time,nrow(D22))
      
      D12.bias <- mean(D12[,1])
      D12.rbias <- (mean(D12[,1])-0)*100
      D12.ase <- mean(D12[,2])
      D12.ese <- sd(D12[,1])
      D12.mse <- D12.bias^2+D12.ese^2
      D12.cr<-sum(D12[,5]<0 & D12[,6]>0)/nrow(D12)
      D12.power<-sum(D12[,5]>0 | D12[,6]<0 )/nrow(D12)
      D12.gewek <- sum( D12[,7]<1.96 & D12[,7]>-1.96 )/nrow(D12)
      D12.time <- apply(D12[,8:10],2,mean)
      D12.res <- c(mean(D12[,1]),D12.bias,D12.rbias,D12.ase,D12.ese,D12.mse,D12.cr,D12.power,D12.gewek,D12.time,nrow(D12))
      
      sigma2.bias <- mean(sigma2[,1])-0.5
      sigma2.rbias <- (mean(sigma2[,1])-0.5)/6.2*100
      sigma2.ase <- mean(sigma2[,2])
      sigma2.ese <- sd(sigma2[,1])
      sigma2.mse <- sigma2.bias^2+sigma2.ese^2
      sigma2.cr<-sum(sigma2[,5]<0.5 & sigma2[,6]>0.5)/nrow(sigma2)
      sigma2.power<-sum(sigma2[,5]>0 | sigma2[,6]<0 )/nrow(sigma2)
      sigma2.gewek <- sum( sigma2[,7]<1.96 & sigma2[,7]>-1.96 )/nrow(sigma2)
      sigma2.time <- apply(sigma2[,8:10],2,mean)
      sigma2.res <- c(mean(sigma2[,1]),sigma2.bias,sigma2.rbias,sigma2.ase,sigma2.ese,sigma2.mse,sigma2.cr,sigma2.power,sigma2.gewek,sigma2.time,nrow(sigma2))
      
      Ke.bias <- mean(Ke[,1])-10
      Ke.rbias <- (mean(Ke[,1])-10)/10*100
      Ke.ase <- mean(Ke[,2])
      Ke.ese <- sd(Ke[,1])
      Ke.mse <- Ke.bias^2+Ke.ese^2
      Ke.cr<-sum(Ke[,5]<10 & Ke[,6]>10)/nrow(Ke)
      Ke.power<-sum(Ke[,5]>0 | Ke[,6]<0 )/nrow(Ke)
      Ke.gewek <- sum( Ke[,7]<1.96 & Ke[,7]>-1.96 )/nrow(Ke)
      Ke.time <- apply(Ke[,8:10],2,mean)
      Ke.res <- c(mean(Ke[,1]),Ke.bias,Ke.rbias,Ke.ase,Ke.ese,Ke.mse,Ke.cr,Ke.power,Ke.gewek,Ke.time,nrow(Ke))
      
      alpha.bias <- NA
      alpha.rbias <- NA
      alpha.ase <- mean(alpha[,2])
      alpha.ese <- sd(alpha[,1])
      alpha.mse <- NA
      alpha.cr<-NA
      alpha.power<-sum(alpha[,5]>0 | alpha[,6]<0 )/nrow(alpha)
      alpha.gewek <- sum( alpha[,7]<1.96 & alpha[,7]>-1.96 )/nrow(alpha)
      alpha.time <- apply(alpha[,8:10],2,mean)
      alpha.res <- c(mean(alpha[,1]),alpha.bias,alpha.rbias,alpha.ase,alpha.ese,alpha.mse,alpha.cr,alpha.power,alpha.gewek,alpha.time,nrow(alpha))
      
      bprec.bias <- NA
      bprec.rbias <- NA
      bprec.ase <- mean(bprec[,2])
      bprec.ese <- sd(bprec[,1])
      bprec.mse <- NA
      bprec.cr<-NA
      bprec.power<-sum(bprec[,5]>0 | bprec[,6]<0 )/nrow(bprec)
      bprec.gewek <- sum( bprec[,7]<1.96 & bprec[,7]>-1.96 )/nrow(bprec)
      bprec.time <- apply(bprec[,8:10],2,mean)
      bprec.res <- c(mean(bprec[,1]),bprec.bias,bprec.rbias,bprec.ase,bprec.ese,bprec.mse,bprec.cr,bprec.power,bprec.gewek,bprec.time,nrow(bprec))
    
      result <- rbind(b0.res,b1.res,D11.res,D22.res,D12.res,sigma2.res,Ke.res,alpha.res,bprec.res)
      setwd(resfolder)
      tblname<-paste('ctable-d',d,'m',m,'.txt',sep='')
      write.table(result,tblname,row.names=FALSE,col.names=FALSE)
      }#m
  }#d
}#ss