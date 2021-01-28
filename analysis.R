library(rjags)

time0 <- proc.time()
##initial values
inits <- list(Inv_cov = structure(.Data = c(1.0,0.0,0.0,10.0), .Dim = c(2,2)),
  alpha = 1.0, bL = c(6.2),bS = c(0.3),bprece = 0.5,".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 115)

##prepare the data
data<-read.table('/scratch/x..b/BNPprior/n600/data/replace0/replace1.txt')
N <- nrow(data)
jagsdata <- list(N=N, T=4,y=as.matrix(data))

##run jags
setwd('/scratch/x..b/BNPprior/n600/analysis')
model <- jags.model(file="replace2.txt", data=jagsdata, inits=inits, n.chains = 1, n.adapt=25000)

model.samples <- coda.samples(model, c("para"), n.iter=25000)

model.res <- as.mcmc(do.call(rbind,model.samples))

time1 <- proc.time()-time0
time1 <- matrix(rep(time1,nrow(summary(model.res)[[1]])),nrow(summary(model.res)[[1]]),5,byrow=TRUE)

res <- cbind(summary(model.res)[[1]],HPDinterval(model.res),geweke.diag(model.res)[[1]],time1[,1:3])
filename <- paste('/scratch/x..b/BNPprior/n600/res/replace3/replace4.txt',sep='')
write.table(res,filename,row.names=FALSE,col.names=FALSE)
