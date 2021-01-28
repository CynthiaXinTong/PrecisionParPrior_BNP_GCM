##------------------------------##
## generate data
##------------------------------##
library(mvtnorm)

set.seed(111)
beta	<-	c(6.2,0.3)
##alpha <- 1

T <- 4
covD <- 0
varE <- 0.5

outcenter <- rpois(60,8)
outcenter <- unique(outcenter[which(outcenter>=5)])[1:10]

for(N in c(200,600)){

newf <- paste('mkdir /scratch/x..b/BNPprior/n',N,sep='')
system(newf)

#create subfolders:analysis,res,data
subf1 <- paste('mkdir /scratch/x..b/BNPprior/n',N,'/analysis',sep='')
system(subf1)
subf2 <- paste('mkdir /scratch/x..b/BNPprior/n',N,'/res',sep='')
system(subf2)
subf3 <- paste('mkdir /scratch/x..b/BNPprior/n',N,'/data',sep='')
system(subf3)

#create subsubfolders:d1,d2,d3,d4;d1m1,..,d4m4
for(d in 1:4){
	for(m in 1:4){
	subsub2 <- paste('mkdir /scratch/x..b/BNPprior/n',N,'/res/d',d,'m',m,sep='')
	system(subsub2)
	}
subsub3 <- paste('mkdir /scratch/x..b/BNPprior/n',N,'/data/d',d,sep='')
system(subsub3)
}

NT	<-	N*T

lambda<-	matrix(c(rep(1,T),seq(0,T-1,1)),T,2)
mu	<-	lambda%*%beta

D	<-	array(c(1,covD,covD,0.1), dim=c(2,2))



for(i in 1001:1500){
u 	<-	rmvnorm(N,c(0,0),D)

## d1
e	<-	matrix(rnorm(NT,0,sqrt(varE)),N,T)
y1	<-	matrix(rep(mu,N),N,T,byrow=TRUE)+t(lambda%*%t(u))+e

path <- paste('/scratch/x..b/BNPprior/n',N,'/data/d1',sep='')
setwd(path)
filename<-paste('data',i,'.txt', sep='')
write.table(y1,filename,row.names=FALSE,col.names=FALSE)

## d2: 5% outliers

N.o <- N*0.05  ## number of outliers at each wave

e1 <- rnorm(N-N.o,0,sqrt(varE))
e1.o <- NULL
for(o in 1:10){
e1.o <- c(e1.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e1 <- c(e1,e1.o)
e1 <- sample(e1)

e2 <- rnorm(N-N.o,0,sqrt(varE))
e2.o <- NULL
for(o in 1:10){
  e2.o <- c(e2.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e2 <- c(e2,e2.o)
e2 <- sample(e2)

e3 <- rnorm(N-N.o,0,sqrt(varE))
e3.o <- NULL
for(o in 1:10){
  e3.o <- c(e3.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e3 <- c(e3,e3.o)
e3 <- sample(e3)

e4 <- rnorm(N-N.o,0,sqrt(varE))
e4.o <- NULL
for(o in 1:10){
  e4.o <- c(e4.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e4 <- c(e4,e4.o)
e4 <- sample(e4)

e	<-	cbind(e1,e2,e3,e4)
y2	<-	matrix(rep(mu,N),N,T,byrow=TRUE)+t(lambda%*%t(u))+e
path <- paste('/scratch/x..b/BNPprior/n',N,'/data/d2',sep='')
setwd(path)
filename<-paste('data',i,'.txt', sep='')
write.table(y2,filename,row.names=FALSE,col.names=FALSE)


## d3: 10% outliers

N.o <- N*0.1  ## number of outliers at each wave

e1 <- rnorm(N-N.o,0,sqrt(varE))
e1.o <- NULL
for(o in 1:10){
  e1.o <- c(e1.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e1 <- c(e1,e1.o)
e1 <- sample(e1)

e2 <- rnorm(N-N.o,0,sqrt(varE))
e2.o <- NULL
for(o in 1:10){
  e2.o <- c(e2.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e2 <- c(e2,e2.o)
e2 <- sample(e2)

e3 <- rnorm(N-N.o,0,sqrt(varE))
e3.o <- NULL
for(o in 1:10){
  e3.o <- c(e3.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e3 <- c(e3,e3.o)
e3 <- sample(e3)

e4 <- rnorm(N-N.o,0,sqrt(varE))
e4.o <- NULL
for(o in 1:10){
  e4.o <- c(e4.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e4 <- c(e4,e4.o)
e4 <- sample(e4)

e	<-	cbind(e1,e2,e3,e4)
y3	<-	matrix(rep(mu,N),N,T,byrow=TRUE)+t(lambda%*%t(u))+e
path <- paste('/scratch/x..b/BNPprior/n',N,'/data/d3',sep='')
setwd(path)
filename<-paste('data',i,'.txt', sep='')
write.table(y3,filename,row.names=FALSE,col.names=FALSE)

## d4: 20% outliers

N.o <- N*0.2  ## number of outliers at each wave

e1 <- rnorm(N-N.o,0,sqrt(varE))
e1.o <- NULL
for(o in 1:10){
  e1.o <- c(e1.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e1 <- c(e1,e1.o)
e1 <- sample(e1)

e2 <- rnorm(N-N.o,0,sqrt(varE))
e2.o <- NULL
for(o in 1:10){
  e2.o <- c(e2.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e2 <- c(e2,e2.o)
e2 <- sample(e2)

e3 <- rnorm(N-N.o,0,sqrt(varE))
e3.o <- NULL
for(o in 1:10){
  e3.o <- c(e3.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e3 <- c(e3,e3.o)
e3 <- sample(e3)

e4 <- rnorm(N-N.o,0,sqrt(varE))
e4.o <- NULL
for(o in 1:10){
  e4.o <- c(e4.o,rnorm(N.o/10,outcenter[o]*sqrt(varE),sqrt(varE)))}
e4 <- c(e4,e4.o)
e4 <- sample(e4)

e	<-	cbind(e1,e2,e3,e4)
y4	<-	matrix(rep(mu,N),N,T,byrow=TRUE)+t(lambda%*%t(u))+e
path <- paste('/scratch/x..b/BNPprior/n',N,'/data/d4',sep='')
setwd(path)
filename<-paste('data',i,'.txt', sep='')
write.table(y4,filename,row.names=FALSE,col.names=FALSE)

}

}#N

