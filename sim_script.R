setwd('/scratch/x..b/BNPprior/n600/analysis')
rr <- 0
for(d in 1:4){
  for (m in 1:4){
  for(i in 1001:1500){

		rr <- rr+1
		newname <- paste('R',rr,'.R',sep='')
		name  <-	paste('d',d,sep='') 
		repfolder<-paste("sed 's/replace0/",name,"/g' analysis.R > temp.R", sep='')
		system(repfolder)
		
		name  <-	paste('data',i,sep='') 
		repfolder<-paste("sed 's/replace1/",name,"/g' temp.R > temp0.R", sep='')
		system(repfolder)
		system('rm -f temp.R')
		
		name  <-	paste('model',m,sep='') 		
		repfolder<-paste("sed 's/replace2/",name,"/g' temp0.R > temp1.R", sep='')
		system(repfolder)
		system('rm -f temp0.R')
		
		name  <-	paste('d',d,'m',m,sep='') 
		repfolder<-paste("sed 's/replace3/",name,"/g' temp1.R > temp2.R", sep='')
		system(repfolder)
		system('rm -f temp1.R')
		
		name  <-	paste('res-',i,sep='') 
		repfolder<-paste("sed 's/replace4/",name,"/g' temp2.R > ",newname, sep='')
		system(repfolder)
		system('rm -f temp2.R')
		
		name <- paste('d',d,'m',m,"-",i,sep='')
		write.table(c(rr,name),'record.txt',append=TRUE,row.names=FALSE,col.names=FALSE)
  }

	}
}
