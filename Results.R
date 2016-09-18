
dirname = getSrcDirectory(function(x) {x})
setwd(dirname)
source('Datasets.R')

cores = rainbow(2)

for (i in 1:10){
  auxname = paste("Resultados",i,".dat",sep="")
  x = read.table(auxname)
  
  auxnamegraphic = names.datasets[i]
  auxnamegraphic = paste("Banco",auxnamegraphic,sep=" ")
  
  auxlegend = c("Dinamico","Longitudinal")
  
  ylim = range(as.vector(c(x[,1],x[,2])))
  pdf(paste(names.datasets[i],".pdf",sep=""))
  par(mfrow = c(2,1))
  plot(x[,1],type = "l",main = auxnamegraphic,ylim=ylim,col=cores[1],xlab="Passos",ylab="nmi")
  lines(x[,2],col=cores[2])
  mtext("Acurácia",3)
  legend("topleft",auxlegend,col=cores[1:2],lty=1,bg="transparent",horiz=T,yjust=1,bty="n")
  
  ylim = range(as.vector(c(x[,3],x[,4])))
  plot(x[,3],type="l",ylim=ylim,col=cores[1],xlab="Passos",ylab="segundos")
  lines(x[,4],col=cores[2])
  mtext("Tempo",3)
  legend("topleft",auxlegend,col=cores[1:2],lty=1,bg="transparent",horiz=T,yjust=1,bty="n")
  dev.off()
}