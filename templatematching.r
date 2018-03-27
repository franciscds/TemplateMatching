#setwd("C:/Users/fcdossantos/Desktop/UFMG 201801/Reconhecimento de Padrões/Template Matching")
rm(list=ls())
graphics.off()
rm(list=ls())
graphics.off()
# ++++++++++install.packages("jpeg")
# install.packages("bmp")
# install.packages("pixmap")
# #require(grDevices)
 library('jpeg')
 library('bmp')
 library('pixmap')
 # #ibrary('rgl')
 library('lattice')

rotate <- function(x) t(apply(x, 2, rev))

extrairFeatures <- function(img_vec) 
{
  #img_mat <- makematrix(img_vec)
  f = matrix(0,3,1)
  f[1,1] = sum(img_vec)
  f[2,1] = var(img_vec)
  f[3,1] = sd(img_vec)
  f
}

makematrix <- function(vet)
{ 
  m <-matrix(vet,nrow=56, ncol=46)
  m
}


#Leitura de dados ###################################################################################
xt <- c();
xe <- c();

for(i in 1:5){
  
  for(j in 1:5){
    
    path_xt <- paste(getwd(), paste(paste("/f", i, sep=""), paste(paste("teste",j, sep=""),".bmp", sep=""), sep="") ,sep="");
    path_xe <- paste(getwd(), paste(paste("/F", i, sep=""), paste(paste("TESTE",j,sep=""),"R.BMP",sep=""),sep=""),sep="");
    
    img_train <- read.bmp(path_xt);
    img_test <- read.bmp(path_xe);
    
    xt <- rbind(xt, t(as.matrix(as.vector(img_train))))
    xe <- rbind(xe, t(as.matrix(as.vector(img_test))))
    
  }
  
}

yt <- matrix(seq(1,5,1), ncol=1, nrow=25);#yt = [1,2,3,4,5,1,2,3,4...];
ye <- matrix(seq(1,5,1), ncol=1, nrow=25);

image(matrix(xe[1,],nrow = 56,ncol=46))
image(matrix(xe[10,],nrow = 56,ncol=46))

#extrai features

n_features = 4;
features = matrix(seq(1,5,1), ncol=3, nrow=25)

for( i in 1:25){
  features[i,] = extrairFeatures(xe[i,])
}

FacesID <- c(seq(1:5), seq(1:5), seq(1:5), seq(1:5), seq(1:5))

dat <- data.frame(FacesID, Media=features[,1], Variancia=features[,2])

xyplot(Variancia ~ Media, groups=dat$FacesID, data = dat, cex=2)












