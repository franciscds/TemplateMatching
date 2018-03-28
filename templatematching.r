#setwd("C:/Users/fcdossantos/Desktop/UFMG 201801/Reconhecimento de Padrões/Template Matching")
rm(list=ls())
graphics.off()
rm(list=ls())
graphics.off()
require('latticeExtra') 
# install.packages("jpeg")
#  install.packages("bmp")
#  install.packages("pixmap")
#  install.packages("lattice")
 
#require(grDevices)
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

similaridade <- function(feature_train,feature_test) 
{
  sim_mat <- matrix(seq(1,5,1), ncol=25, nrow=25);
  colum <-matrix(seq(1,5,1), ncol=1, nrow=25);
  for( i in 1: 25){
    for(j in 1:25){
      sim_mat[i,j] = dist(rbind(feature_train[i,],feature_test[j,]))
      if( sim_mat[i,j]==0)
      {
        sim_mat[i,j] = 100000000;
      }
    }
  }
  for(i in 1:25)
  {
    colum[i,] <- which(sim_mat[i,] == min(sim_mat[i,]))
  }
  
  colum
  
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
feature_train = matrix(seq(1,5,1), ncol=3, nrow=25)
feature_test = matrix(seq(1,5,1), ncol=3, nrow=25)

for( i in 1:25){
  
  feature_train[i,] = extrairFeatures(xe[i,])
  feature_test[i,] = extrairFeatures(xt[i,])
  
}



# FacesID <- c(seq(1:5), seq(1:5), seq(1:5), seq(1:5), seq(1:5))
# 
# dat <- data.frame(FacesID, Media=feature_train[,1], Variancia=feature_train[,2])
# 
# xyplot(Variancia ~ Media, groups=dat$FacesID, data = dat, cex=2)

# dat_teste <- data.frame(TipoConjunto="Teste", FacesID, 
#                         Feature1=features_teste[,1], Feature2=features_teste[,2])
# 
# data_total = smartbind(dat_train, dat_teste) # Merge data frames
# 
# TemaTrain <- standard.theme(col = FALSE)
# TemaTrain$superpose.symbol$pch <- 1
# 
# TemaTeste <- standard.theme(col = FALSE)
# TemaTeste$superpose.symbol$pch <- 3
# 
# 
# xlim = c(min(data_total$Feature1)*0.95 , 1.05*max(data_total$Feature1))
# ylim = c(min(data_total$Feature2)*0.95 , 1.05*max(data_total$Feature2))
# 
# p1 <- xyplot(Feature2 ~ Feature1, 
#              groups= dat_train$FacesID, 
#              data = dat_train, cex=1,
#              col = list("green","red","blue","yellow","black"),
#              par.settings = TemaTrain,
#              xlim = xlim,
#              ylim = ylim)
# 
# p2 <- xyplot(Feature2 ~ Feature1, 
#              groups= dat_teste$FacesID, 
#              data = dat_teste, cex=1,
#              col = list("green","red","blue","yellow","black"),
#              par.settings = TemaTeste,
#              xlim = xlim,
#              ylim = ylim)
# 
# print(p1 + as.layer(p2)) 


 x <- similaridade(feature_train,feature_test)
 count <- matrix(0,ncol=1, nrow=5)
for( i in 1:25)
{
  if(ye[i,]== ye[x[i,],])
    {
      count[ye[i,],]= count[ye[i,],]+1;
    }
    
}
 
