rm(list=ls())
graphics.off()
# install.packages("jpeg")
# install.packages("bmp")
# install.packages("pixmao")
require(grDevices)
library('jpeg')
library('bmp')
library('pixmap')
# library('rgl')

rotate <- function(x) t(apply(x, 2, rev))

FeatureExtraction <- function(x) 
{
    vetor_features <- vector(mode = "numeric")
    vetor_features <- c(vetor_features, sum(sum(as.numeric(x)))) ## nъmero de 1s

    x_esquerda <- x[, 1:dim(x)[2]/2]
    x_direita <- x[, (dim(x)[2]/2):dim(x)[2]]
    x_cima <- x[1:(dim(x)[1]/2), ]
    x_baixo <- x[(dim(x)[1]/2):dim(x)[1], ]
    vetor_features <- c(vetor_features, sum(sum(as.numeric(x_esquerda)))) ## 
    vetor_features <- c(vetor_features, sum(sum(as.numeric(x_direita)))) ## 
    vetor_features <- c(vetor_features, sum(sum(as.numeric(x_cima)))) ## 
    vetor_features <- c(vetor_features, sum(sum(as.numeric(x_baixo)))) ## 
  
}

similarityBetween2Images <- function(char1,char2)
{
  
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

for( i in 1:25){
    
    for(j in 1:25){
        
        m <- M[j:(j + 43 - 1), i:(i + 32 - 1)]
        fm <- features(m)
        fK <- features(K)
        Mcorr[j, i] <- sim(fK, fm)
  }
  
}

