rm(list=ls())
library(ExPosition)
data(snps.druguse)


X <- makeNominalData(snps.druguse$DATA1)
Y <- makeNominalData(snps.druguse$DATA2)

I <- nrow(X)
J <- ncol(X)
K <- ncol(Y)

Nx <- ncol(snps.druguse$DATA1)
Ny <- ncol(snps.druguse$DATA2)

mx <- as.vector((matrix(1,1,I) %*% X %*% matrix(1,J,1))^-1 %*% (matrix(1,1,I) %*% X))##1X1^-1 1X
my <- as.vector((matrix(1,1,I) %*% Y %*% matrix(1,K,1))^-1 %*% (matrix(1,1,I) %*% Y)) ##1Y1^-1 1Y
	##alternative: 
	#mx <- colSums(X)/sum(X)
	#my <- colSums(Y)/sum(Y)
Wx <- diag(1/mx)
Wy <- diag(1/my)


Zx <- (X - (matrix(1,I,1) %*% ( matrix(1,1,I) %*% X * (1/I)) ) ) * (1/(Nx * sqrt(I)))
Zy <- (Y - (matrix(1,I,1) %*% ( matrix(1,1,I) %*% Y * (1/I)) ) ) * (1/(Ny * sqrt(I)))
Zr <- t(Zx) %*% Zy


gsvd.res <- genPDQ(Zr,Wx,Wy)

##Component Scores
Fj <- Wx %*% gsvd.res$p %*% gsvd.res$Dd
Fk <- Wy %*% gsvd.res$q %*% gsvd.res$Dd

##Latent variables
Lx <- Zx %*% Wx %*% gsvd.res$p
Ly <- Zy %*% Wy %*% gsvd.res$q
	##a test:
	
gsvd.res$Dv
diag(t(Lx) %*% Ly)
