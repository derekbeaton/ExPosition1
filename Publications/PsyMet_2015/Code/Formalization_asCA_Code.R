rm(list=ls())
library(ExPosition)
data(snps.druguse)

X <- makeNominalData(snps.druguse$DATA1)
Ox <- X/sum(X)
Y <- makeNominalData(snps.druguse$DATA2)
Oy <- Y/sum(Y)

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


R <- t(X) %*% Y
Or <- R/sum(R)
Er <- mx %o% my
Zr <- Or - Er

gsvd.res <- genPDQ(Zr,Wx,Wy)

##Component Scores
Fj <- Wx %*% gsvd.res$p %*% gsvd.res$Dd
Fk <- Wy %*% gsvd.res$q %*% gsvd.res$Dd

Fx <- Ox %*% Fj %*% diag(gsvd.res$Dv^-1)
Fy <- Oy %*% Fk %*% diag(gsvd.res$Dv^-1)

##Latent variables
Lx <- Fx * sqrt(I)
Ly <- Fy * sqrt(I) 
	##a test:
		
gsvd.res$Dv
diag(t(Lx) %*% Ly)