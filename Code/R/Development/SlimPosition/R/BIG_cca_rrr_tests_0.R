rm(list=ls())

##tests
  ## need an RRR test.

library(rrr)

### source a bunch of stuff...
source('R/tolerance.svd.R')
source('R/gsvd.R')
source('R/invert.rebuild_matrix.R')
source('R/power.rebuild_matrix.R')
source('R/isDiagonal.matrix.R')
source('R/expo.scale.R')
source('R/sp.cca.R')
source('R/sp.rrr.R')
load('data/two.table.wine.rda')

X <- wine$subjective
Y <- wine$objective


cca.res <- cancor(X,Y)
rrr.res <- rrr(X,Y)

my.cca.res <- sp.cca(X,Y,scale.X = F,scale.Y = F,compact = F)
my.rrr.res <- sp.rrr(X,Y,scale.X = F,scale.Y = F,compact = F)


X <- expo.scale(X,scale=F)
Y <- expo.scale(Y,scale=F)


cca.dat <- t(power.rebuild_matrix(X,-1/2)) %*% (power.rebuild_matrix(X,-1/2) %*% (t(X) %*% Y) %*% power.rebuild_matrix(t(Y),-1/2)) %*% power.rebuild_matrix(Y,-1/2)
rrr.dat <- t(power.rebuild_matrix(X,-1/2)) %*% (power.rebuild_matrix(X,-1/2) %*% (t(X) %*% Y))

cca.t.svd.res <- tolerance.svd(cca.dat)
rrr.t.svd.res <- tolerance.svd(rrr.dat)

cca.t.svd.res$p <- power.rebuild_matrix(t(X),-1/2) %*% (power.rebuild_matrix(X,-1/2) %*% cca.t.svd.res$u)
cca.t.svd.res$q <- power.rebuild_matrix(t(Y),-1/2) %*% (power.rebuild_matrix(Y,-1/2) %*% cca.t.svd.res$v)

rrr.t.svd.res$p <- power.rebuild_matrix(t(X),-1/2) %*% (power.rebuild_matrix(X,-1/2) %*% rrr.t.svd.res$u)
rrr.t.svd.res$q <- rrr.t.svd.res$v

  ### i can get these much more directly...
cca.t.svd.res$lx <- (X %*% cca.t.svd.res$p)# * matrix(1/cca.t.svd.res$d,nrow(X),length(cca.t.svd.res$d),byrow=T)
cca.t.svd.res$ly <- (Y %*% cca.t.svd.res$q)# * matrix(1/cca.t.svd.res$d,nrow(Y),length(cca.t.svd.res$d),byrow=T)

rrr.t.svd.res$lx <- (X %*% rrr.t.svd.res$p)# * matrix(1/rrr.t.svd.res$d,nrow(X),length(rrr.t.svd.res$d),byrow=T)
rrr.t.svd.res$ly <- (Y %*% rrr.t.svd.res$q)# * matrix(1/rrr.t.svd.res$d,nrow(Y),length(rrr.t.svd.res$d),byrow=T)


  ### these need to be much much more efficient.
cca.t.svd.res$fi <- t(X) %*% (cca.t.svd.res$lx * matrix(cca.t.svd.res$d,nrow(X),length(cca.t.svd.res$d),byrow=T))
cca.t.svd.res$fj <- t(Y) %*% (cca.t.svd.res$ly * matrix(cca.t.svd.res$d,nrow(Y),length(cca.t.svd.res$d),byrow=T))

rrr.t.svd.res$fi <- t(X) %*% (rrr.t.svd.res$lx * matrix(rrr.t.svd.res$d,nrow(X),length(rrr.t.svd.res$d),byrow=T))
rrr.t.svd.res$fj <- rrr.t.svd.res$q * matrix(rrr.t.svd.res$d,nrow(rrr.t.svd.res$q),length(rrr.t.svd.res$d),byrow=T)




## tset the conditions/LVs here.



t(cca.t.svd.res$lx) %*% cca.t.svd.res$ly
diag(t(cca.t.svd.res$lx) %*% cca.t.svd.res$ly)

t(rrr.t.svd.res$lx) %*% rrr.t.svd.res$ly
diag(t(rrr.t.svd.res$lx) %*% rrr.t.svd.res$ly)
