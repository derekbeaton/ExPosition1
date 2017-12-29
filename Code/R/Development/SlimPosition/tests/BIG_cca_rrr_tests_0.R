rm(list=ls())

##tests
  ## need an RRR test.

#library(rrr)

### source a bunch of stuff...
# source('R/tolerance.svd.R')
# source('R/gsvd.R')
# source('R/invert.rebuild_matrix.R')
# source('R/power.rebuild_matrix.R')
# source('R/isDiagonal.matrix.R')
# source('R/expo.scale.R')
# source('R/sp.cca.R')
# source('R/sp.rrr.R')
# load('data/two.table.wine.rda')
data("two.table.wine")

X <- wine$subjective
Y <- wine$objective


cca.res <- cancor(X,Y)

my.cca.res <- sp.cca(X,Y,scale.X = F,scale.Y = F,compact = F)


## for CCA P/Q = xcoef/ycoef

  ## for reference
# res <- gsvd(
#   invert.rebuild_matrix(crossprod(X)) %*% t(X) %*% Y %*% invert.rebuild_matrix(crossprod(Y)),
#   crossprod(X),
#   crossprod(Y),
#   k = k
# )


X <- expo.scale(X,scale=F)
Y <- expo.scale(Y,scale=F)


cca.dat <- t(power.rebuild_matrix(X,-1/2)) %*% (power.rebuild_matrix(X,-1/2) %*% (t(X) %*% Y) %*% power.rebuild_matrix(t(Y),-1/2)) %*% power.rebuild_matrix(Y,-1/2)



cca.t.svd.res <- tolerance.svd(cca.dat)

cca.t.svd.res$u / my.cca.res$u
cca.t.svd.res$v / my.cca.res$v


cca.t.svd.res$p <- power.rebuild_matrix(t(X),-1/2) %*% (power.rebuild_matrix(X,-1/2) %*% cca.t.svd.res$u)
cca.t.svd.res$q <- power.rebuild_matrix(t(Y),-1/2) %*% (power.rebuild_matrix(Y,-1/2) %*% cca.t.svd.res$v)

cca.t.svd.res$p / my.cca.res$p
cca.t.svd.res$q / my.cca.res$q

### i can get these much more directly...

cca.t.svd.res$lx <- (X %*% cca.t.svd.res$p)# * matrix(1/cca.t.svd.res$d,nrow(X),length(cca.t.svd.res$d),byrow=T)
cca.t.svd.res$ly <- (Y %*% cca.t.svd.res$q)# * matrix(1/cca.t.svd.res$d,nrow(Y),length(cca.t.svd.res$d),byrow=T)

### these need to be much much more efficient.
cca.t.svd.res$fi <- t(X) %*% (cca.t.svd.res$lx * matrix(cca.t.svd.res$d,nrow(X),length(cca.t.svd.res$d),byrow=T))
cca.t.svd.res$fj <- t(Y) %*% (cca.t.svd.res$ly * matrix(cca.t.svd.res$d,nrow(Y),length(cca.t.svd.res$d),byrow=T))


t(cca.t.svd.res$lx) %*% cca.t.svd.res$ly
diag(t(cca.t.svd.res$lx) %*% cca.t.svd.res$ly)



rrr.res <- rrr(X,Y)
my.rrr.res <- sp.rrr(X,Y,scale.X = F,scale.Y = F,compact = F)
rrr.dat <- t(power.rebuild_matrix(X,-1/2)) %*% (power.rebuild_matrix(X,-1/2) %*% (t(X) %*% Y))
rrr.t.svd.res <- tolerance.svd(rrr.dat)
rrr.t.svd.res$p <- power.rebuild_matrix(t(X),-1/2) %*% (power.rebuild_matrix(X,-1/2) %*% rrr.t.svd.res$u)
rrr.t.svd.res$q <- rrr.t.svd.res$v

  ### i can get these much more directly...
rrr.t.svd.res$lx <- (X %*% rrr.t.svd.res$p)# * matrix(1/rrr.t.svd.res$d,nrow(X),length(rrr.t.svd.res$d),byrow=T)
rrr.t.svd.res$ly <- (Y %*% rrr.t.svd.res$q)# * matrix(1/rrr.t.svd.res$d,nrow(Y),length(rrr.t.svd.res$d),byrow=T)
### these need to be much much more efficient.
rrr.t.svd.res$fi <- t(X) %*% (rrr.t.svd.res$lx * matrix(rrr.t.svd.res$d,nrow(X),length(rrr.t.svd.res$d),byrow=T))
rrr.t.svd.res$fj <- rrr.t.svd.res$q * matrix(rrr.t.svd.res$d,nrow(rrr.t.svd.res$q),length(rrr.t.svd.res$d),byrow=T)

t(rrr.t.svd.res$lx) %*% rrr.t.svd.res$ly
diag(t(rrr.t.svd.res$lx) %*% rrr.t.svd.res$ly)
