
source('./R/gsvd.R')
source('./R/tolerance.svd.R')
source('./R/power.rebuild_matrix.R')
source('./R/invert.rebuild_matrix.R')
source('./R/isDiagonal.matrix.R')
source('./R/sp.cca.R')
source('./R/sp.cca_big.R')
source('./R/expo.scale.R')
source('./R/sp.rrr.R')
source('./R/sp.rrr_big.R')

load('./data/two.table.wine.rda')

X <- as.matrix(wine$objective)
Y <- as.matrix(wine$subjective)

cca.res <- sp.cca(X,Y,compact=F)
cca_big.res <- sp.cca_big(X,Y,compact=F)



rrr.res <- sp.rrr(X,Y,compact=F)
rda.res <- sp.rda(X,Y,compact=F)
rrr_big.res <- sp.rrr_big(X,Y,compact=F)
rda_big.res <- sp.rda_big(X,Y,compact=F)


