## test for canonical correlation analysies (CCA) and correspondence analysis (CA): the two most important techniques for the GSVD test.

source('./R/gsvd.R')
source('./R/tolerance.svd.R')
source('./R/power.rebuild_matrix.R')
source('./R/invert.rebuild_matrix.R')
source('./R/isDiagonal.matrix.R')

  #authors data for CA
load('./data/authors.rda')


## let's just do this directly through the GSVD and do all the preprocessing here.
sum.data <- sum(authors$ca$data)
wi <- rowSums(authors$ca$data)/sum.data
wj <- colSums(authors$ca$data)/sum.data

ca.res <- gsvd( (authors$ca$data/sum.data) - (wi %o% wj), 1/wi, 1/wj)

  #wine data for CCA
load('./data/two.table.wine.rda')

X <- scale(wine$objective, center = T, scale = T)
Y <- scale(wine$subjective, center = T, scale = T)

cca.res <- gsvd(t(X) %*% Y, crossprod(X), crossprod(Y))




### The most important results that need to match with any other implementation of the SVD are:
  # $d (or $d.orig if using a reduced set of vectors) -- the singular values
  # $u the right singular vectors
  # $v the left singular vectors


# From there, it might be worthy to test if R or C++ are faster for the pre and post multiplications of the data and the pre multiplications of the vectors.



