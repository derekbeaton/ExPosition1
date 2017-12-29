## correspondence analysis (CA)

#source('./R/gsvd.R')
#source('./R/tolerance.svd.R')
#source('./R/power.rebuild_matrix.R')
#source('./R/invert.rebuild_matrix.R')
#source('./R/isDiagonal.matrix.R')

  #authors data for CA
#load('./data/authors.rda')


## let's just do this directly through the GSVD and do all the preprocessing here.
sum.data <- sum(authors$ca$data)
wi <- rowSums(authors$ca$data)/sum.data
wj <- colSums(authors$ca$data)/sum.data

ca.res <- gsvd( (authors$ca$data/sum.data) - (wi %o% wj), 1/wi, 1/wj)

