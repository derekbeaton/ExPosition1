

rm(list=ls())
DATA <- authors$ca$data

system.time({
  prof.row <- rowSums(DATA)/sum(DATA)
  prof.col <- colSums(DATA)/sum(DATA)
  deviations <- sweep(sweep(DATA,1,prof.row*sum(DATA),"/"),2,prof.col)
  prof.res <- gsvd(deviations,prof.row, 1/prof.col)
  #prof.res$fi <- prof.res$fi / matrix(prof.row,nrow(deviations),ncol(prof.res$fi),byrow=F)
  prof.res$fi <- sweep(prof.res$fi,1,prof.row,"/")
})



system.time({
  sum.data <- sum(DATA)
  wi <- rowSums(DATA)/sum.data
  wj <- colSums(DATA)/sum.data
  res <- gsvd( (DATA/sum.data) - (wi %o% wj), 1/wi, 1/wj)
})


