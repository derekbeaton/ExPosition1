#setwd("C:/Users/Jenny/Documents/projects/ExPosition-Family/Code/R/Development/SlimPosition/")

code.dir<-"./R/"
code.fns<-list.files(path=code.dir,pattern="*.R$")
sapply(paste0(code.dir,code.fns),source)

load("./data/two.table.wine.rda")

pca.res<-sp.pca(wine$objective,center=T,scale=T,k=4,compact=T,graphs=T)
diag(t(pca.res$fi) %*% pca.res$fi) / (pca.res$d[1:ncol(pca.res$fi)]^2)
  ##also a test via the "latent variable" model.

bada.res<-sp.bada(DATA=wine$objective,center=T,scale=T,DESIGN = wine$supplemental$color,
                  make.data.nominal = T,k = 0,compact = F,graphs = T)
lv.max <- t(bada.res$lx) %*% bada.res$ly
if(isDiagonal.matrix(lv.max)){
  diag(lv.max) / bada.res$d
}


load("./data/dica.wine.rda")

library(ExPosition)

dica.res<-sp.dica(DATA=dica.wine$data,make.data.nominal = F,
                  DESIGN=rep(c("L","R","B"),each=4),
                  k = 0,compact = T,graphs = T)

