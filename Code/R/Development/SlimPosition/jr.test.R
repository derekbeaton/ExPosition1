#setwd("C:/Users/Jenny/Documents/projects/ExPosition-Family/Code/R/Development/SlimPosition/")

code.dir<-"./R/"
code.fns<-list.files(path=code.dir,pattern="*.R$")
sapply(paste0(code.dir,code.fns),source)

load("./data/two.table.wine.rda")

pca.res<-sp.pca(wine$objective,center=T,scale=T,k=4,compact=T,graphs=T)

bada.res<-sp.bada(DATA=wine$objective,center=T,scale=T,DESIGN = wine$supplemental$varietal,
                  make.data.nominal = T,k = 0,compact = T,graphs = T)

load("./data/dica.wine.rda")

library(ExPosition)

dica.res<-sp.dica(DATA=dica.wine$data,make.data.nominal = F,
                  DESIGN=rep(c("L","R","B"),each=4),
                  k = 0,compact = T,graphs = T)

