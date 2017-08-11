setwd("C:/Users/Jenny/Documents/projects/ExPosition-Family/Code/R/Development/SlimPosition/")

code.dir<-"../R/"
code.fns<-list.files(code.dir,"R")
sapply(code.fns,source)

load("../data/two.table.wine.rda")

pca.res<-sp.pca(wine$objective,center=T,scale=T,k=4,compact=T,graphs=T)

bada.res<-sp.bada(DATA=wine$objective,center=T,scale=T,DESIGN = wine$supplemental$varietal,
                  make.data.nominal = T,k = 0,compact = T,graphs = T)

load("../data/dica.wine.rda")

dica.res<-sp.dica(DATA=dica.wine$data,make.data.nominal = T,
                  DESIGN=dica.wine$design,
                  k = 0,compact = T,graphs = make.data.nominal)

