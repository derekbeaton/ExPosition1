library(TInPosition)
library(MExPosition)
###CSDA Tests

#for ade4 
library(ade4)
data(deug)
deug.dudi <- dudi.pca(deug$tab, center = deug$cent,scale = FALSE, scan = FALSE)
inertia <- inertia.dudi(deug.dudi,row.inertia = T)$row.abs
prettyPlot(deug.dudi$li,contributionCircles=TRUE,contributions=inertia)
detach(package:ade4)

# for FactoMineR 
library(FactoMineR)
data(decathlon) 
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13,graph=FALSE) 
prettyPlot(res.pca$ind$coord, contributionCircles=TRUE, contributions=res.pca$ind$contrib)
detach(package:FactoMineR)
detach(package:ellipse)
detach(package:lattice)
detach(package:cluster)
detach(package:scatterplot3d)



#beer.taste.res.style <- epPCA(DATA = BEER, scale = FALSE, DESIGN = STYLES, make_design_nominal = FALSE)

data(beer.tasting.notes)
these.rows <- which(rowSums(beer.tasting.notes$region.design[,-5])==1) 
BEER <- beer.tasting.notes$data[these.rows,] 
STYLES<-beer.tasting.notes$style.design[these.rows,] 
beer.taste.res.style <- epPCA.inference.battery(DATA = BEER, scale = FALSE, DESIGN = STYLES, make_design_nominal = FALSE, test.iters = 1000)

##
these.rows <- which(rowSums(beer.tasting.notes$region.design[,-5])==1) 
BEER <- beer.tasting.notes$data[these.rows,] 
DESIGN <- beer.tasting.notes$pale.sour.style[these.rows,] 
beer.bada <- tepBADA.inference.battery(DATA = BEER, scale = FALSE, DESIGN = DESIGN, make_design_nominal = FALSE, test.iters = 1000)


BEER.recode <- apply(BEER,2,cut,breaks=4,labels=c("LOW","MidLOW","MidHIGH","HIGH")) 
rownames(BEER.recode) <- rownames(BEER)
mca.res <- epMCA(DATA = BEER.recode, make_data_nominal = TRUE,DESIGN = STYLES, make_design_nominal = FALSE, correction = NULL)
hellinger.res <- epMCA(DATA = BEER.recode, make_data_nominal = TRUE, DESIGN = STYLES, make_design_nominal = FALSE, hellinger = TRUE, symmetric = FALSE, correction = NULL)

##
BEER.DIST <- as.matrix(dist(BEER,upper=TRUE,diag=TRUE))
flav<-epMDS(DATA=BEER.DIST^2, DESIGN=STYLES, make_design_nominal =FALSE,graphs=FALSE) 
phys.dist <- beer.tasting.notes$physical.distances 
phys<-epMDS(DATA=phys.dist^2, DESIGN= STYLES,make_design_nominal =FALSE,graphs=FALSE)
epGraphs(flav,contributionPlots=FALSE,correlationPlot=FALSE,main="Taste Profile Distance")
epGraphs(phys,contributionPlots=FALSE,correlationPlot=FALSE,main="Physical Distance")


##
table <- c(rep("flavors",ncol(BEER.DIST)),rep("meters",ncol(phys.dist))) 
flavor.phys.dist <- cbind(BEER.DIST,phys.dist)
demo.distatis <- mpDISTATIS(flavor.phys.dist, DESIGN=STYLES, sorting='No', normalization='MFA',table=table, make.design.nominal=FALSE)