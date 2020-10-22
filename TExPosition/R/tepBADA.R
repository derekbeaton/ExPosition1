#tepBADA <- function(DATA,scale=TRUE,center=TRUE,DESIGN=NULL,make_design_nominal=TRUE,group.masses=NULL,ind.masses=NULL,weights=NULL,graphs=TRUE,k=0){
#tepBADA <- function(DATA,scale=TRUE,center=TRUE,DESIGN=NULL,make_design_nominal=TRUE,group.masses=NULL,weights=NULL,graphs=TRUE,k=0){	
tepBADA <- function(DATA,scale=TRUE,center=TRUE,DESIGN=NULL,make_design_nominal=TRUE,graphs=TRUE,k=0){	
	
  OGDATA <- DATA	
	
	DESIGN <- texpoDesignCheck(DATA,DESIGN,make_design_nominal,force_bary=TRUE)
	colDESIGN <- colnames(DESIGN)
	massedDESIGN <- t(apply(DESIGN,1,'/',colSums(DESIGN)))
	colnames(massedDESIGN) <- colDESIGN	
	
	main <- deparse(substitute(DATA))		
	
	#Don't want to carry over previous centering and scaling from outside tepBADA
	attributes(DATA)$`scaled:center` <- rep(0, dim(DATA)[2])
	attributes(DATA)$`scaled:scale` <- rep(1, dim(DATA)[2])
	
	DATA <- expo.scale(as.matrix(DATA),scale=scale,center=center)
  R <- t(massedDESIGN) %*% DATA
	
	this.center <- attributes(DATA)$`scaled:center`
	this.scale <- attributes(DATA)$`scaled:scale`	

	colnames(R) <- colnames(DATA)
	rownames(R) <- colnames(DESIGN)	
	
	#res <- corePCA(R,k=k)
	
	#res needs to be a class that supplementaryRows recognizes
	res <- epPCA(R, scale = FALSE, center = FALSE, graphs = FALSE, k = k)
	res <- res$ExPosition.Data
	res$center <- this.center
	res$scale <- this.scale
  
	#DATA is now scaled, so supplementary takes the original data
	supplementaryRes <- supplementaryRows(OGDATA,res)
	res$fii <- supplementaryRes$fii
	res$dii <- supplementaryRes$dii
	res$rii <- supplementaryRes$rii
	
	res$lx <- res$fii
	res$ly <- supplementaryCols(t(massedDESIGN),res,center=FALSE,scale=FALSE)$fjj

	assignments <- fii2fi(DESIGN,res$fii,res$fi)
	assignments$r2 <- R2(NULL,res$di,ind.masses=NULL,res$dii)
	class(assignments) <- c("tepAssign","list")
	res$assign <- assignments

	#new res here
	class(res) <- c("tepBADA","list")		
	tepPlotInfo <- tepGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs,lvPlots=FALSE)
	return(tepOutputHandler(res=res,tepPlotInfo=tepPlotInfo))
}
