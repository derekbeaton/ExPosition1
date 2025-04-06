#' tepGraphs: TExPosition plotting function
#' 
#' TExPosition plotting function which is an interface to
#' \code{\link{prettyGraphs}}.
#' 
#' tepGraphs is an interface between \code{\link{TExPosition}} and
#' \code{\link{prettyGraphs}}.
#' 
#' @param res results from TExPosition
#' @param x_axis which component should be on the x axis?
#' @param y_axis which component should be on the y axis?
#' @param tepPlotInfo A list (\code{$Plotting.Data}) from \code{tepGraphs} or
#' \code{TExPosition}.
#' @param DESIGN A design matrix to apply colors (by pallete selection) to row
#' items
#' @param fi.col A matrix of colors for the group items. If NULL, colors will
#' be selected.
#' @param fi.pch A matrix of pch values for the group items. If NULL, pch
#' values are all 21.
#' @param fii.col A matrix of colors for the row items (observations). If NULL,
#' colors will be selected.
#' @param fii.pch A matrix of pch values for the row items (observations). If
#' NULL, pch values are all 21.
#' @param fj.col A matrix of colors for the column items. If NULL, colors will
#' be selected.
#' @param fj.pch A matrix of pch values for the column items. If NULL, pch
#' values are all 21.
#' @param col.offset A numeric offset value. Is passed to
#' \code{\link{createColorVectorsByDesign}}.
#' @param constraints Plot constraints as returned from
#' \code{\link{prettyPlot}}. If NULL, constraints are selected.
#' @param lv.constraints Plot constraints for latent variables. If NULL,
#' constraints are selected.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param main main label for the graph window
#' @param lvPlots a boolean. If TRUE, latent variables (X, Y) are plotted. If
#' FALSE, latent variables are not plotted.
#' @param lvAgainst a boolean. If TRUE, latent variables (X, Y) are plotted
#' against each other. If FALSE, latent variables are plotted like factor
#' scores.
#' @param contributionPlots a boolean. If TRUE (default), contribution bar
#' plots will be created.
#' @param correlationPlotter a boolean. If TRUE (default), a correlation circle
#' plot will be created. Applies to PCA family of methods (CA is excluded for
#' now).
#' @param showHulls a value between 0 and 1 to make a peeled hull at that
#' percentage. All values outside of 0-1 will not plot any hulls.
#' @param graphs a boolean. If TRUE, graphs are created. If FALSE, only data
#' associated to plotting (e.g., constraints, colors) are returned.
#' @return The following items are bundled inside of $Plotting.Data:\cr
#' \item{$fii.col}{the colors that are associated to the individuals (row
#' items; $fii).} \item{$fii.pch}{the pch values associated to the individuals
#' (row items; $fii).} \item{$fi.col}{the colors that are associated to the
#' groups ($fi).} \item{$fi.pch}{the pch values associated to the groups
#' ($fi).} \item{$fj.col}{the colors that are associated to the column items
#' ($fj).} \item{$fj.pch}{the pch values associated to the column items ($fj).}
#' \item{$constraints}{axis constraints for the plots (determines end points of
#' the plots).}
#' @author Derek Beaton
#' @seealso \code{\link{prettyGraphs}}
#' @keywords multivariate graphs misc
#' @examples
#' 
#' 	#this is for TExPosition's iris data
#' 	data(ep.iris)
#' 	bada.iris <- tepBADA(ep.iris$data,DESIGN=ep.iris$design,
#' 	  make_design_nominal=FALSE,graphs=FALSE)
#' 	#there are only 2 components, not 3.		
#' 	bada.iris.plotting.data <- tepGraphs(bada.iris,x_axis=1,y_axis=2)
#' 
tepGraphs <-
function(res,x_axis=1,y_axis=2,tepPlotInfo=NULL,DESIGN=NULL,fi.col=NULL, fi.pch=NULL, fii.col=NULL, fii.pch = NULL, fj.col=NULL, fj.pch = NULL,col.offset=NULL,constraints=NULL,lv.constraints=NULL,xlab=NULL,ylab=NULL,main=NULL,lvPlots=TRUE,lvAgainst=TRUE,contributionPlots=TRUE,correlationPlotter=TRUE,showHulls=1,graphs=TRUE){

	pca.types <- c('tepBADA','tepPLS')
	ca.types <- c('tepDICA','tepPLSCA')
	bary.types <- c('tepBADA','tepDICA')
	
	#A simple override/check. If someone puts in texpoOutput class data, tepGraphs will recognize it.
	if(class(res)[1] == "texpoOutput"){
		if(length(res)==2){
			tepPlotInfo <- res$Plotting.Data
		}
		res <- res$TExPosition.Data
	}
	
	#perhaps make this stuff a function, or have TExPosition call all of tepGraphs.
	if(!(class(res)[1] %in% c(pca.types,ca.types))){
		stop("Unknown TExPosition class. Plotting has stopped.")
	}
	if( (!is.null(tepPlotInfo)) && (class(tepPlotInfo)[1] != "tepGraphs") ){
		stop("Unknown tepPlotInfo class. Plotting has stopped.")
	}
	
	#tepPlotInfo check will look for proper colors & constraints, mostly.
	if( !is.null(tepPlotInfo) ){
		#lx or ly are derived from fii; so I can check Lvs here. Also, Lvs take on the properties of fii.*
		if(!(nrow(res$fi)==nrow(tepPlotInfo$fi.col)) || !(nrow(res$fj)==nrow(tepPlotInfo$fj.col)) || !(nrow(res$lx)==nrow(tepPlotInfo$fii.col)) || !(nrow(res$ly)==nrow(tepPlotInfo$fii.col))){
			print('Dimension mismatch. tepPlotInfo will be reset, no hulls can be shown.')
			tepPlotInfo <- list(fii.col=NULL,fii.pch=NULL,fi.col=NULL,fi.pch=NULL,fj.col=NULL,fj.pch=NULL,constraints=NULL,lv.constraints=NULL)
		}		
	}else{
		tepPlotInfo <- list(fii.col=NULL,fii.pch=NULL,fi.col=NULL,fi.pch=NULL,fj.col=NULL,fj.pch=NULL,constraints=NULL,lv.constraints=NULL)
	}	
	
	###Use this block to establish defaults.
	if(is.null(main)){
		main <- deparse(substitute(res))
	}
	if(length(unlist(strsplit(main,"")))>40){
		main <- "Results"
	}				
	if(is.null(xlab)){
		xlab <- paste("Component ",x_axis," variance: ", round(res$t[x_axis],3), "%",sep="")
	}
	if(is.null(ylab)){
		ylab <- paste("Component ",y_axis," variance: ", round(res$t[y_axis],3), "%",sep="")
	}
	if( (!is.null(col.offset)) && is.numeric(col.offset)){
		if(col.offset > 1){
			col.offset <- col.offset / as.numeric(paste(c(1,rep(0,nchar(as.character(col.offset)))),collapse=""))
		}
	}	
	
	if(length(fi.col)==1){
		fi.col <- as.matrix(rep(fi.col,nrow(res$fi)))
	}
	if(length(fii.col)==1){
		fii.col <- as.matrix(rep(fii.col,nrow(res$lx)))
	}	

	if(class(res)[1]%in%bary.types){ ##means it is a barycentric method and we want to force the obs colors to match the groups.
		if( (is.null(fi.col) || is.null(fii.col)) && !is.null(DESIGN)){
			DESIGN <- texpoDesignCheck(DATA=NULL,DESIGN=DESIGN,make_design_nominal=FALSE,force_bary=TRUE)
			obs.cols <- createColorVectorsByDesign(DESIGN,offset=col.offset)
			fi.col <- obs.cols$gc					
			fii.col <- obs.cols$oc
		}else if( (is.null(fi.col) || is.null(fii.col)) && ((!is.null(tepPlotInfo$fi.col)) && (!is.null(tepPlotInfo$fii.col))) ){
			fii.col <- tepPlotInfo$fii.col
			fi.col <- tepPlotInfo$fi.col
		}else{
			#this will catch failures and stop.
			stop("fi.col, fii.col and/or DESIGN are NULL.")		
		}
		###final check.
		if( nrow(fi.col) != nrow(res$fi) || nrow(fii.col) != nrow(res$lx) ){
			print('Incorrect fii.col or fi.col. Attempting to create default colors.')
			if( (is.null(fi.col) || is.null(fii.col)) && !is.null(DESIGN)){
				DESIGN <- texpoDesignCheck(DATA=NULL,DESIGN=DESIGN,make_design_nominal=FALSE,force_bary=TRUE)
				obs.cols <- createColorVectorsByDesign(DESIGN,offset=col.offset)
				fi.col <- obs.cols$gc					
				fii.col <- obs.cols$oc
			}else{
				#this will catch failures and stop.
				stop("fi.col, fii.col and/or DESIGN are NULL.")		
			}	
		}
			
	}else{ ##means it is PLS.
		if( (is.null(fi.col) || is.null(fii.col)) && !is.null(DESIGN)){
			DESIGN <- texpoDesignCheck(DATA=NULL,DESIGN=DESIGN,make_design_nominal=FALSE,force_bary=FALSE)
			fake.design <- cbind(rep(0,nrow(DESIGN)),DESIGN) #matrix(0,nrow(DESIGN)+1,ncol(DESIGN)+1)
			fake.design <- rbind(c(1,rep(0,ncol(DESIGN))),fake.design)
			obs.cols <- createColorVectorsByDesign(fake.design,offset=col.offset)
			fi.col <- as.matrix(rep(obs.cols$oc[1,],nrow(res$fi)))
			fii.col <- as.matrix(obs.cols$oc[-1,])		
		}else if( (is.null(fi.col) || is.null(fii.col)) && ((!is.null(tepPlotInfo$fi.col)) && (!is.null(tepPlotInfo$fii.col))) ){
			fii.col <- tepPlotInfo$fii.col
			fi.col <- tepPlotInfo$fi.col		
		}# else{
			# print('else')
			# these.cols <- prettyGraphsHSVColorSelection(n.colors=2,offset=col.offset)
			# fi.col <- matrix(these.cols[1],nrow(res$fi),1)
			# fii.col <- matrix(these.cols[2],nrow(res$lx),1)		
		# }
		##final check.
		if( nrow(fi.col) != nrow(res$fi) || nrow(fii.col) != nrow(res$lx) ){
			print('Incorrect fii.col or fi.col. Creating default colors.')		
			these.cols <- prettyGraphsColorSelection(n.colors=2,offset=col.offset)
			fi.col <- matrix(these.cols[1],nrow(res$fi),1)
			fii.col <- matrix(these.cols[2],nrow(res$lx),1)			
		}
	}	

	###establish fj.col
	if(length(fj.col)==1){
		fj.col <- as.matrix(rep(fj.col,nrow(res$fj)))
	}else if(is.null(fj.col) && !is.null(tepPlotInfo$fj.col)){
		fj.col <- tepPlotInfo$fj.col 
	}
	###This is a final check.
	if(is.null(fj.col)){
		fj.col <- createColorVectorsByDesign(matrix(1,nrow(res$fj),1),hsv=FALSE)$oc
	}
	if(nrow(fj.col)!=nrow(res$fj)){
		print('Incorrect fj.col. Creating default colors.')
		fj.col <- createColorVectorsByDesign(matrix(1,nrow(res$fj),1),hsv=FALSE)$oc
	}	

	###establish fi.pch	
	if(length(fi.pch)==1){
		fi.pch <- as.matrix(rep(fi.pch,nrow(res$fi)))
	}else if(is.null(fi.pch) && !is.null(tepPlotInfo$fi.pch)){
		fi.pch <- tepPlotInfo$fi.pch 
	}
	###This is a final check.
	if(is.null(fi.pch)){
		fi.pch <- as.matrix(rep(21,nrow(res$fi)))
	}
	if(nrow(fi.pch)!=nrow(res$fi)){
		print('Incorrect fi.pch. Creating default pch.')
		fi.pch <- as.matrix(rep(21,nrow(res$fi)))
	}
	
	###establish fii.pch	
	if(length(fii.pch)==1){
		fii.pch <- as.matrix(rep(fii.pch,nrow(res$fi)))
	}else if(is.null(fii.pch) && !is.null(tepPlotInfo$fii.pch)){
		fii.pch <- tepPlotInfo$fii.pch 
	}
	###This is a final check.
	if(is.null(fii.pch)){
		fii.pch <- as.matrix(rep(21,nrow(res$fi)))
	}
	if(nrow(fii.pch)!=nrow(res$fi)){
		print('Incorrect fii.pch. Creating default pch.')
		fii.pch <- as.matrix(rep(21,nrow(res$fi)))
	}	
	
	###establish fj.pch	
	if(length(fj.pch)==1){
		fj.pch <- as.matrix(rep(fj.pch,nrow(res$fj)))
	}else if(is.null(fj.pch) && !is.null(tepPlotInfo$fj.pch)){
		fj.pch <- tepPlotInfo$fj.pch 
	}
	###This is a final check.
	if(is.null(fj.pch)){
		fj.pch <- as.matrix(rep(21,nrow(res$fj)))
	}
	if(nrow(fj.pch)!=nrow(res$fj)){
		print('Incorrect fj.pch. Creating default pch.')
		fj.pch <- as.matrix(rep(21,nrow(res$fj)))
	}	
	
	if(is.null(constraints) && !is.null(tepPlotInfo$constraints)){
		constraints <- tepPlotInfo$constraints
	}	
	constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=constraints)				
	if(is.null(lv.constraints) && !is.null(tepPlotInfo$lv.constraints)){
		lv.constraints <- tepPlotInfo$lv.constraints
	}
	lv.constraints <- calculateLVConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=lv.constraints)	
	
	#by the time I get here, I should be guaranteed to have a fii.col/pch, fi.col/pch, fj.col/pch, and constraints.		
	####BEGIN BARYCENTRIC METHOD PLOTTING
	if(graphs){
		if(class(res)[1]%in%bary.types){
			#ONLY FOR BARYCENTRIC
			fii.plot.info <- prettyPlot(res$fii,x_axis=x_axis,y_axis=y_axis,col=fii.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,pch=fii.pch,contributionCircles=FALSE,dev.new=TRUE)
			#ONLY FOR BARYCENTRIC
			if(showHulls > 0 && showHulls <= 1){
				colorDesign <- makeNominalData(fii.col)
				for(i in 1:nrow(res$fi)){
					peeledHull(res$fii[which(fii.col[, 1] == fi.col[i,1]), ], x_axis = x_axis, y_axis = y_axis, percentage = showHulls, col = "black", lwd = 3)
					peeledHull(res$fii[which(fii.col[, 1] == fi.col[i,1]), ], x_axis = x_axis, y_axis = y_axis, percentage = showHulls, col = fi.col[i, ], lwd = 1)
				}
			}			
			##FOR ALL PLS
			fi.plot.info <- prettyPlot(res$fi,x_axis=x_axis,y_axis=y_axis,col=fi.col,axes=FALSE,contributionCircles=TRUE,contributions=res$ci,pch=fi.pch,dev.new=FALSE,new.plot=FALSE)
		}else{
			fi.plot.info <- prettyPlot(res$fi,x_axis=x_axis,y_axis=y_axis,col=fi.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,contributionCircles=TRUE,contributions=res$ci,pch=fi.pch,dev.new=TRUE)
		}
		##FOR ALL PLS
		fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,pch=fj.pch,contributionCircles=TRUE,contributions=res$cj,dev.new=TRUE)	
		
		##For all PLS, but looks weird for barycentric methods. 
		###PLOT LVs
		if(lvPlots){
			if(lvAgainst){
				lxxlab <- paste("LX ",x_axis,sep="")
				lyxlab <- paste("LY ",x_axis,sep="")
				lxylab <- paste("LX ",y_axis,sep="")
				lyylab <- paste("LY ",y_axis,sep="")				
				prettyPlot(cbind(res$lx[,x_axis],res$ly[,x_axis]),x_axis=1,y_axis=2,col=fii.col,pch=fii.pch,xlab=lxxlab,ylab=lyxlab,main=paste(lxxlab," vs. ",lyxlab,sep=""),constraints=lv.constraints)
				prettyPlot(cbind(res$lx[,y_axis],res$ly[,y_axis]),x_axis=1,y_axis=2,col=fii.col,pch=fii.pch,xlab=lxylab,ylab=lxylab,main=paste(lxylab," vs. ",lyylab,sep=""),constraints=lv.constraints)
			}else{
				prettyPlot(res$lx,x_axis=x_axis,y_axis=y_axis,col=fii.col,pch=fii.pch,xlab=xlab,ylab=ylab,main=main,constraints=lv.constraints)
				prettyPlot(res$ly,x_axis=x_axis,y_axis=y_axis,col=fii.col,pch=fii.pch,xlab=xlab,ylab=ylab,main=main,constraints=lv.constraints)					
			}
		}
		
		if(contributionPlots){
			contributionBars(res$fi,res$ci,x_axis=x_axis,y_axis=y_axis,main=main,col=fi.plot.info$col)
			contributionBars(res$fj,res$cj,x_axis=x_axis,y_axis=y_axis,main=main,col=fj.plot.info$col)			
		}
		
		##ONLY FOR BADA		
		if(correlationPlotter && class(res)[1]=='tepBADA'){
			correlationPlotter(res$X,res$fi,col=fj.col,pch=fj.pch,x_axis=1,y_axis=2,xlab=xlab,ylab=ylab,main=main) 
		}									
	}
	####END BARYCENTRIC METHOD PLOTTING
	
	tepPlotInfo <- list(fii.col=fii.col, fii.pch=fii.pch,fi.col=fi.col, fi.pch=fi.pch,fj.col=fj.col,fj.pch=fj.pch,constraints=constraints,lv.constraints=lv.constraints)
	class(tepPlotInfo) <- c("tepGraphs", "list")
	#return(tepPlotInfo)
	invisible(tepPlotInfo)	
}
