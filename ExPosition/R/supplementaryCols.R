#this function works as a shortcut for users. It's a "recognition engine" to auto perform 1) correct preprocessing and 2) supplemental projection.

#RE: PCA -- with cols, the normalization gets tricky. With variables, center/scale should occur as it did with active.


#' Supplementary columns
#' 
#' Computes factor scores for supplementary measures (columns).
#' 
#' This function recognizes the class types of: \code{\link{epPCA}},
#' \code{\link{epMDS}}, \code{\link{epCA}}, \code{\link{epMCA}}, and
#' \code{TExPosition} methods. Further, the function recognizes if Hellinger
#' (as opposed to row profiles; in CA, MCA and DICA) were used.
#' 
#' @usage supplementaryCols(SUP.DATA, res, center = TRUE, scale = TRUE)
#' @param SUP.DATA a data matrix of supplementary measures (must have the same
#' observations [rows] as active data)
#' @param res ExPosition or TExPosition results
#' @param center a boolean, string, or numeric. See \code{\link{expo.scale}}
#' @param scale a boolean, string, or numeric. See \code{\link{expo.scale}}
#' @return A list of values containing:\cr \item{fjj}{factor scores computed
#' for supplemental columns} \item{djj}{squared distances for supplemental
#' columns} \item{rjj}{cosines for supplemental columns}
#' @author Derek Beaton
#' @keywords misc multivariate
#' @export supplementaryCols
supplementaryCols <- function(SUP.DATA,res,center=TRUE,scale=TRUE){
	SUP.DATA <- as.matrix(SUP.DATA)
	
	output.types <- c("expoOutput","texpoOutput")
	data.types <- c("ExPosition.Data","TExPosition.Data")
	mds.types <- c('epMDS')#can add DiSTATIS to this.
	pca.types <- c('epPCA','tepBADA')
	ca.types <- c('epCA','epMCA','tepDICA')	
		
	if(class(res)[1] %in% output.types){
		indicator <- which(output.types %in% class(res)[1])
		if(any(names(res) %in% data.types) && length(names(res))==2){
			if(output.types[indicator]=="expoOutput"){
				res <- res$ExPosition.Data
			}
			if(output.types[indicator]=="texpoOutput"){
				res <- res$TExPosition.Data
			}
		}else{
			stop(paste("res class type is unknown:",names(res),sep=" "))
		}
	}
	
	if((class(res)[1] %in% c(pca.types))){
		#some trickery happens here... if no res$M is available, it is passed as NULL.
		sup.transform <- pcaSupplementaryColsPreProcessing(SUP.DATA,center=center,scale=scale,M=res$M)
		sup.proj <- supplementalProjection(sup.transform,res$fi,res$pdq$Dv)
	}
	
	 else if((class(res)[1] %in% c(ca.types))){
	 	if(res$hellinger){
	 		sup.transform <- hellingerSupplementaryColsPreProcessing(SUP.DATA)
	 		if(res$symmetric){
	 			sup.proj <- supplementalProjection(sup.transform,f.scores=res$fi,Dv=res$pdq$Dv)
	 		}else{
	 			sup.proj <- supplementalProjection(sup.transform,f.scores=res$pdq$p,Dv=res$pdq$Dv)
	 		}	
	 	}else{
			sup.transform <- caSupplementalElementsPreProcessing(t(SUP.DATA))
			if(res$symmetric){
				this.Dv <- res$pdq$Dv
			}else{
				this.Dv <- res$eigs
			}		
			if((class(res)[1] %in% c('epMCA'))){ ##stupid corrections.
				sup.proj <- supplementalProjection(sup.transform,res$fi,this.Dv,scale.factor=res$pdq$Dv/res$pdq.uncor$Dv[1:length(res$pdq$Dv)])
			}else{
				sup.proj <- supplementalProjection(sup.transform,res$fi,this.Dv)
			}
		}
	}
	
	 else if((class(res)[1] %in% c(mds.types))){ #this is the same as rows. 
		sup.transform <- mdsSupplementalElementsPreProcessing(SUP.DATA,res$D,res$M)
		sup.proj <- supplementalProjection(sup.transform,res$fi,res$pdq$Dv)
	}else{
		stop("Unknown class type. Supplementary projection computation must stop.")	
	}
	
	fjj <- sup.proj$f.out
	djj <- sup.proj$d.out	
	rjj <- sup.proj$r.out
	rownames(fjj) <- rownames(djj) <- rownames(rjj) <- colnames(SUP.DATA)
	return(list(fjj=fjj,djj=djj,rjj=rjj))
}
