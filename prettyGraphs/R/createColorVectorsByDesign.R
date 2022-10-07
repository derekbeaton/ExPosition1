#' createColorVectorsByDesign
#' 
#' Produces a color vector for items by using a design matrix.
#' 
#' 
#' @param design_matrix A dummy coded design matrix.
#' @param hsv a boolean. If TRUE, use
#' \code{\link{prettyGraphsHSVColorSelection}}. If FALSE, use
#' \code{\link{prettyGraphsColorSelection}}.
#' @param offset numeric. An offset value to be passed to
#' \code{\link{prettyGraphsHSVColorSelection}} or
#' \code{\link{prettyGraphsColorSelection}}.
#' @return Produces a list with the following items\cr \item{oc}{The colors of
#' the observations (based on group)} \item{gc}{The colors of the groups}
#' @author Derek Beaton
#' @keywords misc
#' @export createColorVectorsByDesign
createColorVectorsByDesign <-
function(design_matrix,hsv=TRUE,offset=NULL){

	if(hsv){
		group_colors <- prettyGraphsHSVColorSelection(n.colors=ncol(design_matrix),offset=offset)
	}else{
		group_colors <- prettyGraphsColorSelection(n.colors=ncol(design_matrix),offset=offset)	
	}
	rownames(group_colors)<-colnames(design_matrix)

	arr.ind <- which(design_matrix==1,arr.ind=TRUE)
	arr.ind <- arr.ind[order(arr.ind[,1]),]
	observation_colors <- as.matrix(group_colors[arr.ind[,2],1])
	rownames(observation_colors) <- rownames(design_matrix)
	return(list(oc=observation_colors,gc=group_colors))
}
