#' calculateLVConstraints
#' 
#' Calculates constraints for plotting latent variables.
#' 
#' 
#' @param results results (with $lx and $ly) from TExPosition (i.e.,
#' $TExPosition.Data)
#' @param x_axis which component should be on the x axis?
#' @param y_axis which component should be on the y axis?
#' @param constraints if available, axis constraints for the plots (determines
#' end points of the plots).
#' @return Returns a list with the following items:\cr \item{$constraints}{axis
#' constraints for the plots (determines end points of the plots).}
#' @author Derek Beaton
#' @keywords misc
calculateLVConstraints <-
function(results,x_axis=1,y_axis=2,constraints=NULL){
	axis1<-x_axis
	axis2<-y_axis	
	
	if(!is.null(constraints)){
		if(("minx" %in% names(constraints)) && ("maxx" %in% names(constraints)) && ("miny" %in% names(constraints)) && ("maxy" %in% names(constraints))){
			constraintsCheck <- computeLVConstraints(results,x_axis,y_axis)
			if(constraintsCheck$minx < constraints$minx){
				constraints$minx <- constraintsCheck$minx
				#print("minx constraint changed.")
			}
			if(constraintsCheck$miny < constraints$miny){
				constraints$miny <- constraintsCheck$miny
				#print("miny constraint changed.")				
			}			
			if(constraintsCheck$maxx > constraints$maxx){
				constraints$maxx <- constraintsCheck$maxx
				#print("maxx constraint changed.")
			}			
			if(constraintsCheck$maxy > constraints$maxy){
				constraints$maxy <- constraintsCheck$maxy
				#print("maxy constraint changed.")				
			}			
			return(constraints)
		}else{
			#constraints <- computeConstraints(results,x_axis,y_axis)
			return(computeLVConstraints(results,x_axis,y_axis))
		}
	}#else{
		#constraints <- computeConstraints(results,x_axis,y_axis)	
	#}
	#return(constraints)
	return(computeLVConstraints(results,x_axis,y_axis))
}
