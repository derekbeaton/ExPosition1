#' designCheck
#' 
#' Checks and/or creates a dummy-coded design matrix.
#' 
#' Returns a properly formatted, dummy-coded (or disjunctive coding) design
#' matrix.
#' 
#' @usage designCheck(DATA, DESIGN = NULL, make_design_nominal = TRUE)
#' @param DATA original data that should be matched to a design matrix
#' @param DESIGN a column vector with levels for observations or a dummy-coded
#' matrix
#' @param make_design_nominal a boolean. Will make DESIGN nominal if TRUE
#' (default).
#' @return \item{DESIGN}{dummy-coded design matrix}
#' @author Derek Beaton
#' @keywords misc
#' @examples
#' 
#' 	data <- iris[,c(1:4)]
#' 	design <- as.matrix(iris[,c('Species')])
#' 	iris.design <- designCheck(data,DESIGN=design,make_design_nominal=TRUE)
#' 
#' @export designCheck
designCheck <-
function(DATA,DESIGN=NULL,make_design_nominal=TRUE){

	#DESIGN
	if(is.null(DESIGN)){
		#print('DESIGN is null, creating default DESIGN matrix.')
		return(createDefaultDesign(DATA))
	}else{
		#check 1: is this a matrix or not? Make it one if not.
		if(!is.matrix(DESIGN)){
			DESIGN <- as.matrix(DESIGN)
		}
		#check 2: do I need to nominalize the matrix? If true, do so.
		if(make_design_nominal){
			if(ncol(DESIGN)==1 && length(unique(DESIGN))>1 ){
				#print('Making a dummy-coded design matrix.')
				DESIGN <- makeNominalData(DESIGN)
			}else{
				print('DESIGN has too many columns or not enough elements. If the current DESIGN fails, a default will be created.')	
			}
			#when I do this, none of the below should apply. However, I can't predict all cases of crazy matrices, so I sanity check myself.
		}
		#else{ #you think you have a correct matrix...
		#check 3: do you have the same number of rows?
		if(!(dim(DATA)[1]==dim(DESIGN)[1])){
			print("Row dimensions do not match for X and Y. Creating default.")
			return(createDefaultDesign(DATA))
		}
		if(!is.character(DESIGN)){
			#check 4.a Are you assigning each observation to only 1 category?
			if(sum(DESIGN)!=nrow(DATA)){
				print("Group Assignment Matrix is incorrect: too many items in the DESIGN matrix! Creating default.")
				return(createDefaultDesign(DATA))
			}
			#}
			#check 4.b Are you assigning each observation to only 1 category?		
			#this could theoretically happen under the above conditions if someone doesn't know what they are doing.
			if(ncol(DESIGN) > nrow(DESIGN)){
				print("Group Assignment Matrix is incorrect: too many groups to assign to (cannot be more than number of rows)! Creating default.")
				return(createDefaultDesign(DATA))
			}
			#check 4.c Are you assigning each observation to only 1 category?				
			if(!sum(rowSums(DESIGN)==1) == nrow(DATA)){
				print("Group Assignment Matrix is incorrect: assignments are missing/duplicated! Creating default.")
				return(createDefaultDesign(DATA))
			}		
		}else{
			print('DESIGN is not dummy-coded matrix. Creating default.')
			return(createDefaultDesign(DATA))
		}
		#just a final cleanliness check.
		if(sum(rownames(DATA) %in% rownames(DESIGN))!=dim(DATA)[1]){
			#print("Rownames do not match. Setting rownames in Y to rownames of X")
			rownames(DESIGN) <- rownames(DATA)
		}			
	}
	return(DESIGN)
}
