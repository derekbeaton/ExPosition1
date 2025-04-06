#' fii2fi: individuals to centers
#' 
#' All computations between individual factor scores (fii) and group factor
#' scores (fi).
#' 
#' 
#' @param DESIGN a dummy-coded design matrix
#' @param fii a set of factor scores for individuals (rows)
#' @param fi a set of factor scores for rows
#' @return A list of values containing: \item{distances}{Euclidean distances of
#' all rows to each category center} \item{assignments}{an assignment matrix
#' (similar to DESIGN) where each individual is assigned to the closest
#' category center} \item{confusion}{a confusion matrix of how many items are
#' assigned (and mis-assigned) to each category}
#' @author Hervé Abdi, Derek Beaton
#' @keywords misc multivariate
fii2fi <-
function(DESIGN,fii,fi){
	Dsup <- fastEucCalc(fii,fi)
	minD <- apply(Dsup,1,min)
	Group_Assigned <- Re(Dsup==matrix(minD,nrow(DESIGN),ncol(DESIGN)))
	return(list(distances=Dsup,assignments=Group_Assigned,confusion=t(Group_Assigned) %*% DESIGN))
}
