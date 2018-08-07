#' Compute normalization for rows or columns of a matrix.
#' @title \code{data.norms}
#' @param X a matrix for input
#' @param type the type of scaling to perform. Options: "ca" which is item divided by row sums, "hellinger" which is sqrt of "ca", "z" which is the same as scale(x), and "ss1" which is sum of squares 1. Also available is "scale" and requires use of center and scale parameters
#' @param center the intended center (see \code{\link{scale}})
#' @param scale the intended scale (see \code{\link{scale}})
#' @param margin which margin to perform this on (i.e., 1 for rows and 2 for columns)
#' @return column or row normalized version of the matrix.
#' @export
data.norms <- function(X,type=NULL,center=F,scale=F,margin=2){

  orig.dims <- dim(X)
  orig.names <- dimnames(X)

  if(is.null(type) & (center==F | is.na(center) | is.null(center)) & (scale==F | is.na(scale) | is.null(scale))){
    return(X)
  }else if(type=="ca"){
    X <- apply(X,margin,function(x){ x/sum(x) })
  }else if(type=="hellinger"){
    X <- apply(X,margin,function(x){ sqrt(x/sum(x)) })
  }else if(type == "z"){
    X <- apply(X,margin,function(x){scale(x,center=T,scale=T)})
  }else if(type=="ss1"){
    X <- apply(X,margin,function(x){scale(x,center=T,scale=sd(x))}) / sqrt(orig.dims[-margin]-1)
  }else if(type=="scale"){
    X <- apply(X,margin,function(x){scale(x,center=center,scale=scale)})
  }else{
    return(X)
  }

  new.dims <- dim(X)
  if(new.dims[1]==orig.dims[2] & new.dims[2]==orig.dims[1]){
    X <- t(X)
  }
  dimnames(X) <- orig.names
  return(X)
}
