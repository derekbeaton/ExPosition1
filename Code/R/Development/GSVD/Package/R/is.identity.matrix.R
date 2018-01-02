is.identity.matrix <- function(x,tol=.Machine$double.eps){

  x <- as.matrix(x)
  x[abs(x) < tol] <- 0

  if(is.diagonal.matrix(x)){
    if( all(diag(x)==1) ){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }

}
