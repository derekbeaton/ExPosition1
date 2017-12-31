is.identity.matrix <- function(x){

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
