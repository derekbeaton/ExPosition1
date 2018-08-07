sp.pls <- function(X, Y, type="regression", center.X = T, scale.X = "SS1", center.Y = T, scale.Y = "SS1", k = 0, compact = T, graphs = F, tol=.Machine$double.eps){

  pls.is.regression <- F
  if( !(type %in% c("regression","correlation","reg","cor","r","c")) ){
    stop("PLS type not recognized.")
  }else{
    if( type %in% c("regression","reg","r")){
      pls.is.regression <- T
    }
  }

  ## if plsr: ship it off.

  ## if plsc: add in the bonus stuff from PLSR
    ## or perhaps just incude that in plsc but with a flag? Then here it can be just shipped off.

  ## or just do it all here... no shipping off.

}
