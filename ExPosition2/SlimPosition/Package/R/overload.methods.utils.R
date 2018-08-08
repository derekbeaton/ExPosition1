## expo class overload methods

#' @export
plot.expo <- function(expo.output, type="components", ...){

  if( !(type %in% c("row.scores","col.scores","row.loadings","col.loadings","scree","lvs","biplot")) ){
    stop("Unknown plot type")
  }

  ## need to check type here, e.g., PCA, CA, MDS vs. PLS, CCA, RDA
  if(expo.output$analysis %in% c("pca","ca")){

    if(type=="row.scores"){
      ep.component.plot(expo.output$fi, ...)
    }
    if(type =="col.scores"){
      ep.component.plot(expo.output$fj, ...)
    }
    if(type=="row.loadings"){
      ep.component.plot(expo.output$u, ...)
    }
    if(type =="col.loadings"){
      ep.component.plot(expo.output$v, ...)
    }
    if(type=="scree"){
      ep.scree(expo.output$tau)
    }

  }

}

# summary.expo <- function(expo.output, ...){
#
#   ## need to check type here, e.g., PCA, CA, MDS vs. PLS, CCA, RDA
#     ## or an alternative is to just base this on the GSVD but know the type.
#
# }
#
#
# print.expo <- function(expo.output,...){
#
# }
