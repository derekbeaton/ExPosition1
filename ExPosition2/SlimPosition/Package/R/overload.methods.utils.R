## expo class overload methods

#' @export
plot.expo <- function(expo.output, type="row.scores", ...){


  if( !(type %in% c("row.scores","col.scores","row.loadings","col.loadings","scree","lvs","biplot")) ){
    stop("Unknown plot type")
  }

  ## need to check type here, e.g., PCA, CA, MDS vs. PLS, CCA, RDA
      ## actually I don't really need to check... almost everything should just work.
      ## the plotting utility here is meant to be as absolutely simple as possible.
  # if(expo.output$analysis %in% c("pca","ca")){

    ## these work for effectively everything except MDS

    if(type=="row.scores"){
      if("fi" %in% names(expo.output)){
        ep.component.plot(expo.output$fi, ...)
      }else{
        stop("fi not found")
      }
    }
    if(type == "col.scores"){
      if("fj" %in% names(expo.output)){
        ep.component.plot(expo.output$fj, ...)
      }else{
        stop("fj not found")
      }
    }

    if(type=="row.loadings"){
      if("u" %in% names(expo.output)){
        ep.component.plot(expo.output$u, ...)
      }else{
        stop("u not found")
      }
    }
    if(type =="col.loadings"){
      if("v" %in% names(expo.output)){
        ep.component.plot(expo.output$v, ...)
      }else{
        stop("v not found")
      }
    }

    ### SHOULD NOT EXIST FOR PLSR -- I will NOT return tau for that.
    if(type=="scree"){
      if("tau" %in% names(expo.output)){
        ep.scree(expo.output$tau)
      }else{
        stop("tau not found")
      }
    }

    ### THESE ARE PLS SPECIFIC WITH EMPHASIS ON PLS*R
    if(type=="r2"){
      if(( "r2.x" %in% names(expo.output) & "r2.y" %in% names(expo.output) )){

      }else{
        stop("r2.x or r2.y not found")
      }
    }
    if(type=="lv" ){
      if(( "lx" %in% names(expo.output) & "ly" %in% names(expo.output) ) ){

      }else{
        stop("lx or ly not found")
      }
    }

  # }

}

  ## all I can guarnatee here is: (1) the type, (2) fi/fj, u/v, and d/t
summary.expo <- function(expo.output, ...){

  ## need to check type here, e.g., PCA, CA, MDS vs. PLS, CCA, RDA
    ## or an alternative is to just base this on the GSVD but know the type.

  if(expo.output$analysis %in% c("pca","ca","mca")){ ## add to the list.

    # make summary for the results like what we would include in a paper


    # make a print statement like the summary.princomp
      ## a PCA/CA/MCA whatever here
    analysis.type <- "A [placeholde] analysis "
    cat( paste0(analysis.type, "was performed on a I = ", nrow(expo.output$fi), "rows by J = ", nrow(expo.output$fj), " matrix. The total number of components (i.e., rank of the matrix) was: ", length(expo.output$d.orig)," with ", length(expo.output$d), " components computed (retained). \nComponents information:\n") )
    cat("Information for retained components:\n")
    print(
      rbind(`Singular values (standard deviation)` = expo.output$d,
            `Percent of Variance` = expo.output$tau[1:length(expo.output$d)],
            `Cumulative Percent` = cumsum(expo.output$tau[1:length(expo.output$d)]))
      )



  }#else if(){ ## the two table techniques.

  #}
  else{
    warning("$analysis type not found.")
  }

  invisible(x)

}


  ## this will be like before where it describes the output.
# print.expo <- function(expo.output,...){
#
# }
