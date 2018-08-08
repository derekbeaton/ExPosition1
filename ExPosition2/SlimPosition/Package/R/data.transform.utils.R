# ExPosition data transform utilities.

#' @export
svd.norm <- function(x){

  return(x/tolerance.svd(x)$d[1])
  ## alternative (which is likely safer):
  #res <- tolerance.svd(x,...)
  #return( (res$u * matrix(res$d/res$d[1],nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )

}



### I need better names for these .norm functions.
  ## norm is far too specific when it comes to statistics.
#' @export
row.scale <- function(X,type=NULL,center=T,scale=F,keep.original.attributes = F){
  return(margin.scale(X,type=type,center=center,scale=scale,margin=1,keep.original.attributes=keep.original.attributes))
}
#' @export
col.scale <- function(X,type=NULL,center=T,scale=F,keep.original.attributes = F){
  return(margin.scale(X,type=type,center=center,scale=scale,margin=2,keep.original.attributes=keep.original.attributes))
}

#' Compute standardization/normalization for rows or columns of a matrix.
#' @title \code{margin.scale}
#' @param X a matrix for input
#' @param type the type of scaling to perform. Options: "rp" (row profile) which is item divided by row sums, "hellinger" which is sqrt of "rp", "z" which is the same as scale(x), and "ss1" which is sum of squares 1. Also available is "scale" and requires use of center and scale parameters
#' @param center the intended center (see \code{\link{scale}})
#' @param scale the intended scale (see \code{\link{scale}})
#' @param margin which margin to perform this on (i.e., 1 for rows and 2 for columns)
#' @return column or row normalized version of the matrix.
#' @export
margin.scale <- function(X,type=NULL,center=F,scale=F,margin=2, keep.original.attributes = F){

  if( !(margin %in% c(1,2)) ){
    stop("`margin` must be either 1 or 2.")
  }

  if(length(dim(X)) != 2){
    stop("X is required to be a data.frame or matrix of only two dimensions (rows and columns).")
  }

  orig.names <- dimnames(X)
  if(keep.original.attributes){ #storing stuff just incase someone needs them.
    orig.attributes <- attributes(X)
  }

  if(margin==1){
    X <- t(X)
  }

  type <- tolower(type)

  if( !(type %in% c("z","ss1","center","rms","scale")) | is.null(type) | type=="none" ){
    attributes(X)$norm.type <- "none"
    attributes(X)$`scaled:center` <- rep(0,ncol(X))
    attributes(X)$`scaled:scale` <- rep(1,ncol(X))
    # do nothing.

  }else if(type == "z"){ ## this can be replaced by a scale call... except the t()

    X <- scale(X,center=T,scale=T)
    attributes(X)$norm.type <- "z"
    ## center & scale are already covered.

  }else if(type=="ss1"){ ## this can be replaced by a scale call... except the t()


    X <- scale(X,center=T,scale=apply(X,2,function(x){ sd(x) * sqrt(length(x)-1) }))
    attributes(X)$norm.type <- "ss1"
    ## center & scale are already covered.

  }else if(type=="rms"){

    X <- scale(X,center=F,scale=T)
    attributes(X)$`scaled:center` <- rep(0,ncol(X))
    attributes(X)$norm.type <- "rms"

  }else if(type=="center"){

    X <- scale(X,center=T,scale=F)
    attributes(X)$`scaled:scale` <- rep(1,ncol(X))
    attributes(X)$norm.type <- "center"

  }else if(type=="scale"){

    # you do you
    X <- scale(X,center=center,scale=scale)
    attributes(X)$norm.type <- "scale"
    if( !any(names(attributes(X)) %in% c("scaled:center")) ){
      attributes(X)$`scaled:center` <- rep(0,ncol(X))
    }
    if( !any(names(attributes(X)) %in% c("scaled:scale")) ){
      attributes(X)$`scaled:scale` <- rep(1,ncol(X))
    }


  }else{
    warning("No margin.scale parameter conditions were met. Returning data as they are.")
    attributes(X)$norm.type <- "none"
    attributes(X)$`scaled:center` <- rep(0,ncol(X))
    attributes(X)$`scaled:scale` <- rep(1,ncol(X))
  }


  attributes(X)$`scaled:margin` <- margin
  if(margin==1){
    X <- t(X)
  }

  # just to be safe.
  dimnames(X) <- orig.names

  if(keep.original.attributes){ #storing stuff just incase someone needs them.
    attributes(X)$original.attributes <- orig.attributes
  }

  attributes(X)$func <- "margin.scale"

  return(X)
}


#' @export
table.scale <- function(X, type="ca", power=1, keep.original.attributes = F){ # primarily for CA-based work.

  if( !(type %in% c("ca","rp","hellinger","power")) | is.null(type) | type=="none" ){
    attributes(X)$norm.type <- "none"
  }

  table.sum <- sum(X)
  if(type=="ca"){

    X <- X / table.sum
    attributes(X)$norm.type <- "ca"

  }else if(type=="rp"){

    row.sums <- rowSums(X)
    X <- sweep(X, 1, row.sums, "/")
    attributes(X)$norm.type <- "rp"
    attributes(X)$row.sums <- row.sums

  }else if(type=="hellinger"){

    X <- t(apply(table.scale(X,type="rp")),1,sqrt)
    attributes(X)$norm.type <- "hellinger"

  }else if(type=="power"){

    X <- t(apply(table.scale(X,type="rp")),1,function(x){x^power})
    attributes(X)$norm.type <- "power"
    attributes(X)$exponent <- power

  }

  attributes(X)$sum <- table.sum
  attributes(X)$func <- "table.scale"

  return(X)

}

#' @export
escofier.coding <- function(DATA, center=T, scale="SS1"){

  DATA <- expo.scale(DATA,center=center,scale=scale)
  dat.col.names <- c(paste0(colnames(DATA),"-"),paste0(colnames(DATA),"+"))
  DATA <- cbind( (1-DATA)/2, (1+DATA)/2 )
  colnames(DATA) <- dat.col.names

  DATA <- as.matrix(DATA)
  attributes(DATA)$variable.map <- gsub("\\-","",gsub("\\+","",dat.col.names))

  return(DATA)
}

#' @export
thermometer.coding <- function(DATA, mins, maxs, norm.to.one = T){

  if(missing(mins)){
    mins <- apply(DATA,2,min,na.rm=T)
  }else{

    if(length(mins)==ncol(DATA)){
      min.test <- mins > apply(DATA,2,min,na.rm=T)
      if(any(min.test)){
        warning("Some inputted minimums are greater than minimums in the data. We will replace those 'mins' with their respective minimum in the data")
        mins[which(min.test)] <- apply(DATA[,which(min.test)],2,min,na.rm=T)
      }
    }else{
      mins <- apply(DATA,2,min,na.rm=T)
    }

  }


  if(missing(maxs)){
    maxs <- apply(DATA,2,max,na.rm=T)
  }else{

    if(length(maxs)==ncol(DATA)){
      max.test <- maxs < apply(DATA,2,max,na.rm=T)
      if(any(max.test)){
        warning("Some inputted maximums are smaller than maximums in the data. We will replace those 'maxs' with their respective maximum in the data")
        maxs[which(max.test)] <- apply(DATA[,which(max.test)],2,max,na.rm=T)
      }
    }else{
      maxs <- apply(DATA,2,max,na.rm=T)
    }

  }

  dat.col.names <- c(paste0(colnames(DATA),"+"),paste0(colnames(DATA),"-"))


  from.mins <- sweep(DATA,2,mins,"-")

  if(norm.to.one){ ## these should be normed so that the variables = 1.
    DATA <- cbind(sweep( from.mins ,2,maxs,"/"), sweep( sweep(from.mins,2,maxs,"-") * -1,2,maxs,"/"))
  }else{
    DATA <- cbind( from.mins ,  sweep(from.mins,2,maxs,"-") * -1)
  }
  colnames(DATA) <- dat.col.names


  DATA <- as.matrix(DATA)
  attributes(DATA)$variable.map <- gsub("\\-","",gsub("\\+","",dat.col.names))

  return(DATA)
}


#' @export
make.data.nominal <- function(datain,impute.NA.to.mean=T){

  ## I can probably do this a bit faster with model.matrix

  data_dims <- dim(datain)
  var_names <- colnames(datain)
  ind_names <- rownames(datain)

  new.col.count <- sum(apply(datain,2,function(x){ uniq <- unique(x); length(uniq) - sum(is.na(uniq))	}))
  dataout <- matrix(0,nrow(datain),new.col.count)
  beginner <- 0
  variable.map <- new_colnames <- matrix(0, 1, 0)
  for (i in 1:data_dims[2]) {
    unique_elements <- unique(datain[, i])
    unique_no_na <- unique_elements[!is.na(unique_elements)]
    mini.mat <- matrix(0, data_dims[1], length(unique_no_na))
    for (j in 1:ncol(mini.mat)) {
      mini.mat[which(datain[, i] == unique_no_na[j]), j] <- 1
      new_colnames <- cbind(new_colnames, paste(var_names[i], ".", unique_no_na[j], sep = ""))
      variable.map <- cbind(variable.map,var_names[i])
    }

    ## here I need to be able to allow for these to remain as NA.
    these.missing <- which(rowSums(mini.mat)==0)
    if(length(these.missing)>0){
      if(impute.NA.to.mean){
        replacement <- colSums(mini.mat)/sum(colSums(mini.mat))
      }else{
        replacement <- rep(NA,ncol(mini.mat))
      }
      mini.mat[these.missing,] <- matrix(replacement,length(these.missing),length(replacement),byrow=T)
    }

    ender <- beginner + length(unique_no_na)
    dataout[, (beginner + 1):ender] <- mini.mat
    beginner <- ender
  }
  colnames(dataout) <- c(new_colnames)
  rownames(dataout) <- ind_names
  dataout <- as.matrix(dataout)
  attributes(dataout)$variable.map <- c(variable.map)


  return(dataout)
}


