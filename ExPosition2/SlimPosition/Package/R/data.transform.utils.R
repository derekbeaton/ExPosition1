# ExPosition data transform utilities.

#' @export
svd.norm <- function(x){

  return(x/tolerance.svd(x)$d[1])
  ## alternative (which is likely safer):
  #res <- tolerance.svd(x,...)
  #return( (res$u * matrix(res$d/res$d[1],nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )

}


##### ROLL THIS BACK
  ### make this more akin to the existing rowNorms
  ### but margin==1 will just t()
  ### also record what happens to center/scale?
#' Compute standardization/normalization for rows or columns of a matrix.
#' @title \code{data.norm}
#' @param X a matrix for input
#' @param type the type of scaling to perform. Options: "ca" which is item divided by row sums, "hellinger" which is sqrt of "ca", "z" which is the same as scale(x), and "ss1" which is sum of squares 1. Also available is "scale" and requires use of center and scale parameters
#' @param center the intended center (see \code{\link{scale}})
#' @param scale the intended scale (see \code{\link{scale}})
#' @param margin which margin to perform this on (i.e., 1 for rows and 2 for columns)
#' @return column or row normalized version of the matrix.
#' @export
data.norm <- function(X,type=NULL,center=F,scale=F,margin=2){

  ## this eventually needs simplification..

  if( !(margin %in% c(1,2)) ){
    stop("`margin` must be either 1 or 2.")
  }
  orig.dims <- dim(X)
  if(length(orig.dims) > 2){
    stop("X is required to be a matrix.")
  }
  orig.names <- dimnames(X)

  if(is.null(type) & (center==F | is.na(center) | is.null(center)) & (scale==F | is.na(scale) | is.null(scale))){
    return(X)
  }else if(type=="ca"){

    X <- apply(X,margin,function(x){ x/sum(x) })

    attributes(X)$`scaled:type` <- "ca"
    attributes(X)$`scaled:center` <- 0
    attributes(X)$`scaled:scale` <- 1

  }else if(type=="hellinger"){

    X <- apply(X,margin,function(x){ sqrt(x/sum(x)) })

    attributes(X)$`scaled:type` <- "hellinger"
    attributes(X)$`scaled:center` <- 0
    attributes(X)$`scaled:scale` <- 1

  }else if(type == "z"){ ## this can be replaced by a scale call... except the t()
    # dat.colmeans <- apply(X,margin,means)
    # dat.sd <- apply(X,margin,sd)

    X <- ifelse(margin==2,scale(X,center=T,scale=T),scale(t(X),center=T,scale=T))
      ## can be replaced by sweeps?
    #X <- apply(X,margin,function(x){scale(x,center=T,scale=T)})
    # attributes(X)$`scaled:center` <- dat.colmeans
    # attributes(X)$`scaled:scale` <- dat.sd

    attributes(X)$`scaled:type` <- "z"

  }else if(type=="ss1"){ ## this can be replaced by a scale call... except the t()

    X <- ifelse(margin==2,scale(X,center=T,scale=T),scale(t(X),center=T,scale=T))
    X <- X / sqrt(orig.dims[-margin]-1)
      ## can be replaced by sweeps?
    #X <- apply(X,margin,function(x){scale(x,center=T,scale=sd(x))}) / sqrt(orig.dims[-margin]-1)
    # attributes(X)$`scaled:center` <- colMeans(X)
    attributes(X)$`scaled:scale` <- attributes(X)$`scaled:scale`*(sqrt(orig.dims[-margin]-1))

    attributes(X)$`scaled:type` <- "ss1"


  }else if(type=="rms"){
      ## can be replaced by sweeps?
    #X <- apply(X,margin,function(x){scale(x,center=F,scale=sd(x))})
    X <- ifelse(margin==2,scale(X,center=T,scale=T),scale(t(X),center=F,scale=T))
    attributes(X)$`scaled:center` <- rep(0,orig.dims[margin])
    #attributes(X)$`scaled:scale` <- apply(X, margin, sd)
    attributes(X)$`scaled:type` <- "rms"

  }else if(type=="scale"){

    ## I can probably replace this with some sweeps:
    ## ok do this with sweeps...
    ## check if c/s are logical, or numeric of size 1, or if they are vectors of length of margin
    ## if none of those fail...
    ## then it's a double sweep: center & scale.
        ## actually -- it is probably faster if I just let scale() do the work.

    if(margin==1){
      X <- scale(t(X),center=center,scale=scale)
    }else{
      X <- scale(X,center=center,scale=scale)
    }

    attributes(X)$`scaled:type` <- "scale"
    if( !any(names(attributes(X)) %in% c("scaled:center")) ){
      attributes(X)$`scaled:center` <- rep(0,orig.dims[margin])
    }
    if( !any(names(attributes(X)) %in% c("scaled:scale")) ){
      attributes(X)$`scaled:scale` <- rep(1,orig.dims[margin])
    }


  }else{
    return(X)
  }

  attributes(X)$`scaled:margin` <- margin

  new.dims <- dim(X)
  if(new.dims[1]==orig.dims[2] & new.dims[2]==orig.dims[1]){
    X <- t(X)
  }
  dimnames(X) <- orig.names


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

# ## consider not using this one...; or calling into data.norms
# rowNorms <- function(X,type=NULL,center=FALSE,scale=FALSE){
#   if(is.null(type)){
#     return(X)
#   }else if(type=='hellinger'){
#     return(sqrt(X/matrix(rowSums(X),nrow(X),ncol(X))))
#   }else if(type == 'ca'){
#     return(X/matrix(rowSums(X),nrow(X),ncol(X)))
#   }else if (type == 'z'){
#     return(t(apply(X,1,scale,T,T)))
#   }else if(type == 'other'){
#     ## this one is expensive.
#     return(t(expo.scale(t(X),center=center,scale=scale)))
#   }else{
#     return(X)
#   }
# }
#
# ## consider not using this one...; or calling into data.norms
#   ## or into expo.scale...
# colNorms <- function(x){
#
# }

#' @export
row.norm <- function(X,type=NULL,center=T,scale=F){
  return(data.norm(X,type=type,center=center,scale=scale,margin=1))
}
#' @export
col.norm <- function(X,type=NULL,center=T,scale=F){
  return(data.norm(X,type=type,center=center,scale=scale,margin=2))
}

#' @export
expo.scale <- function(DATA,center=TRUE,scale=TRUE){

  column.names <- colnames(DATA)
  DATA_dims <- dim(DATA)

  ######THIS BLOCK INTENDED TO CREATE CENTERS AND SCALES BASED ON REQUESTS.
  if(class(scale)=="character"){
    if(tolower(scale)=="ss1"){ ##if you want to get SS1
      if(is.logical(center) && center){
        center <- apply(DATA, 2, mean, na.rm = TRUE)
      }else if(is.logical(center) && !center){
        center <- rep(0,DATA_dims[2])
      }##else, you are on your own. I perform rudimentary checks later for lengths and whatnot.
      scale <- apply(DATA, 2, sd, na.rm = TRUE)*(sqrt(DATA_dims[1]-1))
    }
    else if(tolower(scale)=="sd"){ ##if you want to get sd-norm; no center.
      center <- rep(0,DATA_dims[2])
      scale <- apply(DATA, 2, sd, na.rm = TRUE)
    }
    else if(tolower(scale)=="rms"){ ##I probably don't need this, but will be consistent.
      if(is.logical(center) && !center){
        center<-FALSE #=rep(0,DATA_dims[2])
        scale<-TRUE #=sqrt(colSums(DATA^2)/(nrow(DATA)-1))
      }else if(is.logical(center) && center){ ##you just wanted a z score then...
        center<-TRUE #=apply(DATA, 2, mean, na.rm = TRUE)
        scale<-TRUE #=apply(DATA, 2, sd, na.rm = TRUE)
      }##else, you are on your own. I perform rudimentary checks later for lengths and whatnot.
    }
    else if(tolower(scale)=="z"){ ##both z and SS1 need center/scale
      center<-TRUE #=apply(DATA, 2, mean, na.rm = TRUE)
      scale<-TRUE #=apply(DATA, 2, sd, na.rm = TRUE)
    }else if(tolower(center)=="median" | tolower(center)=="med"){
      center <- apply(DATA,2,median,na.rm=T)
      if(tolower(scale)=="mad"){
        scale <- apply(DATA,2,mad,na.rm=T)
      }else if(tolower(scale)=="iqr"){
        scale <- apply(DATA,2,IQR,na.rm=T)
      }
    }
    else{ ## you made a booboo
      center<-TRUE
      scale<-TRUE
      print("Something is wrong with 'center' and 'scale'. 'center' and 'scale' both set to TRUE.")
    }
    ##will include other normalization schemes in the future. The Mu-methods will need row norms and other norms.
  }


  ######THIS BLOCK INTENDED TO PERFORM A SET OF CHECKS
  if((!is.logical(center)) && (!(class(center)=="numeric" && length(center)==DATA_dims[2]))){
    center <- TRUE
    print("Something is wrong with 'center'. 'center' set to TRUE.")
  }
  if((!is.logical(scale)) && (!(class(scale)=="numeric" && length(scale)==DATA_dims[2]))){
    scale <- TRUE
    print("Something is wrong with 'scale'. 'scale' set to TRUE.")
  }


  ###NOW PERFORM THE ACTUAL NORMS.
  scale.info <- scale(DATA,center=center,scale=scale)

  #center checks
  center.out <- attributes(scale.info)$`scaled:center`
  if(is.null(center.out)){
    center.out <- rep(0,DATA_dims[2]) ##create a 0 center
  }
  if(is.null(names(center.out))){
    names(center.out) <- column.names
  }
  #scale checks
  scale.out <- attributes(scale.info)$`scaled:scale`
  if(is.null(scale.out)){
    scale.out <- rep(1,DATA_dims[2]) ##create a 1 scale
  }
  if(is.null(names(scale.out))){
    names(scale.out) <- column.names
  }

  #this forces every data matrix to pass through here to have a center and a scale attribute.
  attributes(scale.info)$`scaled:center` <- center.out
  attributes(scale.info)$`scaled:scale` <- scale.out
  #recall DATA - 0 center * 1 scale = DATA.
  return(scale.info)
}
