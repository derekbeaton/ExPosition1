## thermometer coding
  ## base this off of what is given per variable, but users can input vectors of expected mins and maxes

thermometer.coding <- function(DATA, mins=NULL, maxs=NULL){

  if(length(mins)==1){
    mins <- rep(mins,ncol(DATA))
  }

  if(length(mins)==ncol(DATA)){
    min.test <- mins > apply(DATA,2,min)
    if(any(min.test)){
      warning("Some inputted minimums are larger than minimums in the data. We will replace those 'mins' with their respective minimum in the data")
      mins[which(min.test)] <- apply(DATA[,which(min.test)],2,min)
    }
  }else{
    mins <- apply(DATA,2,min)
  }



  if(length(maxs)==1){
    maxs <- rep(maxs,ncol(DATA))
  }

  if(length(maxs)==ncol(DATA)){
    max.test <- maxs < apply(DATA,2,max)
    if(any(max.test)){
      warning("Some inputted maximums are smaller than maximums in the data. We will replace those 'maxs' with their respective maximum in the data")
      maxs[which(max.test)] <- apply(DATA[,which(max.test)],2,max)
    }
  }else{
    maxs <- apply(DATA,2,max)
  }

  dat.col.names <- c(paste0(colnames(DATA),"-"),paste0(colnames(DATA),"+"))
  DATA <- cbind( (DATA-mins) / matrix(maxs,nrow(DATA),ncol(DATA),byrow=T), (maxs-DATA) / matrix(maxs,nrow(DATA),ncol(DATA),byrow=T))
  colnames(DATA) <- dat.col.names

  return(DATA)
}


