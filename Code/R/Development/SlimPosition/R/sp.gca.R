## generalized correspondence analysis, i.e., the Escofier DATA-MODEL+MARGINS approach.

sp.gca <- function(DATA, ROW.W=NULL, COL.W=NULL, MODEL=NULL, k = 0, compact = T, graphs = F){

  sum.data <- sum(DATA)

  if( length(ROW.W)==0 | length(ROW.W)!= nrow(DATA) ){
    ROW.W <- rowSums(DATA)/sum.data
  }
  if(length(COL.W)==0 | length(COL.W)!= ncol(DATA) ){
    COL.W <- colSums(DATA)/sum.data
  }
  if(length(MODEL)==0 | (nrow(DATA) != nrow(MODEL) & ncol(DATA) != ncol(MODEL)) ){
    MODEL <- ROW.W %o% COL.W
  }

  res <- gsvd( (DATA/sum.data) - MODEL, 1/ROW.W, 1/COL.W, k = k )

}
