#' @title Constrained principal component analysis (ConstPCA)
#' 
#' @description 
#' Constrained principal component analysis (ConstPCA)  is a method that has been 
#  developed by Yoshio Takane. In ConstPCA, first the data matrix 
#' DataX, is decomposed into several additive orthogonal parts according to the external
#' information associated with rowa and columns (External Analysis). In the seconed step, 
#' the decomposed parts (External Analysis) are subjected to PCA (Internal Analysis).
#'
#' @param DataX a numeric matrix or data frame of size (N * P)
#' @param DataG a row informationn matrix (column design matrix): external information associated with individuals (rows) (N * Q). DataG 
#' might be a Design matrix or a continus matrix. When there is no row external
#' information DataG is the identity matrix (IN). 
#' @param Center centering variables (columnwise centering). By default is TRUE. 
#' @param Scale scaling variables (columnwise standardization), by defalt is FALSE. By default variables are columnwise centered.
#' @return list with the following results:
#' @return \item{PG}{     projector associated with DataG}
#' @return \item{QG}{     orthogonal complement of PG}
#' @return \item{QH}{     orthogonal complement of PH}
#' @return \item{A B C D}{         External Analysis: decomposition of DataX by means of external informations A B C D}
#' @return \item{SVD A B C D}{     Internal Analysis: SVD on decomposition matrices}
#'
#' 
#' @references Y. Takane (2013). \emph{ Constrained Principal
#' Component Analysis and Related Techniques}, Chapman & Hall/CRC Monographs on 
#' Statistics & Applied Probability.
#' 

tepCPCA2 <- 
function(DATA1,DATA2,center1=TRUE,scale1="SS1",center2=TRUE,scale2="SS1",DESIGN=NULL,make_design_nominal=TRUE,graphs=TRUE,k=0)
{
  
  
  #------------- require library
  #require(MASS)  # ginv: calculate the Moore-Penrose generalized inverse of a matrix
  
  # # # #-------------  Checking  
  # # # if(!is.data.frame(DataX) & !is.matrix(DataX))
    # # # stop("\nOops the class of Data must be data.frame or matrix")
  
  # # # if(sum(is.na(DataX)) != 0)
    # # # stop("Oops there are missing values")
  
  # # # #---------------------
  # # # if (class(DataX) == 'data.frame') {
    # # # Data = as.matrix(DataX)
  # # # }


##must check for NAs -- across all functions.

	if(nrow(DATA1) != nrow(DATA2)){
		stop("DATA1 and DATA2 must have the same number of rows.")
	}

	main <- paste("Row CPCA: ",deparse(substitute(DATA1))," & ", deparse(substitute(DATA2)),sep="")
	DESIGN <- texpoDesignCheck(DATA1,DESIGN, make_design_nominal=make_design_nominal)
	DESIGN <- texpoDesignCheck(DATA2,DESIGN, make_design_nominal=FALSE)	

	DATA1 <- as.matrix(DATA1)
	DATA2 <- as.matrix(DATA2)	
	DATA1 <- expo.scale(DATA1,scale=scale1,center=center1)	
	DATA2 <- expo.scale(DATA2,scale=scale2,center=center2)	
    
  #-------------   
  N = dim(DataX)[1]                       #----number of individuals
  
  
  
  #------------- results
  res = list()
  res$DataX = DataX
  
  #------------- Metric matrices 
  DataK = diag(N)
  
  #------------- preprocessing: scaling DataX
  DataX <- DATA1
  DataG <- DATA2
  if(is.null(rownames(DataX))) {rownames(DataX) = paste(1:nrow(DataX))}
  if(is.null(colnames(DataX))) {colnames(DataX) = paste("V", 1:ncol(DataX), sep="")}
  
  
  #---------------------------------------- External Analysis
  #--------------- Orthogonal projectors 
  res$PG = PG = DataG %*% ginv(t(DataG) %*% DataG) %*% t(DataG)
  res$QG = QG = diag(N) - PG
  
  
  #--------------- Decomposition of DataX by means of external informations
  #----------------------------- page 79 eq. (3.25)
  # DataX = A + B + C + D 
  # The most dominant tendency in the data than can be explined by both G and H
  A.mat = PG %*% DataX
  colnames(A.mat) = colnames(DataX)
  rownames(A.mat) = rownames(res$DataX) 
  
  B.mat = QG %*% DataX
  colnames(B.mat) = colnames(DataX)
  rownames(B.mat) = rownames(res$DataX)
  
  
  
  #---------------------------------------- Internal Analysis ; have to integrate this with genPDQ() 
  # A.svd = svd(A.mat)
  
  # rownames(A.svd$u) = rownames(DataX)
  # colnames(A.svd$u) = paste("Dim", 1:ncol(A.svd$u), sep="")
  # rownames(A.svd$v) = colnames(DataX)
  # colnames(A.svd$v) = paste("Dim", 1:ncol(A.svd$v), sep="")
  
  # res$A$PCA$components = A.svd$u %*% diag(A.svd$d)
  # res$A$PCA$loadings   = A.svd$v %*% diag(A.svd$d) / sqrt(N)
  # colnames(res$A$PCA$components) = paste("Dim", 1:ncol(A.svd$u), sep="")
  # colnames(res$A$PCA$loadings )  = paste("Dim", 1:ncol(A.svd$u), sep="")
  
  A.res <- corePCA(A.mat)
  A.res2 <- genPDQ(DataX, t(PG) %*% PG,diag(ncol(DataX)))
  	##Can I use the GSVD directly?
   
  #---------------------------------------- Internal Analysis 
  # B.svd = svd(B.mat)
  # rownames(B.svd$u) = rownames(DataX)
  # colnames(B.svd$u) = paste("Dim", 1:ncol(B.svd$u), sep="")
  # rownames(B.svd$v) = colnames(DataX)
  # colnames(B.svd$v) = paste("Dim", 1:ncol(B.svd$v), sep="")
  
  # res$B$PCA = list()
  # res$B$PCA$components = B.svd$u %*% diag(B.svd$d)
  # res$B$PCA$loadings   = B.svd$v %*% diag(B.svd$d) / sqrt(N)
  # colnames(res$B$PCA$components) = paste("Dim", 1:ncol(B.svd$u), sep="")
  # colnames(res$B$PCA$loadings )  = paste("Dim", 1:ncol(B.svd$u), sep="")
  
  B.res <- corePCA(B.mat)
  B.res2 <- genPDQ(DataX, t(QG) %*% QG,diag(ncol(DataX)))
  	##Can I use the GSVD directly?  
    
  return(res) 
}