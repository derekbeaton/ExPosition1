## a generalized SVD function.

#  Generalized SVD: A GSVD function that takes in left and right constraints (usually diagonal matrices, but any positive semi-definite matrix is fine).
#   Constraints are applied to the left and right singular vectors for the orthogonality constraint.

#'
#'  @export
#'
#'  @title \code{gsvd}: the generalized singular value decomposition.
#'
#'  @description \code{gsvd} takes in left (\code{LW}) and right (\code{RW}) constraints (usually diagonal matrices, but any positive semi-definite matrix is fine) that are applied to the data (\code{DAT})
#'   Left and right constraints are used for the orthogonality conditions.
#'
#'  @param DAT a data matrix to decompose
#'  @param LW \bold{L}eft \bold{W}eights -- the constraints applied to the left side (rows) of the matrix and thus left singular vectors
#'  @param RW \bold{R}ight \bold{W}eights -- the constraints applied to the right side (rows) of the matrix and thus right singular vectors
#'  @param k total number of components to return though the full variance (based on nu and nv) will still be returned (see \code{Dv.orig})
#'  @param tol A tolerance level for eliminating (tiny variance or negative or imaginary) components. Default is .Machine$double.eps
#'
#'  @return
#'  A list with seven elements:
#'  \item{u} Left singular vectors. A matrix whose columns contain the left singular vectors of x, present if nu > 0. Dimension c(n, nu) but also accounting for \code{tol}.
#'  \item{v} Right singular vectors. A matrix whose columns contain the left singular vectors of x, present if nv > 0. Dimension c(p, nv) but also accounting for \code{tol}.
#'  \item{p} Left generalized singular vectors. A vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
#'  \item{q} Right generalized singular vectors. A vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
#'  \item{d} a vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
#'  \item{d.orig} a vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
#'  \item{tau} a vector that contains the (original) explained variance per component.
#'
#'  @seealso \code{\link{tolerance.svd}} and \code{\link{svd}}
#'
#'  @examples
#'  ## an example with correspondence analysis.
#'  authors <- rbind(
#'    cbind(7836, 13112, 6026),
#'    cbind(53655, 102383, 42413),
#'    cbind(115615, 184541, 59226),
#'    cbind(161926, 340479, 62754),
#'    cbind(38177, 105101, 12670),
#'    cbind(46371, 58367, 14299)
#'    )
#'
#'  Observed <- authors/sum(authors)
#'  row.w <- rowSums(Observed)
#'    row.W <- diag(1/row.w)
#'  col.w <- colSums(Observed)
#'    col.W <- diag(1/col.w)
#'  Expected <- row.w %o% col.w
#'  Deviations <- Observed - Expected
#'  ca.res <- gsvd(Deviations,row.W,col.W)
#'    ## fi & fj in example deprecated because this is now part of the GSVD return.
#'  #fi <- row.W %*% ca.res$p %*% diag(ca.res$d)
#'  #fj <- col.W %*% ca.res$q %*% diag(ca.res$d)
#'
#'  @author Derek Beaton
#'  @keywords multivariate, diagonalization, eigen



## I don't think I want to allow nu and nv here... but maybe.
#gsvd <- function(DAT, LW=NaN, RW=NaN, nu= min(dim(DAT)), nv = min(dim(DAT)), k = 0, tol=.Machine$double.eps){
gsvd <- function(DAT, LW, RW, k = 0, tol=.Machine$double.eps){

  # preliminaries
  DAT.dims <- dim(DAT)
  if(length(DAT.dims)!=2){
    stop("gsvd: DAT must have dim length of 2 (i.e., rows and columns)")
  }
  DAT <- as.matrix(DAT)
  DAT[abs(DAT) < tol] <- 0
  RW.is.vector <- LW.is.vector <- RW.is.missing <- LW.is.missing <- F

    # check if LW and RW are missing, if they are vectors, or if they are diagonal matrices.
      ## TODO: Check if LW or RW are identity or all 0s
    if( missing(LW) ){
      LW.is.missing <- T
    }else{
      if ( is.null(dim(LW)) & (length(LW) > 0) ) {
        if( (abs(max(LW) - min(LW)) < tol) ){# stolen from: https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
          LW.is.missing <- T
          warning("gsvd: LW was a diagonal matrix with identical elements. LW will not be used in the GSVD.")
        }else{
          LW.is.vector <- T
        }
      }else if(!LW.is.vector){
        if( is.identity.matrix(LW) | is.empty.matrix(LW) ){
          LW.is.missing <- T
          warning("gsvd: LW was an identity or empty matrix. LW will not be used in the GSVD.")
        }else if( is.diagonal.matrix(LW) ){
          LW <- diag(LW)
          if( (abs(max(LW) - min(LW)) < tol) ){# stolen from: https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
            LW.is.missing <- T
            warning("gsvd: LW was a diagonal matrix with identical elements. LW will not be used in the GSVD.")
          }else{
            LW.is.vector <- T  #now it's a vector
          }
        }else{
          stop("gsvd: unknown condition for LW.")
        }
      }else{
        stop("gsvd: unknown condition for LW.")
      }
    }

    if( missing(RW) ){
      RW.is.missing <- T
    }else{
      if ( is.null(dim(RW)) & (length(RW) > 0) ) {
        if( (abs(max(RW) - min(RW)) < tol) ){# stolen from: https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
          RW.is.missing <- T
          warning("gsvd: RW was a vector with identical elements. RW will not be used in the GSVD.")
        }else{
          RW.is.vector <- T
        }
      }else if(!RW.is.vector){
        if( is.identity.matrix(RW) | is.empty.matrix(RW) ){
          LW.is.missing <- T
          warning("gsvd: RW was an identity or empty matrix. RW will not be used in the GSVD.")
        }else if( is.diagonal.matrix(RW) ){
          RW <- diag(RW)
          if( (abs(max(RW) - min(RW)) < tol) ){# stolen from: https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
            RW.is.missing <- T
            warning("gsvd: RW was a diagonal matrix with identical elements. RW will not be used in the GSVD.")
          }else{
            RW.is.vector <- T  #now it's a vector
          }
        }else{
          stop("gsvd: unknown condition for RW.")
        }
      }else{
        stop("gsvd: unknown condition for RW.")
      }
    }

    ## these tests can be moved up but I just can't find a good place for them.
    if( LW.is.vector ){  ## replace with sweep
      if( length(LW)==nrow(DAT) ){
        #DAT <- matrix(sqrt(LW),nrow=nrow(DAT),ncol=ncol(DAT),byrow=F) * DAT
        DAT <- sweep(DAT,1,sqrt(LW),"*")
      }else{
        stop("gsvd:length(LW) does not equal nrow(DAT)")
      }
    }else if(!LW.is.missing){
      if( nrow(LW)==ncol(LW) & nrow(LW)==nrow(DAT)){
        #DAT <- power.rebuild_matrix(LW, power = 1/2) %*% DAT
        DAT <- (LW %^% (1/2)) %*% DAT
      }else{
        stop("gsvd:nrow(LW) does not equal ncol(LW) nor nrow(DAT)")
      }
    }


    if( RW.is.vector ){  ## replace with sweep
      if( length(RW)==ncol(DAT)){
        #DAT <- DAT * matrix(sqrt(RW),nrow=nrow(DAT),ncol=ncol(DAT),byrow=T)
        DAT <- sweep(DAT,2,sqrt(RW),"*")
      }else{
        stop("gsvd:length(RW) does not equal ncol(DAT)")
      }
    }else if(!RW.is.missing){
      if( nrow(RW)==ncol(RW) & nrow(RW)==ncol(DAT)){
        #DAT <- DAT %*% power.rebuild_matrix(RW, power = 1/2)
        DAT <- DAT %*% (RW %^% (1/2))
      }else{
        stop("gsvd:nrow(RW) does not equal ncol(RW) nor ncol(DAT)")
      }
    }


  if(k<=0){
    k <- min(nrow(DAT),ncol(DAT))
  }

  res <- tolerance.svd(DAT,nu=k,nv=k,tol=tol)

  res$d.orig <- res$d
  res$tau <- res$d.orig^2/sum(res$d.orig^2)
  components.to.return <- min(length(res$d.orig),k) #a safety check

  res$d <- res$d.orig[1:components.to.return]
    ## u and v should already be k vectors but again, be safe.
  res$u <- as.matrix(res$u[,1:components.to.return])
  res$v <- as.matrix(res$v[,1:components.to.return])


  if(LW.is.vector){
    res$p <- sweep(res$u,1,1/sqrt(LW),"*")
    res$fi <- sweep(sweep(res$p,1,LW,"*"),2,res$d,"*")
    #res$p <- matrix(1/sqrt(LW),nrow=nrow(res$u),ncol=ncol(res$u),byrow=F) * res$u
    #res$fi <- matrix(LW,nrow=nrow(res$p),ncol=ncol(res$p),byrow=F) * res$p * matrix(res$d,nrow(res$p),ncol(res$p),byrow=T)
  }else if(!LW.is.missing){
    #res$p <- power.rebuild_matrix(LW, power = -1/2) %*% res$u
    res$p <- (LW %^% (-1/2)) %*% res$u
    res$fi <- sweep((LW %*% res$p),2,res$d,"*")
    #res$fi <- LW %*% res$p * matrix(res$d,nrow(res$p),ncol(res$p),byrow=T)
  }else{
    res$p <- res$u
    res$fi <- sweep(res$p,2,res$d,"*")
    #res$fi <- res$p * matrix(d,nrow(p),ncol(p),byrow=T)
  }

  if(RW.is.vector){
    res$q <- sweep(res$v,1,1/sqrt(RW),"*")
    res$fj <- sweep(sweep(res$q,1,RW,"*"),2,res$d,"*")
    #res$q <- matrix(1/sqrt(RW),nrow=nrow(res$v),ncol=ncol(res$v),byrow=F) * res$v
    #res$fj <- matrix(RW,nrow=nrow(res$q),ncol=ncol(res$q),byrow=F) * res$q * matrix(res$d,nrow(res$q),ncol(res$q),byrow=T)
  }else if(!RW.is.missing){
    #res$q <- power.rebuild_matrix(RW, power = -1/2) %*% res$v
    res$q <- (RW %^% (-1/2)) %*% res$v
    res$fj <- sweep((RW %*% res$q),2,res$d,"*")
    #res$fj <- RW %*% res$q * matrix(res$d,nrow(res$q),ncol(res$q),byrow=T)
  }else{
    res$q <- res$v
    res$fj <- sweep(res$q,2,res$d,"*")
    #res$fj <- res$q * matrix(res$d,nrow(res$q),ncol(res$q),byrow=T)
  }

  rownames(res$fi) <- rownames(res$u) <- rownames(res$p) <- rownames(DAT)
  rownames(res$fj) <- rownames(res$v) <- rownames(res$q) <- colnames(DAT)

  return(res)
  #return(list(fi = fi, fj = fj, p = p, q = q, u = res$u, v = res$v, d = d, d.orig = d.orig, tau = tau))
}
