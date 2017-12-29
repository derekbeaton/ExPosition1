#plsr
  ## this is a good first pass. It needs some restructuring/speed up but not much else (well, graphics...)

### NOTE: PLS analyses need their own class


sp.plsr <- function(X, Y, center.X = T, scale.X = "SS1", center.Y = T, scale.Y = "SS1", k = 0, compact = T, graphs = F, tol=.Machine$double.eps){

  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same number of rows.")
  }

      ## maybe I need a replacement for these to use a sweep or apply
        ## and maybe I can updated expo.scale to be a more minimal "class"
  X.orig <- X <- expo.scale(X, scale = scale.X, center = center.X)
    X.center <- attributes(X)$`scaled:center`
    X.scale <- attributes(X)$`scaled:scale`
  Y.orig <- Y <- expo.scale(Y, scale = scale.Y, center = center.Y)
    Y.center <- attributes(Y)$`scaled:center`
    Y.scale <- attributes(Y)$`scaled:scale`

  X.svd <- tolerance.svd(X)
    X.trace <- sum(X.svd$d^2)
    X.rank <- length(X.svd$d)
  Y.svd <- tolerance.svd(Y)
    Y.trace <- sum(Y.svd$d^2)
    Y.rank <- length(Y.svd$d)
  gc()

  Y.isEmpty <- X.isEmpty <- F


  if(k<1){
    k <- min(c(X.rank,Y.rank))
  }else{
    k <- min(c(k,X.rank,Y.rank))
  }

  pred.u.mat <- u.mat <- matrix(NA,ncol(X),k)
  v.mat <- matrix(NA,ncol(Y),k)
  t.mat <- LX.mat <- LY.mat <- matrix(NA,nrow(X),k)
  cross.trace <- r2.x <- r2.y <- delta.vec <- beta.vec <- rep(NA,k)
  if(!compact){
    X.resids <- X.hats <- array(NA,dim=c(nrow(X),ncol(X),k))
  }
  Y.resids <- Y.hats <- array(NA,dim=c(nrow(Y),ncol(Y),k))

  for(i in 1:k){

    res <- gsvd(t(X) %*% Y, k=1)  ## we could get real weird here some day. Basically a "predictive" CCA/RRR or even weighted PLSR.
      cross.trace[i] <- sum(res$d.orig^2)
    u.mat[,i] <- res$u
    v.mat[,i] <- res$v
    delta.vec[i] <- res$d
    rm(res) ## save space maybe

    LX.mat[,i] <- X %*% as.matrix(u.mat[,i])
    LY.mat[,i] <- Y %*% as.matrix(v.mat[,i])

    ## what makes it PLSR, essentially.
    t.mat[,i] <- as.matrix(LX.mat[,i] / sqrt(sum(LX.mat[,i]^2)))
    pred.u.mat[,i] <- t(X) %*% t.mat[,i]
    beta.vec[i] <- c(t(LY.mat[,i]) %*% t.mat[,i])


    ## these are component-wise/iterative hats and resids...
    X.rec <- t.mat[,i] %*% t(pred.u.mat[,i])
    X <- X - X.rec
      X[which(abs(X) < tol)] <- 0
      r2.x[i] <- (X.trace-sum(X^2)) / X.trace
    if(!compact){
      X.hats[,,i] <- X.rec * matrix(X.scale,nrow(X),ncol(X),byrow=T) + matrix(X.center,nrow(X),ncol(X),byrow=T)
      X.resids[,,i] <- (X.orig * matrix(X.scale,nrow(X),ncol(X),byrow=T) + matrix(X.center,nrow(X),ncol(X),byrow=T)) - X.hats[,,i]
    }

    Y.rec <- t(beta.vec[i] * t(t.mat[,i] %*% t(v.mat[,i])))
    Y <- Y - Y.rec
      Y[which(abs(Y) < tol)] <- 0
      r2.y[i] <- (Y.trace-sum(Y^2)) / Y.trace
    Y.hats[,,i] <- Y.rec * matrix(Y.scale,nrow(Y),ncol(Y),byrow=T) + matrix(Y.center,nrow(Y),ncol(Y),byrow=T)
    Y.resids[,,i] <- (Y.orig * matrix(Y.scale,nrow(Y),ncol(Y),byrow=T) + matrix(Y.center,nrow(Y),ncol(Y),byrow=T)) - Y.hats[,,i]


    if( sum(Y)==0 ){
      Y.isEmpty <- T
    }
    if( sum(X)==0 ){
      X.isEmpty <- T
    }
    if(Y.isEmpty | X.isEmpty){
      gc()
      break
    }
  }

    ## this is do-able for PLSC
  Y.rec <- (t.mat * matrix(beta.vec,nrow(Y),i,byrow=T)) %*% t(v.mat)
    Y.rec[which(abs(Y.rec) < tol)] <- 0
  Y.hat <-  Y.rec * matrix(Y.scale,nrow(Y),ncol(Y),byrow=T) + matrix(Y.center,nrow(Y),ncol(Y),byrow=T)
    Y.hat[which(abs(Y.hat) < tol)] <- 0
  Y.resid <- (Y.orig * matrix(Y.scale,nrow(Y),ncol(Y),byrow=T) + matrix(Y.center,nrow(Y),ncol(Y),byrow=T)) - Y.hat
  if(!compact){
    X.rec <- t.mat %*% t(pred.u.mat)
      X.rec[which(abs(X.rec) < tol)] <- 0
    X.hat <- X.rec * matrix(X.scale,nrow(X),ncol(X),byrow=T) + matrix(X.center,nrow(X),ncol(X),byrow=T)
      X.hat[which(abs(X.hat) < tol)] <- 0
    X.resid <- (X.orig * matrix(X.scale,nrow(X),ncol(X),byrow=T) + matrix(X.center,nrow(X),ncol(X),byrow=T)) - X.hat
  }

  ## just to conform to how it is done in PLSC
  fi <-  u.mat * matrix(delta.vec,nrow(u.mat),ncol(u.mat),byrow=T)
  fj <-  v.mat * matrix(delta.vec,nrow(v.mat),ncol(v.mat),byrow=T)

  if(compact){
    res <- list(fi=fi, fj=fj, d=delta.vec, u=u.mat, v=v.mat, lx=LX.mat, ly=LY.mat, Y.hats=Y.hats, Y.hat=Y.hat, Y.resid=Y.resid, r2.x=r2.x, r2.y=r2.y)
  }else{
    res <- list(fi=fi, fj=fj, d=delta.vec, cross.trace=cross.trace, u=u.mat, v=v.mat, lx=LX.mat, ly=LY.mat, beta=beta.vec, t=t.mat, pred.u=pred.u.mat, X.hats=X.hats, X.hat=X.hat, Y.hats=Y.hats, Y.hat=Y.hat, X.resids=X.resids, X.resid=X.resid, Y.resids=Y.resids, Y.resid=Y.resid, r2.x=r2.x, r2.y=r2.y)
  }

  ## this will take some thinkin'

  if(graphs){
    #sp.component_plot(res$fi)
    #sp.component_plot(res$fj)
    #sp.latentvar_plot(res)
    #sp.latentvar_plot(res,axis=2)
    #sp.scree(res$d.orig^2)
  }

  return(res)

}
