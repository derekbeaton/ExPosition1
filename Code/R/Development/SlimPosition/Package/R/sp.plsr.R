#plsr

### NOTE: PLS analyses need their own class

sp.plsr <- function(X, Y, center.X = T, scale.X = "SS1", center.Y = T, scale.Y = "SS1", k = 0, compact = T, graphs = F, tol=.Machine$double.eps){

  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same number of rows.")
  }

      ## maybe I need a replacement for these to use a sweep or apply
        ## and maybe I can updated expo.scale to be a more minimal "class"
  Xorig <- X <- expo.scale(X, scale = scale.X, center = center.X)
    X.center <- attributes(X)$`scaled:center`
    X.scale <- attributes(X)$`scaled:scale`
  Yorig <- Y <- expo.scale(Y, scale = scale.Y, center = center.Y)
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

  pLX.mat <- u.mat <- matrix(NA,ncol(X),k)
  v.mat <- matrix(NA,ncol(Y),k)
  t.mat <- LX.mat <- LY.mat <- matrix(NA,nrow(X),k)
  r2.x <- r2.y <- delta.vec <- beta.vec <- rep(NA,k)
  if(!compact){
    X.resids <- X.hats <- array(NA,dim=c(nrow(X),ncol(X),k))
  }
  Y.resids <- Y.hats <- array(NA,dim=c(nrow(Y),ncol(Y),k))

  for(i in 1:k){

    res <- gsvd(t(X) %*% Y, k=1)  ## we could get real weird here some day. Basically a "predictive" CCA/RRR or even weighted PLSR.
    u.mat[,i] <- res$u
    v.mat[,i] <- res$v
    delta.vec[i] <- res$d
    rm(res) ## save space if possible.

    LX.mat[,i] <- X %*% as.matrix(u.mat[,i])
    LY.mat[,i] <- Y %*% as.matrix(v.mat[,i])

    ## what makes it PLSR, essentially.
    t.mat[,i] <- as.matrix(LX.mat[,i] / sqrt(sum(LX.mat[,i]^2)))
    pLX.mat[,i] <- t(X) %*% t.mat[,i]
    beta.vec[i] <- c(t(LY.mat[,i]) %*% t.mat[,i])


      ##  there has to be a way to combine these...
    X <- X-(t.mat[,i] %*% t(pLX.mat[,i]))
      X[which(abs(X) < tol)] <- 0
      r2.x[i] <- (X.trace-sum(X^2)) / X.trace
    Y <- Y-t(beta.vec[i] * t(t.mat[,i] %*% t(v.mat[,i])))
      Y[which(abs(Y) < tol)] <- 0
      r2.y[i] <- (Y.trace-sum(Y^2)) / Y.trace

    ## don't do all of these if compact==T
    #### NOTE: This is doable for PLSC

    Y.hats[,,i] <- ( ((t.mat[,1:i] * matrix(beta.vec[1:i],nrow(Y),i,byrow=T)) %*% t(v.mat[,1:i])) * matrix(Y.scale,nrow(Y),ncol(Y),byrow=T) + matrix(Y.center,nrow(Y),ncol(Y),byrow=T))
    Y.resids[,,i] <- Yorig - Y.hats[,,i]
    if(!compact){
      X.hats[,,i] <- ( (t.mat[,1:i] %*% t(pLX.mat[,1:i])) * matrix(X.scale,nrow(X),ncol(X),byrow=T) + matrix(X.center,nrow(X),ncol(X),byrow=T))
      X.resids[,,i] <- Xorig - X.hats[,,i]
    }



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

  Xrec <- t.mat %*% t(pLX.mat)
  Xhat <- (Xrec * matrix(attributes(X)$`scaled:scale`,nrow(X),ncol(X),byrow=T) + matrix(attributes(X)$`scaled:center`,nrow(X),ncol(X),byrow=T))

  Yrec <- (t.mat * matrix(beta.vec,nrow(Y),length(beta.vec),byrow=T)) %*% t(v.mat)
  Yhat <- (Yrec * matrix(attributes(Y)$`scaled:scale`,nrow(Y),ncol(Y),byrow=T) + matrix(attributes(Y)$`scaled:center`,nrow(Y),ncol(Y),byrow=T))



  ## just to conform to how it is done in PLSC
  fi <-  u.mat * matrix(delta.vec,nrow(u.mat),ncol(u.mat),byrow=T)
  fj <-  v.mat * matrix(delta.vec,nrow(v.mat),ncol(v.mat),byrow=T)

  if(compact){
    #res <- list(fi=res$fi, fj=res$fj, d.orig=res$d.orig, u=res$u, v=res$v, lx=res$lx, ly=res$ly)
    res <- list(fi=fi, fj=fj, d=delta.vec, u=u.mat, v=v.mat, lx=LX.mat, ly=LY.mat)
  }else{
    res <- list(fi=fi, fj=fj, d=delta.vec, u=u.mat, v=v.mat, lx=LX.mat, ly=LY.mat, beta=beta.vec, t=t.mat, pLX=pLX.mat, Xhat=Xhat, Yhat=Yhat, X.hats=X.hats, Y.hats=Y.hats, X.resids=X.resids, Y.resids=Y.resids, r2.x=r2.x, r2.y=r2.y)
  }

  ## what is the common variance?

  if(graphs){
    #sp.component_plot(res$fi)
    #sp.component_plot(res$fj)
    #sp.latentvar_plot(res)
    #sp.latentvar_plot(res,axis=2)
    #sp.scree(res$d.orig^2)
  }

  return(res)

}
