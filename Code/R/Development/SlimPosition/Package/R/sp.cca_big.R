sp.cca_big <- function(X, Y, center.X = T, scale.X = "SS1", center.Y = T, scale.Y = "SS1", k = 0, compact = T, graphs = F){

  X <- expo.scale(X, center = center.X, scale = scale.X)
  Y <- expo.scale(Y, center = center.Y, scale = scale.Y)


  # # ## I know that there are a few steps here I can remove/speed up; not everything here is required.

  # # dat <- t(power.rebuild_matrix(X,1/2)) %*% (power.rebuild_matrix(X,1/2) %*% (t(X) %*% Y) %*% power.rebuild_matrix(t(Y),1/2)) %*% power.rebuild_matrix(Y,1/2)

  # # res <- tolerance.svd(dat)
  # # if(k<=0){
    # # k <- min(min(nrow(dat),ncol(dat)),length(res$d))
  # # }
  # # res$d.orig <- res$d
  # # res$d <- res$d[1:k]
  # # res$u <- res$u[,1:k]
  # # res$v <- res$v[,1:k]

  # # res$p <- power.rebuild_matrix(t(X),-1/2) %*% (power.rebuild_matrix(X,-1/2) %*% res$u)
  # # res$q <- power.rebuild_matrix(t(Y),-1/2) %*% (power.rebuild_matrix(Y,-1/2) %*% res$v)

  # # res$fi <- t(X) %*% ((X %*% res$p) * matrix(res$d,nrow(X),length(res$d),byrow=T))
  # # res$fj <- t(Y) %*% ((Y %*% res$q) * matrix(res$d,nrow(Y),length(res$d),byrow=T))

  # # res$lx <- (X %*% res$fi) * matrix(1/res$d,nrow(Y),length(res$d),byrow=T)
  # # res$ly <- (Y %*% res$fj) * matrix(1/res$d,nrow(X),length(res$d),byrow=T)

  # # if(graphs){
    # # sp.component_plot(res$fi)
    # # sp.component_plot(res$fj)
    # # sp.latentvar_plot(res)
    # # sp.latentvar_plot(res,axis=2)
    # # sp.scree(res$d.orig^2)
  # # }
  # # if(compact){
    # # res <- list(fi=res$fi, fj=res$fj, d.orig=res$d.orig, u=res$u, v=res$v, lx=res$lx, ly=res$ly)
  # # }

  # # return(res)

}
