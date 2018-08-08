#' @export
ep.mca <- function(DATA, make.data.nominal = T, asymmetric = F, benzecri.correction = T, k = 0, compact = T, graphs = F, tol=.Machine$double.eps){

  if(any(is.na(DATA))){
    stop("No NAs allowed.")
  }

  if(make.data.nominal){  ## if not I hope you know what you're doing!
    DATA <- make.data.nominal(DATA)
  }else{ ## all rowsums better be the same...
    stopifnot(all.equal(rowSums(DATA)))
  }
  num.variables <- rowSums(DATA)[1]

  # ship off to CA.
  res <- ep.ca(DATA,asymmetric=F, k=k, compact=F, graphs=F, tol=tol)

  if(benzecri.correction){

    keepers <- which(res$d.orig^2 > (1/num.variables))
    k.length <- length(res$d)
    this.k <- min(k.length,length(keepers))

    new.eigvals <- ((num.variables/(num.variables - 1)) * ( (res$d.orig[keepers]^2) - (1/num.variables)))^2
    res$d.orig <- sqrt(new.eigvals)
    res$tau <- (new.eigvals / sum(new.eigvals)) * 100


    scale.factors <- res$d.orig[1:this.k] / res$d[1:this.k]


    res$fi <- res$fi[,1:this.k]
    res$fj <- res$fj[,1:this.k]

    res$fi <- sweep(res$fi[,1:this.k], 2, scale.factors, "*")
    res$fj <- sweep(res$fj[,1:this.k], 2, scale.factors, "*")
    res$p <- res$p[,this.k]
    res$q <- res$q[,this.k]
    res$u <- res$u[,this.k]
    res$v <- res$v[,this.k]
    res$d <- res$d[this.k]
  }

  res$asymmetric <- asymmetric
  if(asymmetric){
    res$fj <- sweep(res$fj,2,res$d,"/")
  }

  res$compact <- F
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, tau = res$tau, d.orig=res$d.orig, u=res$u, v=res$v, wi = res$wi, wj = res$wj)
  }
  res$data.attributes <- attributes(DATA) ## maybe it's none?
  res$analysis <- "mca"
  class(res) <- "expo"

  if(graphs){
    max.lims <- apply(rbind(res$fi[,1:2], res$fj[,1:2]),2,max) * 1.25
    min.lims <- apply(rbind(res$fi[,1:2], res$fj[,1:2]),2,min) * 1.25
    xlims <- c(min.lims[1],max.lims[1])
    ylims <- c(min.lims[2],max.lims[2])
    c1.label <- paste0("Component 1: ", round(res$tau[1],digits=3), " % variance" )
    c2.label <- paste0("Component 2: ", round(res$tau[2],digits=3), " % variance" )

    plot(res,type="row.scores", main="Row component scores", xlim = xlims, ylim = ylims, xlab= c1.label, ylab = c2.label)
    plot(res,type="col.scores", main="Column component scores", xlim = xlims, ylim = ylims, xlab= c1.label, ylab = c2.label)
    plot(res,type="scree")
  }


  return(res)
}
