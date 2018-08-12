ep.conditional.ca <- function(DATA, CONFOUND=NULL, make.confound.nominal = F, asymmetric = F, k = 0, compact = T, graphs = F){

  if(is.null(CONFOUND)){
    stop("A CONFOUND is required. If you do not have one please use standard correspondence anlaysis (see ?ep.ca)")
  }

  ## have to come up with a DESIGN matrix test like I had in the original EXPO

  if(make.confound.nominal){  ## if not I hope you know what you're doing!
    CONFOUND <- make.data(CONFOUND)
  }
    ## actually replace stopifnot so I can be more verbose
  stopifnot(all.equal(rowSums(CONFOUND)),
            all.equal(rowSums(CONFOUND), rep(1,nrow(CONFOUND)))
            )




  DATA <- table.scale(DATA, type="ca") # no user choice here.
  wi <- rowSums(DATA)
  wj <- colSums(DATA)

  MODEL <- (CONFOUND/ attributes(DATA)$sum ) %*% sweep(t(CONFOUND) %*% (DATA * attributes(DATA)$sum), 1, colSums(CONFOUND), "/")
    ## maybe this can move off to its own function at some point?
  DATA.prime <- (DATA - MODEL + (wi %o% wj)) * attributes(DATA)$sum
    ## or maybe actually this should ship off to the gca...? nah keep it simple here.

  # ship off to CA.
  res <- ep.ca(DATA.prime, asymmetric=asymmetric, k=k, compact=compact, graphs=F, tol=tol)


  res$analysis <- "conditional.ca"
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
