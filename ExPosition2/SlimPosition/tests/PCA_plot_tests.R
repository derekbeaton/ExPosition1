### these all look pretty good.

rm(list=ls())
roxygen2::roxygenise()
devtools::load_all(".")

library(ExPosition)
data("beer.tasting.notes")


orig.res <- epPCA(beer.tasting.notes$data, scale = "SS1",graphs=F)
new.res <- ep.pca(beer.tasting.notes$data,col.scale.type = "ss1")

orig.res$ExPosition.Data$fi / new.res$fi
orig.res$ExPosition.Data$fj / new.res$fj
orig.res$ExPosition.Data$pdq$Dv / new.res$d.orig
orig.res$ExPosition.Data$t / new.res$tau



ep.pca(beer.tasting.notes$data,col.scale.type = "ss1", graphs=T)
plot(new.res,type="row.scores")
plot(new.res,type="col.scores")
plot(new.res,type="col.loadings")
plot(new.res,type="scree")
