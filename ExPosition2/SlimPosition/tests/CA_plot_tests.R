### these all look pretty good.

rm(list=ls())
roxygen2::roxygenise()
devtools::load_all(".")

library(ExPosition)
data("authors")


orig.res <- epCA(beer.tasting.notes$data, graphs=F)
new.res <- ep.ca(beer.tasting.notes$data)

orig.res$ExPosition.Data$fi / new.res$fi
orig.res$ExPosition.Data$fj / new.res$fj
orig.res$ExPosition.Data$pdq$Dv / new.res$d.orig
orig.res$ExPosition.Data$t / new.res$tau



ep.ca(beer.tasting.notes$data, graphs=T)

plot(new.res,type="row.scores")
plot(new.res,type="col.scores")
plot(new.res,type="col.loadings")
plot(new.res,type="scree")
