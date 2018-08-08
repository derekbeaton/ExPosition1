### these all look pretty good.

rm(list=ls())
roxygen2::roxygenise()
devtools::load_all(".")

library(ExPosition)
data("authors")


orig.res <- epCA(authors$ca$data, graphs=F)
new.res <- ep.ca(authors$ca$data)

orig.res$ExPosition.Data$fi / new.res$fi
orig.res$ExPosition.Data$fj / new.res$fj
orig.res$ExPosition.Data$pdq$Dv / new.res$d.orig
orig.res$ExPosition.Data$t / new.res$tau



orig.res.a <- epCA(authors$ca$data, graphs=F, symmetric = F)
new.res.a <- ep.ca(authors$ca$data, asymmetric = T)
orig.res.a$ExPosition.Data$fi / new.res.a$fi
orig.res.a$ExPosition.Data$fj / new.res.a$fj
orig.res.a$ExPosition.Data$pdq$Dv / new.res.a$d.orig
orig.res.a$ExPosition.Data$t / new.res.a$tau




ep.ca(authors$ca$data, graphs=T)

plot(new.res,type="row.scores")
plot(new.res,type="row.loadings")
plot(new.res,type="col.scores")
plot(new.res,type="col.loadings")
plot(new.res,type="scree")
