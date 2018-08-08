### these all look pretty good.

rm(list=ls())
roxygen2::roxygenise()
devtools::load_all(".")

library(ExPosition)
data(mca.wine)

mca.wine.res <- epMCA(mca.wine$data,graphs=F)
mca.wine.res_un <- epMCA(mca.wine$data,graphs=F, correction = NULL)

new.res <- ep.mca(mca.wine$data)
new.res_un <- ep.mca(mca.wine$data, benzecri.correction = F)



## these are OK
mca.wine.res$ExPosition.Data$fi / new.res$fi
mca.wine.res$ExPosition.Data$fj / new.res$fj
mca.wine.res$ExPosition.Data$pdq$Dv / new.res$d.orig
mca.wine.res$ExPosition.Data$t / new.res$tau


## these are OK
mca.wine.res_un$ExPosition.Data$fi / new.res_un$fi
mca.wine.res_un$ExPosition.Data$fj / new.res_un$fj
mca.wine.res_un$ExPosition.Data$pdq$Dv / new.res_un$d.orig
mca.wine.res_un$ExPosition.Data$t / new.res_un$tau

