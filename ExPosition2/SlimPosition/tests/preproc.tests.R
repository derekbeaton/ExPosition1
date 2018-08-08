### these all look pretty good.

rm(list=ls())
roxygen2::roxygenise()
devtools::load_all(".")

library(ExPosition)
data("beer.tasting.notes")
test.dat <- beer.tasting.notes$data


scale(test.dat) / margin.scale(test.dat,type="z")
expo.scale(test.dat,scale="SS1") / margin.scale(test.dat,type="ss1")
t(expo.scale(t(test.dat),scale="SS1")) / margin.scale(test.dat,margin = 1,type="ss1")




row.scale(test.dat,type="rp") / rowNorms(test.dat,type = "ca")
row.scale(test.dat,type="hellinger") / rowNorms(test.dat, type="hellinger")
row.scale(test.dat,type="z") / rowNorms(test.dat,type="z")

row.scale(test.dat,type="rp") / margin.scale(test.dat,margin = 1,type="rp")
row.scale(test.dat,type="hellinger") / margin.scale(test.dat,margin = 1,type="hellinger")
row.scale(test.dat,type="z") / margin.scale(test.dat,margin = 1,type="z")


t(rowNorms(t(test.dat),type="ca")) / margin.scale(test.dat,margin = 2,type="rp")
t(rowNorms(t(test.dat),type="hellinger")) / margin.scale(test.dat,margin = 2,type="hellinger")
t(rowNorms(t(test.dat),type="z")) / margin.scale(test.dat,margin = 2,type="z")



t(scale(t(test.dat))) / margin.scale(test.dat,margin = 1, type="scale", center = T, scale = T)
scale(test.dat) / margin.scale(test.dat,margin = 2, type="scale", center = T, scale = T)



t(scale(t(test.dat),scale=F)) / margin.scale(test.dat,margin = 1, type="scale", center = T, scale = F)
scale(test.dat,scale=F) / margin.scale(test.dat,margin = 2, type="scale", center = T, scale = F)

t(scale(t(test.dat),center=F, scale=T)) / margin.scale(test.dat,margin = 1, type="scale", center = F, scale = T)
scale(test.dat,center=F,scale=T) / margin.scale(test.dat,margin = 2, type="scale", center = F, scale = T)



scale(test.dat,center=F,scale=T) / margin.scale(test.dat,margin = 2, type="rms")

t(scale(t(test.dat),center=F,scale=T)) / margin.scale(test.dat,margin = 1, type="rms")





### need some tests for the "scale" option. but will hold off on that for now.
