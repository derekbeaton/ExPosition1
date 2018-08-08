library(ExPosition)
data("beer.tasting.notes")
test.dat <- beer.tasting.notes$data


scale(test.dat) / data.norm(test.dat,type="z")
expo.scale(test.dat,scale="SS1") / data.norm(test.dat,type="ss1")
t(expo.scale(t(test.dat),scale="SS1")) / data.norm(test.dat,margin = 1,type="ss1")


row.norm(test.dat,type="ca") / rowNorms(test.dat,type = "ca")
row.norm(test.dat,type="hellinger") / rowNorms(test.dat, type="hellinger")
row.norm(test.dat,type="z") / rowNorms(test.dat,type="z")

row.norm(test.dat,type="ca") / data.norm(test.dat,margin = 1,type="ca")
row.norm(test.dat,type="hellinger") / data.norm(test.dat,margin = 1,type="hellinger")
row.norm(test.dat,type="z") / data.norm(test.dat,margin = 1,type="z")


t(rowNorms(t(test.dat),type="ca")) / data.norm(test.dat,margin = 2,type="ca")
t(rowNorms(t(test.dat),type="hellinger")) / data.norm(test.dat,margin = 2,type="hellinger")
t(rowNorms(t(test.dat),type="z")) / data.norm(test.dat,margin = 2,type="z")



t(scale(t(test.dat))) / data.norm(test.dat,margin = 1, type="scale", center = T, scale = T)
scale(test.dat) / data.norm(test.dat,margin = 2, type="scale", center = T, scale = T)



t(scale(t(test.dat),scale=F)) / data.norm(test.dat,margin = 1, type="scale", center = T, scale = F)
scale(test.dat,scale=F) / data.norm(test.dat,margin = 2, type="scale", center = T, scale = F)

t(scale(t(test.dat),center=F, scale=T)) / data.norm(test.dat,margin = 1, type="scale", center = F, scale = T)
scale(test.dat,center=F,scale=T) / data.norm(test.dat,margin = 2, type="scale", center = F, scale = T)



scale(test.dat,center=F,scale=T) / data.norm(test.dat,margin = 2, type="rms")


