####This script shows how to use a variety of genetic inheritance models with PLSCA.
###These match the ideas presented in the paper (co-dominant) as well as Appendix B.


rm(list=ls())
gc()

source('LibsFuncs.R')

load('../Data/SNPS.rda') 	##SNPS data
load('../Data/TRAITS.rda')	##TRAITS data
load('../Data/DES.rda')		##Participant design matrix -- indicates e.g., group relationship


##The trait data will be used exactly as is for all analyses in this script.
cat.TRAITS <- makeNominalData(TRAITS)
trait.names <- unlist(lapply(strsplit(colnames(cat.TRAITS),"\\."),function(x){x[3]}))
trait.colors <- as.matrix(trait.names)
trait.colors <- replace(trait.colors,trait.colors=="HI",'firebrick3')
trait.colors <- replace(trait.colors,trait.colors=="MID",'goldenrod')
trait.colors <- replace(trait.colors,trait.colors=="LOW",'olivedrab4')

##participant colors
part.colors <- createColorVectorsByDesign(DES)


##Full categorical i.e., co-dominant
CD.SNPS <- SNP.model.maker(SNPS,'c')
cd.zygosity <- unlist(lapply(strsplit(colnames(CD.SNPS$SNPS),"\\."),function(x){x[3]}))
cd.colors <- as.matrix(cd.zygosity)
cd.colors <- replace(cd.colors, cd.colors =="AA",'steelblue4')
cd.colors <- replace(cd.colors, cd.colors =="Aa",'mediumorchid4')
cd.colors <- replace(cd.colors, cd.colors =="aa",'orangered')
res<-tepPLSCA(CD.SNPS$SNPS, cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- cd.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Dominant
D.SNPS <- SNP.model.maker(SNPS,'d')
d.zygosity <- unlist(lapply(strsplit(colnames(D.SNPS$SNPS),"\\."),function(x){x[3]}))
d.colors <- as.matrix(d.zygosity)
d.colors <- replace(d.colors, d.colors =="ND",'steelblue4')
d.colors <- replace(d.colors, d.colors =="D",'orangered')
res<-tepPLSCA(D.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- d.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Recessive
R.SNPS <- SNP.model.maker(SNPS,'r')
r.zygosity <- unlist(lapply(strsplit(colnames(R.SNPS$SNPS),"\\."),function(x){x[3]}))
r.colors <- as.matrix(r.zygosity)
r.colors <- replace(r.colors, r.colors =="NR",'steelblue4')
r.colors <- replace(r.colors, r.colors =="R",'orangered')
res<-tepPLSCA(R.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- r.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Overdominant
O.SNPS <- SNP.model.maker(SNPS,'o')
o.zygosity <- unlist(lapply(strsplit(colnames(O.SNPS$SNPS),"\\."),function(x){x[3]}))
o.colors <- as.matrix(o.zygosity)
o.colors <- replace(o.colors, o.colors =="NOD",'steelblue4')
o.colors <- replace(o.colors, o.colors =="OD",'orangered')
res<-tepPLSCA(O.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- r.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Additive -- equal emphasis on allele for the heterozygote
A.SNPS <- SNP.model.maker(SNPS,'q',c(0.5,0.5))
a.zygosity <- unlist(lapply(strsplit(colnames(A.SNPS$SNPS),"\\."),function(x){x[3]}))
a.colors <- as.matrix(a.zygosity)
a.colors <- replace(a.colors, a.colors =="A",'steelblue4')
a.colors <- replace(a.colors, a.colors =="a",'orangered')
res<-tepPLSCA(A.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- a.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Multiplicative -- emphasis on the minor allele
M.SNPS <- SNP.model.maker(SNPS,'q',c(0.25,0.75))
m.zygosity <- unlist(lapply(strsplit(colnames(M.SNPS$SNPS),"\\."),function(x){x[3]}))
m.colors <- as.matrix(m.zygosity)
m.colors <- replace(m.colors, m.colors =="A",'steelblue4')
m.colors <- replace(m.colors, m.colors =="a",'orangered')
res<-tepPLSCA(M.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- m.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Multiplicative 2 -- strong emphasis on the minor allele
M2.SNPS <- SNP.model.maker(SNPS,'q',c(0.1,0.9))
m2.zygosity <- unlist(lapply(strsplit(colnames(M2.SNPS$SNPS),"\\."),function(x){x[3]}))
m2.colors <- as.matrix(m2.zygosity)
m2.colors <- replace(m2.colors, m2.colors =="A",'steelblue4')
m2.colors <- replace(m2.colors, m2.colors =="a",'orangered')
res<-tepPLSCA(M2.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- m2.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)



#Multiplicative 3 -- strong emphasis on the major allele
M3.SNPS <- SNP.model.maker(SNPS,'q',c(0.9,0.1))
m3.zygosity <- unlist(lapply(strsplit(colnames(M3.SNPS$SNPS),"\\."),function(x){x[3]}))
m3.colors <- as.matrix(m3.zygosity)
m3.colors <- replace(m3.colors, m3.colors =="A",'steelblue4')
m3.colors <- replace(m3.colors, m3.colors =="a",'orangered')
res<-tepPLSCA(M3.SNPS$SNPS,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- m3.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)