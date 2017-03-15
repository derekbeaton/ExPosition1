####This script shows how to use a variety of genetic inheritance models in one analysis.

rm(list=ls())
gc()

source('LibsFuncs.R')

load('../Data/SNPS.rda') 	##SNPS data
load('../Data/TRAITS.rda')	##TRAITS data
load('../Data/DES.rda')		##Participant design matrix -- indicates e.g., group relationship


cat.TRAITS <- makeNominalData(TRAITS)
trait.names <- unlist(lapply(strsplit(colnames(cat.TRAITS),"\\."),function(x){x[3]}))
trait.colors <- as.matrix(trait.names)
trait.colors <- replace(trait.colors,trait.colors=="HI",'firebrick3')
trait.colors <- replace(trait.colors,trait.colors=="MID",'goldenrod')
trait.colors <- replace(trait.colors,trait.colors=="LOW",'olivedrab4')


CD.SNPS <- SNP.model.maker(SNPS[,1:4],'c')
D.SNPS <- SNP.model.maker(SNPS[,5:6],'d')
A.SNPS <- SNP.model.maker(SNPS[,7:9],'q',c(0.5,0.5))

##Now a mixture of Co-Dominant (SNPs 1-4), Dominant (SNPs 5-6), and Additive (SNPs 7-9) 
SNP.DAT <- cbind(CD.SNPS$SNPS,D.SNPS$SNPS,A.SNPS$SNPS)
##quick check:
rowSums(SNP.DAT) ##all equal 9, which is the total number of SNPs. 


zygosity <- unlist(lapply(strsplit(colnames(SNP.DAT),"\\."),function(x){x[3]}))
z.colors <- as.matrix(zygosity)
z.colors <- replace(z.colors, z.colors =="AA",'steelblue4')
z.colors <- replace(z.colors, z.colors =="Aa",'mediumorchid4')
z.colors <- replace(z.colors, z.colors =="aa",'orange')
z.colors <- replace(z.colors, z.colors =="ND",'steelblue2')
z.colors <- replace(z.colors, z.colors =="D",'orangered3')
z.colors <- replace(z.colors, z.colors =="A",'blue')
z.colors <- replace(z.colors, z.colors =="a",'red')

part.colors <- createColorVectorsByDesign(DES)

##Mixed inheritance models treated in a fully categorical way.
res<-tepPLSCA(SNP.DAT,cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- z.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)
