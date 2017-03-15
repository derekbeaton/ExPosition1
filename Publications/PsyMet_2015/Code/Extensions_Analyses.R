####This script shows how to perform each analysis as outlined in the paper.

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


CD.SNPS <- SNP.model.maker(SNPS,'c')
cd.zygosity <- unlist(lapply(strsplit(colnames(CD.SNPS$SNPS),"\\."),function(x){x[3]}))
cd.colors <- as.matrix(cd.zygosity)
cd.colors <- replace(cd.colors, cd.colors =="AA",'steelblue4')
cd.colors <- replace(cd.colors, cd.colors =="Aa",'mediumorchid4')
cd.colors <- replace(cd.colors, cd.colors =="aa",'orangered')

part.colors <- createColorVectorsByDesign(DES)

##Simple PLSCA
res<-tepPLSCA(CD.SNPS$SNPS, cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- cd.colors
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)


##Multiblock PLSCA -- where, e.g., there are 3 trait blocks
	##This informs us if there is some structure in a set that can explain participant deviations.
	###TRAITS 1-4 = BLOCK 1
	###TRAITS 5-6 = BLOCK 1
	###TRAITS 7-12 = BLOCK 3
	####TO NOTE: There is no need to re-run the analysis. Multiblock is a projection of a known a priori structure.	
	block1 <- 1:12
	block2 <- 13:18	
	block3 <- 18:36
		
	BLOCK1.LVs <- cat.TRAITS[,block1] %*% diag(res$TExPosition.Data$W2[block1]) %*% res$TExPosition.Data$pdq$q[block1,]
	BLOCK2.LVs <- cat.TRAITS[,block2] %*% diag(res$TExPosition.Data$W2[block2]) %*% res$TExPosition.Data$pdq$q[block2,]
	BLOCK3.LVs <- cat.TRAITS[,block3] %*% diag(res$TExPosition.Data$W2[block3]) %*% res$TExPosition.Data$pdq$q[block3,]
	
	prettyPlot(BLOCK1.LVs, col=part.colors$oc)
	prettyPlot(BLOCK2.LVs, col=part.colors$oc)	
	prettyPlot(BLOCK3.LVs, col=part.colors$oc)	


##mean-centered PLSCA (a.k.a. discriminant PLSCA, discriminant correspondence analysis (CA), between groups (CA))
##To note, there are two ways to do this (theoretically and via code)

##1 as a PLSCA
res<-tepPLSCA(CD.SNPS$SNPS,DES,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- cd.colors
res$Plotting.Data$fj.col <- part.colors$gc
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)


##2 as a Discriminant CA
res<-tepDICA(CD.SNPS$SNPS,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fi.col <- part.colors$gc
res$Plotting.Data$fj.col <- cd.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvPlots=FALSE,contributionPlots=FALSE)


##Seed PLSCA
SEED.SNPS <- CD.SNPS$SNPS[,c(paste0("SNP.6.",c('AA','aa')),paste0("SNP.2.",c('AA','Aa','aa')))]
OTHER.SNPS <- CD.SNPS$SNPS[,which(!(colnames(CD.SNPS$SNPS) %in% colnames(SEED.SNPS)))]
res<-tepPLSCA(OTHER.SNPS, SEED.SNPS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE,symmetric=FALSE)
tepGraphs(res,lvPlots=FALSE,contributionPlots=FALSE)



##Mixed-modality (i.e., heterogeneous data)
####TO NOTE: this version will differ slightly from the example provided in the paper. The pipeline and processes are the same, however, we will use this example to illustrate mixed data wherein the traits are categorical, but, the SNPS are in standard [0, 1, 2]

##Here, we provide two examples of this: (1) SNPs & Traits, and (2) SNPs & group (i.e., discriminant)

quant.snps <- SNPS
class(quant.snps) <- "numeric"
center.normed.snps <- expo.scale(quant.snps)
escofier.transform <- cbind((1-center.normed.snps)/2,(1+center.normed.snps)/2)


##Mixed-modality simple PLSCA w/ strictly quantitative SNPs.
res<-tepPLSCA(escofier.transform, cat.TRAITS,FALSE,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fj.col <- trait.colors
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvAgainst=FALSE,contributionPlots=FALSE)


##Mixed-modality mean-centered PLSCA w/strictly quantitative SNPs.
res<-tepDICA(escofier.transform,FALSE,DESIGN=DES,FALSE,graphs=FALSE)
res$Plotting.Data$fii.col <- part.colors$oc
tepGraphs(res,lvPlots=FALSE,contributionPlots=FALSE)