rm(list=ls())
gc()

source('LibsFuncs.R')

load('../Data/SNPS.rda') 	##SNPS data
load('../Data/TRAITS.rda')	##TRAITS data
load('../Data/CONTINUOUS.rda')	##CONTINUOUS data
load('../Data/DES.rda')		##Participant design matrix -- indicates e.g., group relationship



	## with the Escofier recoding transform:
mimoPLS <- tepPLSCA(SNPS,escofier.transform(CONTINUOUS),T,F,DESIGN=DES,make_design_nominal=F,graphs=F)
	## for this type of PLS only interpret one side of the Escofier values, e.g., ".+" as they are redundant.

## Examples of inference tests below. Only permutation and bootstrap provided.

	### Permutation: 100 iterations only. Increase as required.
perm.iters <- 100
perm.eigs <- matrix(NA,perm.iters,length(mimoPLS$TExPosition.Data$eigs))
for(i in 1:perm.iters){
	
	perm.sample <- sample(nrow(SNPS),nrow(SNPS),F)
	mimoPLS.perm <- tepPLSCA(SNPS[perm.sample,],escofier.transform(CONTINUOUS),T,F,DESIGN=DES,make_design_nominal=F,graph=F)
	perm.eigs[i,] <- mimoPLS.perm$TExPosition.Data$eigs
		
}
original.eigs.matrix <- matrix(mimoPLS$TExPosition.Data$eigs,perm.iters,length(mimoPLS$TExPosition.Data$eigs),byrow=T)

	### test each component:
component.p.values <- pmax(1-(colSums(original.eigs.matrix > perm.eigs) / perm.iters), 1/perm.iters)
	### omnibus test
omnibus.p.value <- max(1-(sum(sum(mimoPLS$TExPosition.Data$eigs) > rowSums(perm.eigs))  / perm.iters), 1/perm.iters)


	### Bootstrap: 100 iterations only. Increase as required.
#### NOTE: Bootstrap works the same here, but the bootstrap ratio computation works differently as the Escofier coding causes an imbalance around 0. The proper BSRs are from the difference between "=" and "-" over resampling

nom.SNPS <- makeNominalData(SNPS)
boot.iters <- 100
fi.boot <- array(NA,dim=c(nrow(mimoPLS$TExPosition.Data$fi),length(mimoPLS$TExPosition.Data$eigs),boot.iters))
	rownames(fi.boot) <- rownames(mimoPLS$TExPosition.Data$fi)
fj.boot <- array(NA,dim=c(nrow(mimoPLS$TExPosition.Data$fj),length(mimoPLS$TExPosition.Data$eigs),boot.iters))
	rownames(fj.boot) <- rownames(mimoPLS$TExPosition.Data$fj)
for(i in 1:boot.iters){
	
	boot.sample <- boot.samples(SNPS,DES,T)	## this will stratify resampling to account for the DESIGN matrix -- strongly recommended in cases with a design.
	boot.nom.SNPS <- nom.SNPS[boot.sample,]
	boot.ESCOFIER.CONTINUOUS <- escofier.transform(CONTINUOUS[boot.sample,])
	boot.R <- t(boot.nom.SNPS) %*% boot.ESCOFIER.CONTINUOUS
	fi.boot[,,i] <- (rowNorms(boot.R,type="ca") %*% mimoPLS$TExPosition.Data$fj %*% diag(1/mimoPLS$TExPosition.Data$pdq$Dv))
	fi.boot[is.na(fi.boot)] <- 0
	fj.boot[,,i] <- rowNorms(t(boot.R),type="ca") %*% mimoPLS$TExPosition.Data$fi %*% diag(1/mimoPLS$TExPosition.Data$pdq$Dv)
	fj.boot[is.na(fj.boot)] <- 0	
	
}

fi.bsrs <- boot.ratio.test(fi.boot)
fj.bsrs <- boot.ratio.test(fj.boot)	## stability of each point; but because of dual coding this is incorrect.
fj.bsrs_proper <- boot.ratio.test((fj.boot[31:60,,] - fj.boot[1:30,,])) ## stability of the *difference*
	## this is more correct as we are testing the stability of a single variable here, and we are doing so through the difference in their component scores. 

# see e.g., :
fj.bsrs$boot.ratios[1:5,1:2]
fj.bsrs$boot.ratios[31:35,1:2]
fj.bsrs_proper$boot.ratios[1:5,1:2]	
	## tests.
# fj.bsrs$sig.boot.ratios[1:30,1:2] - fj.bsrs$sig.boot.ratios[31:60,1:2]
# fj.bsrs$sig.boot.ratios[1:30,1:2] - fj.bsrs_proper$sig.boot.ratios[,1:2]
# fj.bsrs$sig.boot.ratios[31:60,1:2] - fj.bsrs_proper$sig.boot.ratios[,1:2]
