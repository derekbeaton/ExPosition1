library(TExPosition)


##Input required: SNPS matrix where participants are on the rows and SNPs on the columns.
	##Here, SNPS must be in [0, 1, 2] allelic counting scheme, where 0 is the major homozygote and 2 is the minor homozygote.
	
##Outputs are based on "model" flag:
	###C or c: co-dominant
	###D or d: dominant
	###R or r: recessive
	###O or o: over-dominant
	###Q or q: quantitative; requires use of quant.vals (default = 0.5/0.5 emphasis of each allele)

SNP.model.maker <- function(SNPS,model='C',quant.vals=c(0.5,0.5)){
	
	if(sum(quant.vals)!=1){
		stop("sum of quant.vals must equal 1.")
	}
	
	SNPS.out <- SNPS
	class(SNPS.out) <- "character"
	if(tolower(model)=='c'){
		SNPS.out <- replace(SNPS.out, SNPS.out=='0','AA')
		SNPS.out <- replace(SNPS.out, SNPS.out=='1','Aa')
		SNPS.out <- replace(SNPS.out, SNPS.out=='2','aa')
		checks <- column.checker(SNPS.out)
		SNPS.keep <- makeNominalData(checks$KEEP)			
	}

	if(tolower(model)=='d'){
		SNPS.out <- replace(SNPS.out, SNPS.out=='0','ND')
		SNPS.out <- replace(SNPS.out, SNPS.out=='1','D')
		SNPS.out <- replace(SNPS.out, SNPS.out=='2','D')
		checks <- column.checker(SNPS.out)
		SNPS.keep <- makeNominalData(checks$KEEP)					
	}

	if(tolower(model)=='r'){
		SNPS.out <- replace(SNPS.out, SNPS.out=='0','NR')
		SNPS.out <- replace(SNPS.out, SNPS.out=='1','NR')
		SNPS.out <- replace(SNPS.out, SNPS.out=='2','R')
		checks <- column.checker(SNPS.out)
		SNPS.keep <- makeNominalData(checks$KEEP)					
	}
	
	if(tolower(model)=='o'){
		SNPS.out <- replace(SNPS.out, SNPS.out=='0','NOD')
		SNPS.out <- replace(SNPS.out, SNPS.out=='1','OD')
		SNPS.out <- replace(SNPS.out, SNPS.out=='2','NOD')
		checks <- column.checker(SNPS.out)
		SNPS.keep <- makeNominalData(checks$KEEP)
	}
	
	if(tolower(model)=='q'){
		SNPS.out <- replace(SNPS.out, SNPS.out=='0','AA')
		SNPS.out <- replace(SNPS.out, SNPS.out=='1','Aa')
		SNPS.out <- replace(SNPS.out, SNPS.out=='2','aa')
		checks <- column.checker(SNPS.out)
		SNPS.keep <- two.column.maker(checks$KEEP,quant.vals)
	}
	
	return(list(SNPS=SNPS.keep,DROPPED=checks$DROPPED))
}


two.column.maker <- function(SNPS.in,quant.vals=c(0.5,0.5)){
	
	SNPS.out <- matrix(0,nrow(SNPS.in),ncol(SNPS.in)*2)
	colnames(SNPS.out) <- unlist(lapply(colnames(SNPS.in),function(x){paste(x,c("A","a"),sep=".")}))
	col.map <- seq(1,ncol(SNPS.out),2)
	for(i in 1:ncol(SNPS.in)){
		
		SNPS.out[which(SNPS.in[,i]=='AA'),col.map[i]] <- 1
		SNPS.out[which(SNPS.in[,i]=='aa'),col.map[i]+1] <- 1	
		SNPS.out[which(SNPS.in[,i]=='Aa'),c(col.map[i],col.map[i]+1)] <- quant.vals

	}
	rownames(SNPS.out) <- rownames(SNPS.in)
	return(SNPS.out)
		
}


column.checker <- function(DAT){
	uniq.check <- apply(DAT,2,function(x){length(unique(x))})
	drop.cols <- which(uniq.check == 1)
	keep.cols <- which(uniq.check > 1)	
	
	KEEP.DATA <- as.matrix(DAT[,keep.cols])
	DROP.DATA <- as.matrix(DAT[,drop.cols])
	
	rownames(KEEP.DATA) <- rownames(DAT)
	rownames(DROP.DATA) <- rownames(DAT)	
	
	if(length(drop.cols) >= 1){
		warning("Some columns dropped. See $DROPPED")
	}
	
	return(list(KEEP=KEEP.DATA,DROPPED=DROP.DATA))
}

escofier.transform <- function(DAT,center=T,scale=T){
	
	scaled.DAT <- expo.scale(DAT,center=center,scale=scale)
	escofier.DAT <- cbind((1-scaled.DAT)/2,(1+ scaled.DAT)/2)
	colnames(escofier.DAT) <- c(paste0(colnames(scaled.DAT),"-"),paste0(colnames(scaled.DAT),"+"))
	return(escofier.DAT)
	
}
