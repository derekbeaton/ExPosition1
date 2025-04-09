#' caChiTest: correspondence analysis tests without resampling.
#' 
#' caChiTest performs 3 sets of chi-square tests along the lines of Lebart's
#' v-tests. These tests are designed to be conservative estimates of chi-square
#' tests on contingency data. The tests treat this data in a standard
#' chi-square framework, but are helpful to understand correspondence analysis
#' data when permutation and bootstrap become unfeasible.
#' 
#' 
#' @param DATA Data as would be entered for Correspondence Analysis (see
#' \code{link{epCA}})
#' @param res Results from correspondence analysis (e.g., output from
#' \code{link{epCA}}).
#' @param critical.value numeric. A value, analogous to a z- or t-score to be
#' used to determine significance (via bootstrap ratio).
#' @return a list with the following values:\cr \item{j.sig.vals}{boolean
#' matrix. Identifies which column items are significant (based on
#' \code{critical.value}).} \item{j.signed.vals}{chi-square values associated
#' to column items, multiplied by the sign of their component scores ($fj).}
#' \item{j.p.vals}{p values associated to column items in a chi-square test.}
#' \item{i.sig.vals}{boolean matrix. Identifies which row items are significant
#' (based on \code{critical.value}).} \item{i.signed.vals}{chi-square values
#' associated to row items, multiplied by the sign of their component scores
#' ($fi).} \item{i.p.vals}{p values associated to row items in a chi-square
#' test.} \item{omni.val}{chi-square value associated to the table.}
#' \item{omni.p}{p value associated to a chi-square tests of the table.}
#' @author Derek Beaton
#' @seealso \code{\link{epCA.inference.battery}}
#' @keywords inference correspondence analysis misc multivariate
caChiTest <- function(DATA,res,critical.value=2){
	
	##cols
	j.vals.to.test <- res$ExPosition.Data$cj * matrix(res$ExPosition.Data$eigs,nrow(res$ExPosition.Data$cj),ncol(res$ExPosition.Data$cj),byrow=TRUE) * sum(DATA)
	j.df <- nrow(DATA) - 1
	#p.vals
	j.p.vals <- 1-pchisq(j.vals.to.test,j.df)
	j.signed.vals <- sign(res$ExPosition.Data$fj) * sqrt(j.vals.to.test)
	rownames(j.signed.vals) <- colnames(DATA)
	j.significant.vals <- j.p.vals < (2*(1-pnorm(critical.value)))
	rownames(j.significant.vals) <- rownames(j.signed.vals)
	
	##rows
	i.vals.to.test <- res$ExPosition.Data$ci * matrix(res$ExPosition.Data$eigs,nrow(res$ExPosition.Data$ci),ncol(res$ExPosition.Data$ci),byrow=TRUE) * sum(DATA)
	i.df <- ncol(DATA) - 1
	#p.vals
	i.p.vals <- 1-pchisq(i.vals.to.test,i.df)
	i.signed.vals <- sign(res$ExPosition.Data$fi) * sqrt(i.vals.to.test)
	rownames(i.signed.vals) <- colnames(DATA)
	i.significant.vals <- i.p.vals < (2*(1-pnorm(critical.value)))
	rownames(i.significant.vals) <- rownames(i.signed.vals)	
	
	##omni
	omni.val <- sum(res$ExPosition.Data$eigs * sum(DATA))
	omni.df <- (nrow(DATA)-1) * (ncol(DATA)-1)
	omni.p <- 1-pchisq(omni.val,omni.df)
	
	return(list(j.sig.vals=j.significant.vals, j.signed.vals=j.signed.vals, j.p.vals=j.p.vals, i.sig.vals=i.significant.vals, i.signed.vals=i.signed.vals, i.p.vals=i.p.vals, omni.val=omni.val,omni.p=omni.p))
	
}
