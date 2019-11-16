\name{snps.druguse}

\alias{snps.druguse}

\docType{data}

\title{Small data set for Partial Least Squares-Correspondence Analysis}

\description{
The data come from a larger study on marijuauna dependent individuals (see Filbey et al., 2009) and are illustrated in Beaton et al., 2013. \cr
The data contain 2 genetic markers and 3 additional drug use questions from 50 marijuauna dependent individuals.
}

\details{
In snps.druguse$DATA1:\cr
	e - Stands for ecstacy use. Responses are yes or no.
	cc - Stands for crack/cocaine use. Responses are yes or no.
	cm - Stands for crystal meth use. Responses are yes or no.
\cr	In snps.druguse$DATA2:\cr
	COMT - Stands for the COMT gene. Alleles are AA, AG, or GG. Some values are NA.
	FAAH - Stands for FAAH gene. Alleles are AA, CA, CC. Some values are NA.
}

\usage{data(snps.druguse)}

\format{
snps.druguse$DATA1: Fifty marijuana dependent participants indicated which, if any, other drugs they have ever used.\cr
snps.druguse$DATA2: Fifty marijuana dependent participants were genotyped for the COMT and FAAH genes.
}

\references{
Filbey, F. M., Schacht, J. P., Myers, U. S., Chavez, R. S., & Hutchison, K. E. (2009). Marijuana craving in the brain. Proceedings of the National Academy of Sciences, 106(31), 13016 -- 13021. 

Beaton D., Filbey F. M., Abdi H. (2013, in press). Integrating Partial Least Squares Correlation and Correspondence Analysis for Nominal Data. In Abdi H, Chin W, Esposito-Vinzi V, Russolillo G, Trinchera L. \emph{Proceedings in Mathematics and Statistics (Vol. 56): New Perspectives in Partial Least Squares and Related Methods}. New York, NY: Springer-Verlag. 

}


\keyword{datasets}
