\name{rowNorms}
\alias{rowNorms}
\title{
	Normalize the rows of a matrix.
}
\description{
This function will normalize the rows of a matrix.
}
\usage{
rowNorms(X, type = NULL, center = FALSE, scale = FALSE)
}
\arguments{
  \item{X}{Data matrix}
  \item{type}{a string. Type of normalization to perform. Options are \code{hellinger}, \code{ca}, \code{z}, \code{other}}
  \item{center}{optional. A vector to center the columns of X.}
  \item{scale}{optional. A vector to scale the values of X.}
}
\details{
rowNorms works like \code{link{expo.scale}}, but for rows. Hellinger row norm via \code{hellinger}, Correspondence analysis analysis row norm (row profiles) via \code{ca}, Z-score row norm via \code{z}. \code{other} passes \code{center} and \code{scale} to \code{\link{expo.scale}} and allows for optional centering and scaling parameters.
}
\value{
Returns a row normalized version of X.
}
\author{
Derek Beaton
}