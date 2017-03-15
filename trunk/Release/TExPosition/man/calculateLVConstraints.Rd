\name{calculateLVConstraints}
\alias{calculateLVConstraints}
\title{
calculateLVConstraints
}
\description{
Calculates constraints for plotting latent variables.
}
\usage{
calculateLVConstraints(results,x_axis=1,y_axis=2,constraints=NULL)
}
\arguments{
  \item{results}{results (with $lx and $ly) from TExPosition (i.e., $TExPosition.Data)}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{constraints}{if available, axis constraints for the plots (determines end points of the plots).}
}
\value{
Returns a list with the following items:\cr
	\item{$constraints}{axis constraints for the plots (determines end points of the plots).}
}
\author{
	Derek Beaton
}
\keyword{ misc }
