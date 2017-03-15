\name{calculateConstraints}
\alias{calculateConstraints}
\title{
calculateConstraints
}
\description{
Calculates constraints for plotting data..
}
\usage{
calculateConstraints(results,x_axis=1,y_axis=2,constraints=NULL)
}
\arguments{
  \item{results}{results from ExPosition (i.e., $ExPosition.Data)}
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
