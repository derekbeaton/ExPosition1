\name{mpGraphs}
\alias{mpGraphs}
\title{mpGraphs: MExPosition plotting function}
\description{MExPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}
\usage{mpGraphs(res, table, DESIGN = NULL, x_axis = 1, y_axis = 2, 
  fi.col = NULL, fj.col = NULL, table.col = NULL, col.offset = NULL, 
  constraints = NULL, xlab = NULL, ylab = NULL, main = NULL, graphs = TRUE)
}

\arguments{
  \item{res}{results from MExPosition (i.e., $mexPosition.Data)}
  \item{table}{results from mpGraphs (i.e., $Plotting.Data)}
  \item{DESIGN}{Design Matrix which differentiates the tables}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{fi.col}{Colors for the rows}
  \item{fj.col}{Colors for the columns}
  \item{table.col}{Colors for the tables}
  \item{col.offset}{Color Offset}
  \item{constraints}{Plotting Constraints}
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
  \item{graphs}{Boolean option. If TRUE (default), graphs will be plotted else, there will be graphical output}
}
\details{
mpGraphs is an interface between \code{\link{MExPosition}} and \code{\link{prettyGraphs}}. 
}
\value{
The following items are bundled inside of $Plotting.Data:\cr
\item{$fi.col}{the colors that are associated to the groups.}
\item{$fj.col}{the colors that are associated to the column items.}
}
\author{
Cherise R. Chin Fatt and Derek Beaton
}

\seealso{
 \code{\link{prettyGraphs}}
}
\keyword{multivariate}
\keyword{graphs}
\keyword{misc}
