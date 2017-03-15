\name{continueResampling}
\alias{continueResampling}
\title{
A stopping mechanism if resampling will take too long.
}
\description{
This function asks the user if they want to continue with resampling if the total time for resampling takes more than 1 minute. It also provides an estimate of how long resampling takes. This function is required for \code{InPosition} and \code{TInPosition} and we do not recommend others use it.
}
\usage{
continueResampling(cycle.time)
}
\arguments{
  \item{cycle.time}{
Is the subtraction of two calls to \code{proc.time}.
}
}
\author{
Derek Beaton
}
\note{
If computation time is expected to take more than 1 minute and \code{interactive()} is TRUE, this asks the user if they would like to continue. If 'Y', looping continues. If 'N', it stops. 

If computation time is expected to takre more than 1 minute and \code{interactive()} is FALSE, the function will proceed as is and perform inference tests. 

A progress bar is provided so the user can see how long the tests will take.

See inference battery functions for details.
}