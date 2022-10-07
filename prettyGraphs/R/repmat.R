#########################################################################################
# A replication of MatLab repmat function!
#
#
# Source:
# R FOR OCTAVE USERS
#    version 0.4
#    Copyright (C) 2001 Robin Hankin
# 	 http://cran.r-project.org/doc/contrib/R-and-octave.txt
#########################################################################################


#' repmat
#' 
#' A function to match repmat in Matlab (Mathworks, Natick, MA)
#' 
#' 
#' @param a Matrix or vector to repeat.
#' @param n Number of row repititions.
#' @param m Number of column repititions.
#' @note This code was created by Robin Hankin. Derek Beaton included the code
#' in prettyGraphs because it is a versatile function and used in prettyGraphs.
#' @author Robin Hankin.
#' @references For repmat see:\cr
#' http://cran.r-project.org/doc/contrib/R-and-octave.txt
#' @keywords misc
#' @export repmat
repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}
