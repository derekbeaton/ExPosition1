# File for funciton vocabulary2CT
# A function to create a continency table from
# sorting, Projecive mapping etc. taks
# current version is March 18, 2018.
# Herve Abdi

#---------------------------------------------------------------------
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(vocabulary2CT)
#
#---------------------------------------------------------------------

#' @title Transforms a data.frame of products by vocabulary 
#' of assessors into
#' a  products by words (from vocabulary) contingency table.
#' 
#' @description \code{vocabulary2CT}
#'  Transforms a data.frame of products by vocabulary 
#' of assessors into 1)
#' a cube of 0/1 contingency tables (one per assessor); and 2)
#' a  products by words (from vocabulary) contingency table.
#' In this contingency table, the number at the intersection
#'  of a row (product) and a column (word) is the number
#'  of assessors who used this word to describe that product.
#'  
#'  @details the cube of 0/1 contingency tables
#'  (i.e., \code{cubeOfVocabulary}   can also be
#'  analyzed with the package \code{PTCA4CATA} as a
#'  pseudo \emph{Check All That Apply}  (CATA) data set.
#' 
#' @param df.voc a data frame with the vocabulary. In this 
#' data.frame 
#' each element 
#' stores
#' the words used by one assessor to describe a product 
#' (words are separated with spaces);
#' @return a list with 1) \code{cubeOfVocabulary}:
#' a 0/1 array of dimension
#' products by words (from the vocabulary) by assessors
#' where each "products by vocabulary" slice gives the vocabulary
#' chosen by the assessor to describe the products; and
#' 2) 
#' \code{CT.vocabulary} a matrix storing the 
#' products by words contingency table.
#' @examples 
#' # Get the BeersProjectiveMapping example
#' data("BeersProjectiveMapping")
#' aContingenyTable <- vocabulary2CT(BeersProjectiveMapping$Vocabulary)
#' 
#' @seealso 
#'  \code{\link[tidytext]{unnest_tokens}}
#'  \code{\link[dplyr]{count}}
#'  \code{\link{BeersProjectiveMapping}}
#' @rdname vocabulary2CT
#' @author Herve Abdi
#' @export 
#' @import dplyr
#' @import tidytext
#   import magrittr
#  @import tibble
#---------------------------------------------------------------------
# A function to transform the vocabulary into a contingency table
#
# needs tidytext and dplyr
vocabulary2CT <- function(df.voc){
  # parameters needed: df.voc
  # Transform the df.voc into a list
  Jk <- list()
  nK <- ncol(df.voc)
  for (k in 1:nK){
    Jk[[k]] <- df.voc[,k]
  }
  names(Jk) <-  colnames(df.voc) # paste0('J_',1:7)
  #-------------------------------------------------------------------
  nI   <- length(Jk[[1]])
  nK   <- length(Jk)

  if (is.null(row.names(df.voc) )) {
    namesOfProducts <- paste0('P',1:nI)
  } else {
    namesOfProducts  <- row.names(df.voc)  
  }
  J_p <-  unlist(Jk) # In order to get the vocabulary
  df4voc.tmp <- data_frame(line = 1:(nI*nK), text = as.character(J_p))
  words = NULL # these two lines are here to appease the parser that
  text = NULL  # thinks that "words" and "text" are undefined 
  # when used as parameters below
  tmp1 <-  unnest_tokens(df4voc.tmp, "words", "text",format = 'text')
  frequency <- count(tmp1, words, sort = FALSE)
  # for some strange reason the pipe creates problems in a function
  # but works fine in a script!
  # So we are back to explicit temporary assigments
  # frequency <- df4voc.tmp %>% 
  #   tidytext::unnest_tokens("words", "text", format = 'text') %>% 
  #   count(words, sort = FALSE)
  # 
  Vocabulary <- frequency$words
  nVoc <- length(Vocabulary)
  namesOfJudges <- colnames(df.voc)
  if (!is.null(namesOfJudges)) namesOfJudges = names(Jk) 
  # Test twice in case the last has no names
  if (is.null(namesOfJudges)){namesOfJudges <- paste0('J',1:nK)}
  cubeOfVocabulary <- array(0, dim = c(nI, nVoc, nK) )
  dimnames(cubeOfVocabulary)[[1]] <- namesOfProducts
  dimnames(cubeOfVocabulary)[[2]] <- Vocabulary
  dimnames(cubeOfVocabulary)[[3]] <- namesOfJudges
  #print('before loop')
  for (k in 1:nK){
    for (i in  1:nI){
      wordsInVoc <- Vocabulary %in% unnest_tokens(
                   data_frame(line = 1, 
                   text =  as.character(Jk[[k]][[i]])),words,text)$words
      cubeOfVocabulary[i,,k] <- wordsInVoc
    } # end of loop in i
  } # end of loop in k
  
  CT.vocabulary <- apply(cubeOfVocabulary,c(1,2),sum)
  return.list <- structure(list(
    cubeOfVocabulary = cubeOfVocabulary,
    CT.vocabulary = CT.vocabulary),
    class = "voc4distatis")
  return(return.list)
}
#---------------------------------------------------------------------
#====================================================================
# Print function for class voc4distatis
# *******************************************************************
#' Change the print function for voc4distatis
#'
#'  Change the print function for voc4distatis class
#'  objects
#'  (e.g.,output of Rv).
#'
#' @param x a list: output of \code{vocabulary2CT}
#' @param ... everything else for the function
#' @author Herve Abdi
#' @keywords internal
#' @export
print.voc4distatis <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nA Cube of K 'Vocabulary 0/1 Contingency Tables' & 1 'Grand Contingency Table' \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$cubeOfVocabulary ", "A brick of K '0/1 contingency tables' (one per assessor)")
  cat("\n$CT.vocabulary    ", "A 'products by words' contingency table")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.voc4distatis
#------------------------------------------------------------------------------