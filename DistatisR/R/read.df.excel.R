# functions in this file
# read.df.excel
# print.distatis.data


#*********************************************************************
# Read data from the sheet
# a function for read.excel
#' \code{read.df.excel} reads \code{distatis} formated
#'  ranking or sorting data from an excel file.
#'
#' \code{read.df.excel} reads \code{distatis} formated
#'  ranking or sorting data from an excel file.
#'
#'  @details 
#'  The data are read from an excel file in which the rows
#'  are the Products to evaluate and the columns are the Assessors
#'  (e.g., Judges, Participants, Subjects, Evaluators).
#'  Depending upon the type of data, the numbers represent
#'  a partition, a rank, or a score.
#'  These data are used as input of
#'  \code{DistanceFromSort} or
#'  \code{DistanceFromRank}.
#'  A contingency table for the vocabulary can also be read
#'  in a different sheet. \code{read.df.excel} is a (small) shell
#'  on top of \code{readxl::read_excel}, note however that
#'  whereas \code{readxl::read_excel} returns a \code{tibble},
#'  \code{read.df.excel} returns a list with one or two
#'  (depending upon the options) \emph{dataframe(s)}.
#'
#' @param path the name of the \code{.xlsx} file (including
#'  the path to the directory if needed, and
#'  the \code{.xlsx} extension). No default.
#' @param sheet the name of the sheet where the
#'  (e.g., Sorting or Ranking) data are stored. No default.
#' @param col_names (default \code{TRUE})
#'   parameter \code{col.names} from
#'   \code{readxl::read_excel}:
#'   "\code{TRUE} to use the first row as column names,
#'   \code{FALSE} to get default names, or a character vector giving
#'   a name for each column."
#' @param voc.sheet If not \code{NULL}
#'  (default) gives the name of the sheet where an optional
#'  contingency table  (products by names) could be stored.
#'  Needs to have the same row names as the sorting/ranking
#'  data frame (df.data) to be useful (but he program does not check).
#'  
#'  @return 
#'  a list with one data frame  `$df.data` (contains the data) 
#'  when `voc.sheet = NULL`
#'  or if not: two data frames 
#'   `$df.data` (contains the data) and 
#'   `$df.voc` (contains the vocabulary).
#' @importFrom readxl read_excel
#' @author Herve Abdi
#' @export

read.df.excel <- function(path, sheet, col_names = TRUE,
                          voc.sheet = NULL
                           ){
  le.df <- as.data.frame(readxl::read_excel(path  = path,
                                    sheet = sheet,
                                    col_names = col_names))
  le.df.data <- le.df[,-1]
  row.names(le.df.data) <- le.df[,1]
  return.list <-  structure(list(df.data = le.df.data),
                             class = "distatis.data")
  if (!is.null(voc.sheet)){
     le.df <- as.data.frame(readxl::read_excel(path  = path,
                                            sheet = voc.sheet,
                                            col_names = col_names))
     le.df.voc <- le.df[,-1]
     row.names(le.df.voc) <- le.df[,1]
     return.list$df.voc   <- le.df.voc
      }
  return(return.list)
}
#_____________________________________________________________________

# ********************************************************************
# ********************************************************************
#' Change the print function for objects of 
#' class \code{distatis.data} output of\code{read.df.excel}
#'
#'  Change the print function for bootRatios
#'
#' @param x a list: output of \code{distatis.data};
#' class \code{distatis.data}.
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @keywords internal
#' @export
print.distatis.data <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Data Frame(s) for distatis (e.g., Sorting, Ranking, or Projective Mapping)\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$df.data ", "A data frame for distatis (e.g. Sorting, Ranking, Mapping) ")
  cat("\n$df.voc  ", "A data frame for the vocabulary contingency table or raw data.frame")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootRatios
#_____________________________________________________________________


