# This file
# Describes the orange dataset
# Created on March 12 2018 by Herve Abdi
# part of the DistatisR package.
#

#' \code{OrangeJuiceSortingRawData}: an example of an excel file
#' with Sorting data and vocabulary. This excel file can be read by
#' \code{read.df.excel}.
#'
#'  \code{OrangeJuiceSorting}: an example of an excel file
#' with sorting data (10 Orange Juices sorted by 44 Assessors)
#' and an an associated
#' vocabulary contingency table
#' (10 Orange Juices by 23 descriptors).
#' Can be read by
#' \code{read.df.excel}.
#'
#' @details
#' In this example of a "sorting task" with vocabulary,
#' 44 assessors sorted 10 orange juices and freely described
#' each group of juices with a few words.
#' The data from the sorting task are in the sheet "Sorting"
#' and the contingency table
#' (10 Orange Juices by 23 descriptors) is in the sheet
#' "Vocabulary".
#' To fetch this dataset use \code{system.file()}
#' (see example below).
#' @name OrangeJuiceSortingRawData
#' @section FileName: OrangeJuiceSortingRawData.xlsx
#' @author Herve Abdi
#' @examples
#' path2file <- system.file("extdata",
#'            "OrangeJuiceSortingRawData.xlsx", package = 'DistatisR')
#' OrangeDataSort <- read.df.excel(path = path2file,
#'                            sheet = 'Sorting',
#'                        voc.sheet = 'Vocabulary')
NULL

