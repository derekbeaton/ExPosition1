# A data set to illustrate
# Ranking data to be used with DistatisR.
# WinesRankingRawData
# Describes the Ranking 6 Wines Data
# Created on March 12 2018 by Herve Abdi
# as part of the DistatisR package.
# Remember to use roxygen2::roxygenize()

#' \code{WinesRankingRawData}: an example of an excel file
#' with (simulated) ranking data. Can be read with the function
#' \code{read.df.excel()}.
#'
#'  \code{WinesRankingRawData}: an example of an excel file
#' with (simulated) ranking data (6 wines ranked by 80 Assessors).
#' Can be read by
#' \code{read.df.excel}.
#'
#' @details
#' In this example of a "ranking task," 
#' 80 (simulated or fictitious) 
#' assessors ranked 6 red wines from Burgundy (France). 
#' The assessor first chooses
#' the most relevant dimension for these wines and then
#' positions the wines on a scale from 1 to 9 for this dimension.
#' The names of the assessors is composed of 4 characters
#' of the general composition {w/m}{a/f}{01 : 80}.
#' The assessors were 40 men and 40 women (first character w/n)
#' and 40 American or 40 French (second character a/f).
#' The red wines that were tasted are Irancy, Saint-Brie,
#' Beaune, Nuits (Cote de Nuits), Beaujolais, and Beaujolais-Nouveau.
#' Irancy & Saint-Brie are near the cities of Auxerre and Chablis,
#' Beaune & Cote de Nuits are from central Burgundy, and
#' Beaujolais and Beaujolais Nouveau are from the south of Burgundy;
#' Beaujolais Nouveau is a young wine (a primeur) 
#' released in November of its year, 
#' after only a few weeks of fermentation.
#' 
#' 
#' @section Availability:
#' To fetch this dataset use \code{system.file()}
#' (see example below).
#' @name WinesRankingRawData
#' @section FileName: WinesRankingRawData.xlsx
#' @author Herve Abdi
#' @examples
#' path2file <- system.file("extdata",
#'            "WinesRankingRawData.xlsx", package = 'DistatisR')
#' ranking6Wines <- read.df.excel(path = path2file, sheet = 'Ranking')
NULL

