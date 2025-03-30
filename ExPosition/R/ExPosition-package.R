

#' (A truncated form of) Punctuation used by six authors (data).
#' 
#' How six authors use 3 different types of puncatuation throughout their
#' writing.
#' 
#' 
#' @name authors
#' @docType data
#' @usage data(authors)
#' @format authors$ca$data: Six authors (rows) and the frequency of three
#' puncutuations (columns). For use with \code{\link{epCA}}.\cr
#' authors$mca$data: A Burt table reformatting of the $ca$data. For use with
#' \code{\link{epMCA}}.
#' @references Brunet, E. (1989). Faut-il ponderer les donnees linguistiques.
#' \emph{CUMFID}, 16, 39-50. \cr Abdi, H., and Williams, L.J. (2010). Principal
#' component analysis. \emph{Wiley Interdisciplinary Reviews: Computational
#' Statistics}, 2, 433-459.\cr Abdi, H., and Williams, L.J. (2010).
#' Correspondence analysis. In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.):
#' \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp.
#' 267-278.
#' @keywords datasets
NULL





#' Twelve wines from 3 regions in France with 18 attributes.
#' 
#' This data should be used for discriminant analyses or analyses where the
#' \emph{group} information is important.
#' 
#' 
#' @name bada.wine
#' @docType data
#' @usage data(bada.wine)
#' @format bada.wine$data: Data matrix with twelve wines (rows) from 3 regions
#' with 18 attributes (columns).\cr bada.wine$design: Design matrix with twelve
#' wines (rows) with 3 regions (columns) to indicate group relationship of the
#' data matrix.
#' @references Abdi, H. and Williams, L.J. (2010). Barycentric discriminant
#' analysis (BADIA). In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.):
#' \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp. 64-75.
#' @keywords datasets
NULL





#' Some of authors' personal beer tasting notes.
#' 
#' Tasting notes, preferences, breweries and styles of 38 different craft beers
#' from various breweries, across various styles.
#' 
#' 
#' @name beer.tasting.notes
#' @docType data
#' @usage data(beer.tasting.notes)
#' @format beer.tasting.notes$data: Data matrix. Tasting notes (ratings) of 38
#' different beers (rows) described by 16 different flavor profiles
#' (columns).\cr beer.tasting.notes$brewery.design: Design matrix. Source
#' brewery of 38 different beers (rows) across 26 breweries (columns). \cr
#' beer.tasting.notes$style.design: Design matrix. Style of 38 different beers
#' (rows) across 20 styles (columns) (styles as listed from Beer Advocate
#' website). \cr beer.tasting.notes$sup.data: Supplementary data matrix. ABV
#' and overall preference ratings of 38 beers described by two features (ABV &
#' overall) in original value and rounded value.
#' @references http://www.beeradvocate.com
#' @source Jenny Rieck and Derek Beaton laboriously ``collected'' these data
#' for ``experimental purposes''.
#' @keywords datasets
NULL





#' Ten assessors sort eight beers into groups.
#' 
#' Ten assessors perform a free-sorting task to sort eight beers into groups.
#' 
#' 
#' @name beers2007
#' @docType data
#' @usage data(beers2007)
#' @format beer2007$data: A data matrix with 8 rows (beers) described by 10
#' assessors (columns).
#' @references Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007).
#' Analyzing assessors and products in sorting tasks: DISTATIS, theory and
#' applications. \emph{Food Quality and Preference}, 627-640.
#' @keywords datasets
NULL





#' Small data set on flavor perception and preferences for coffee.
#' 
#' One coffee from Oak Cliff roasters (Dallas, TX) was used in this experiment.
#' Honduran source with a medium roast. The coffee was brewed in two ways and
#' served in two ways (i.e., a 2x2 design). Two batches each of coffee were
#' brewed at 180 degrees (Hot) Farenheit or at room temperature (Cold). One of
#' each was served cold or heated back up to 180 degrees (Hot).
#' 
#' Flavor profiles measured: Salty, Spice Cabinet, Sweet, Bittery, and Nutty.
#' 
#' @name coffee.data
#' @docType data
#' @usage data(coffee.data)
#' @format coffee.data$preferences: Ten participants indicated if they liked a
#' particular serving or not.\cr coffee.data$ratings: Ten participants
#' indicated on a scale of 0-2 the presence of particular flavors. In an array
#' format.
#' @keywords datasets
NULL





#' Alzheimer's Patient-Spouse Dyads.
#' 
#' Conversational data from Alzheimer's Patient-Spouse Dyads.
#' 
#' 
#' @name dica.ad
#' @docType data
#' @usage data(dica.ad)
#' @format dica.ad$data: Seventeen dyads described by 58 variables.\cr
#' dica.ad$design: Seventeen dyads that belong to three groups.
#' @references Williams, L.J., Abdi, H., French, R., & Orange, J.B. (2010). A
#' tutorial on Multi-Block Discriminant Correspondence Analysis (MUDICA): A new
#' method for analyzing discourse data from clinical populations. \emph{Journal
#' of Speech Language and Hearing Research},\bold{ 53}, 1372-1393.
#' @keywords datasets
NULL





#' Twelve wines from 3 regions in France with 16 attributes.
#' 
#' This data should be used for discriminant analyses or analyses where the
#' \emph{group} information is important.
#' 
#' 
#' @name dica.wine
#' @docType data
#' @usage data(dica.wine)
#' @format dica.wine$data: Data matrix with twelve wines (rows) from 3 regions
#' with 16 attributes (columns) \emph{in disjunctive (0/1) coding}.\cr
#' dica.wine$design: Design matrix with twelve wines (rows) with 3 regions
#' (columns) to indicate group relationship of the data matrix.
#' @references Abdi, H. (2007). Discriminant correspondence analysis. In N.J.
#' Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Thousand
#' Oaks (CA): Sage. pp. 270-275.
#' @keywords datasets
NULL





#' Fisher's iris Set (for ExPosition)
#' 
#' The world famous Fisher's iris set: 150 flowers from 3 species with 4
#' attributes.
#' 
#' 
#' @name ep.iris
#' @docType data
#' @usage data(ep.iris)
#' @format ep.iris$data: Data matrix with 150 flowers (rows) from 3 species
#' with 4 attributes (columns) describing sepal and petal features.\cr
#' ep.iris$design: Design matrix with 150 flowers (rows) with 3 species
#' (columns) indicating which flower belongs to which species.
#' @source http://en.wikipedia.org/wiki/Iris_flower_data_set
#' @keywords datasets
NULL





#' ExPosition: \emph{Ex}ploratory Analysis with the Singular Value
#' Decom\emph{Position}
#' 
#' Exposition is defined as \emph{a comprehensive explanation of an idea}. With
#' ExPosition for R, a comprehensive explanation of your data will be provided
#' with minimal effort.\cr\cr The core of ExPosition is the singular value
#' decomposition (SVD; see: \code{\link{svd}}). The point of ExPosition is
#' simple: to provide the user with an overview of their data that only the SVD
#' can provide. ExPosition includes several techniques that depend on the SVD
#' (see below for examples and functions).
#' 
#' 
#' @name ExPosition-package
#' @aliases ExPosition-package ExPosition
#' @docType package
#' @author Questions, comments, compliments, and complaints go to Derek Beaton
#' \email{exposition.software@@gmail.com}.\cr\cr
#' 
#' The following people are authors or contributors to ExPosition code, data,
#' or examples:\cr Derek Beaton, Hervé Abdi, Cherise Chin-Fatt, Joseph Dunlop,
#' Jenny Rieck, Rachel Williams, Anjali Krishnan, and Francesca M. Filbey.
#' @seealso \code{\link{epPCA}}, \code{\link{epMDS}}, \code{\link{epCA}},
#' \code{\link{epMCA}}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H. and Williams, L.J. (2010). Correspondence analysis.
#' In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr Abdi, H. (2007).
#' Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr Abdi,
#' H. (2007). Metric multidimensional scaling. In N.J. Salkind (Ed.):
#' \emph{Encyclopedia of Measurement and Statistics.} Thousand Oaks (CA): Sage.
#' pp. 598-605.\cr Greenacre, M. J. (2007). Correspondence Analysis in
#' Practice. \emph{Chapman and Hall}.\cr Benzécri, J. P. (1979). Sur le calcul
#' des taux d'inertie dans l'analyse d'un questionnaire. \emph{Cahiers de
#' l'Analyse des Données}, \bold{4}, 377-378.\cr
#' @import prettyGraphs
#' @importFrom stats cor dist sd
#' @keywords package multivariate
#' @examples
#' 
#' #For more examples, see each individual function (as noted above).
#' 
NULL





#' Faces analyzed using Four Algorithms
#' 
#' Four algorithms compared using a distance matrix between six faces.
#' 
#' 
#' @name faces2005
#' @docType data
#' @usage data(faces2005)
#' @format faces2005$data: A data structure representing a distance matrix
#' (6X6) for four algorithms.
#' @references Abdi, H., & Valentin, D. (2007). DISTATIS: the analysis of
#' multiple distance matrices. \emph{Encyclopedia of Measurement and
#' Statistics}. 284-290.
#' @keywords datasets
NULL





#' How twelve French families spend their income on groceries.
#' 
#' This data should be used with \code{\link{epPCA}}
#' 
#' 
#' @name french.social
#' @docType data
#' @usage data(french.social)
#' @format french.social$data: Data matrix with twelve families (rows) with 7
#' attributes (columns) describing what they spend their income on.
#' @references Lebart, L., and Fénelon, J.P. (1975) \emph{Statistique et
#' informatique appliquées}. Paris: Dunod\cr Abdi, H., and Williams, L.J.
#' (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics}, 2, 433-459.
#' @keywords datasets
NULL





#' A collection of beer tasting notes from untrained assessors.
#' 
#' A collection of beer tasting notes of 9 beers, across 16 descriptors, from 4
#' untrained assessors.
#' 
#' 
#' @name great.beer.tasting.1
#' @docType data
#' @usage data(great.beer.tasting.1)
#' @format great.beer.tasting.1$data: Data matrix (cube). Tasting notes
#' (ratings) of 9 different beers (rows) described by 16 different flavor
#' profiles (columns) by 4 untrained assessors. Thes data contain NAs and must
#' be imputed or adjusted before an analysis is performed.\cr
#' great.beer.tasting.1$brewery.design: Design matrix. Source brewery of 9
#' different beers (rows) across 5 breweries (columns). \cr
#' great.beer.tasting.1$flavor: Design matrix. Intended prominent flavor of 9
#' different beers (rows) across 3 flavor profiles (columns).
#' @source Rachel Williams, Jenny Rieck and Derek Beaton recoded, collected
#' data and/or ``ran the experiment''.
#' @keywords datasets
NULL





#' A collection of beer tasting notes from untrained assessors.
#' 
#' A collection of beer tasting notes of 13 beers, across 15 descriptors, from
#' 9 untrained assessors.
#' 
#' 
#' @name great.beer.tasting.2
#' @docType data
#' @usage data(great.beer.tasting.2)
#' @format great.beer.tasting.2$data: Data matrix (cube). Tasting notes
#' (ratings) of 13 different beers (rows) described by 15 different flavor
#' profiles (columns) by 9 untrained assessors. All original values were on an
#' interval scale of 0-5. Any decimal values are imputed from alternate data
#' sources or additional assessors.\cr great.beer.tasting.2$brewery.design:
#' Design matrix. Source brewery of 13 different beers (rows) across 13
#' breweries (columns). \cr great.beer.tasting.2$style.design: Design matrix.
#' Style of 13 different beers (rows) across 8 styles (columns). Some complex
#' styles were truncated.
#' @source Rachel Williams, Jenny Rieck and Derek Beaton recoded, collected
#' data and/or ``ran the experiment''.
#' @keywords datasets
NULL





#' Data from 17 Alzheimer's Patient-Spouse dyads.
#' 
#' Seventeen Alzheimer's Patient-Spouse Dyads had conversations recorded and 58
#' attributes were recoded for this data. Each attribute is a frequency of
#' occurence of the item.
#' 
#' 
#' @name jlsr.2010.ad
#' @docType data
#' @usage data(jlsr.2010.ad)
#' @format jlsr.2010.ad$ca$data: Seventeen patient-spouse dyads (rows)
#' described by 58 conversation items. For use with \code{\link{epCA}} and
#' discriminant analyses.\cr jlsr.2010.ad$mca$design: A design matrix that
#' indicates which group the dyad belongs to: control (CTRL), early stage
#' Alzheimer's (EDAT) or middle stage Alzheimer's (MDAT).
#' @references Williams, L.J., Abdi, H., French, R., and Orange, J.B. (2010). A
#' tutorial on Multi-Block Discriminant Correspondence Analysis (MUDICA): A new
#' method for analyzing discourse data from clinical populations. \emph{Journal
#' of Speech Language and Hearing Research}, 53, 1372-1393.
#' @keywords datasets
NULL





#' Data of categories of images as view in an \emph{f}MRI experiment.
#' 
#' Contains 2 data sets: distance matrix of \emph{f}MRI scans of participants
#' viewing categories of items and distance matrix of the actual pixels from
#' the images in each category.
#' 
#' 
#' @name jocn.2005.fmri
#' @docType data
#' @usage data(jocn.2005.fmri)
#' @format jocn.2005.fmri$images$data: A distance matrix of 6 categories of
#' images based on a pixel analysis. \cr jocn.2005.fmri$scans$data: A distance
#' matrix of 6 categories of images based on \emph{f}MRI scans.
#' @seealso http://openfmri.org/dataset/ds000105
#' @references O'Toole, A. J., Jiang, F., Abdi, H., and Haxby, J. V. (2005).
#' Partially distributed representations of objects and faces in ventral
#' temporal cortex. \emph{Journal of Cognitive Neuroscience}, \emph{17}(4),
#' 580-590. \cr Haxby, J. V., Gobbini, M. I., Furey, M. L., Ishai, A.,
#' Schouten, J. L., and Pietrini, P. (2001). Distributed and overlapping
#' representation of faces and objects in ventral temporal cortex.
#' \emph{Science}, 293, 2425-2430.\cr
#' @keywords datasets
NULL





#' Six wines described by several assessors with qualitative attributes.
#' 
#' Six wines described by several assessors with qualitative attributes.
#' 
#' 
#' @name mca.wine
#' @docType data
#' @usage data(mca.wine)
#' @format mca.wine$data: A (categorical) data matrix with 6 wines (rows) from
#' several assessors described by 10 attributes (columns). For use with
#' \code{\link{epMCA}}.
#' @references Abdi, H., & Valentin, D. (2007). Multiple correspondence
#' analysis. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and
#' Statistics}. Thousand Oaks (CA): Sage. pp. 651-657.
#' @keywords datasets
NULL





#' Six wines described by several assessors with rank attributes.
#' 
#' 
#' @name pca.wine
#' @docType data
#' @usage data(pca.wine)
#' @format pca.wine$data: A data matrix with 6 wines (rows) from several
#' assessors described by 11 attributes (columns). For use with
#' \code{\link{epPCA}}.
#' @seealso \link{mca.wine}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.
#' @keywords datasets
NULL





#' Small data set for Partial Least Squares-Correspondence Analysis
#' 
#' The data come from a larger study on marijuauna dependent individuals (see
#' Filbey et al., 2009) and are illustrated in Beaton et al., 2013. \cr The
#' data contain 2 genetic markers and 3 additional drug use questions from 50
#' marijuauna dependent individuals.
#' 
#' In snps.druguse$DATA1:\cr e - Stands for ecstacy use. Responses are yes or
#' no.  cc - Stands for crack/cocaine use. Responses are yes or no.  cm -
#' Stands for crystal meth use. Responses are yes or no. \cr In
#' snps.druguse$DATA2:\cr COMT - Stands for the COMT gene. Alleles are AA, AG,
#' or GG. Some values are NA.  FAAH - Stands for FAAH gene. Alleles are AA, CA,
#' CC. Some values are NA.
#' 
#' @name snps.druguse
#' @docType data
#' @usage data(snps.druguse)
#' @format snps.druguse$DATA1: Fifty marijuana dependent participants indicated
#' which, if any, other drugs they have ever used.\cr snps.druguse$DATA2: Fifty
#' marijuana dependent participants were genotyped for the COMT and FAAH genes.
#' @references Filbey, F. M., Schacht, J. P., Myers, U. S., Chavez, R. S., &
#' Hutchison, K. E. (2009). Marijuana craving in the brain. Proceedings of the
#' National Academy of Sciences, 106(31), 13016 -- 13021.
#' 
#' Beaton D., Filbey F. M., Abdi H. (2013, in press). Integrating Partial Least
#' Squares Correlation and Correspondence Analysis for Nominal Data. In Abdi H,
#' Chin W, Esposito-Vinzi V, Russolillo G, Trinchera L. \emph{Proceedings in
#' Mathematics and Statistics (Vol. 56): New Perspectives in Partial Least
#' Squares and Related Methods}. New York, NY: Springer-Verlag.
#' @keywords datasets
NULL





#' Six wines described by 3 assessors.
#' 
#' How six wines are described by 3 assessors across various flavor profiles,
#' totaling 10 columns.
#' 
#' 
#' @name wines2007
#' @docType data
#' @usage data(wines2007)
#' @format wines2007$data: A data set with 3 experts (studies) describing 6
#' wines (rows) using several variables using a scale from 1 to 7 with a total
#' of 10 measures (columns).\cr wines2007$table: A data matrix which identifies
#' the 3 experts (studies). \cr
#' @references Abdi, H., & Valentin, D. (2007). STATIS. In N.J. Salkind (Ed.):
#' \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage.
#' pp. 955-962.
#' @keywords datasets
NULL





#' Wines Data from 12 assessors described by 15 flavor profiles.
#' 
#' 10 experts who describe 12 wines using four variables (cat-pee, passion
#' fruit, green pepper, and mineral) considered as standard, and up to two
#' additional variables if the experts chose.
#' 
#' 
#' @name wines2012
#' @docType data
#' @usage data(wines2012)
#' @format wines2012$data: A data set with 10 experts (studies) describing 12
#' wines (rows) using four to six variables using a scale from 1 to 9 with a
#' total of 53 measures (columns).\cr wines2012$table: A data matrix which
#' identifies the 10 experts (studies). \cr wines2012$supplementary: A data
#' matrix with 12 wines (rows) describing 4 Chemical Properties (columns).\cr
#' @references Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M.
#' (2012). STATIS and DISTATIS: Optimum multi-table principal component
#' analysis and three way metric multidimensional scaling. \emph{Wiley
#' Interdisciplinary Reviews: Computational Statistics}, 4, 124-167.
#' @keywords datasets
NULL





#' Twenty words described by 2 features.
#' 
#' Twenty words ``randomly'' selected from a dictionary and described by two
#' features: length of word and number of definitions.
#' 
#' 
#' @name words
#' @docType data
#' @usage data(words)
#' @format words$data: A data matrix with 20 words (rows) described by 2
#' attributes (columns). For use with \code{\link{epPCA}}.
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.
#' @keywords datasets
NULL



