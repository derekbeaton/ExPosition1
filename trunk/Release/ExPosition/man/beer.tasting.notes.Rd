\name{beer.tasting.notes}
\alias{beer.tasting.notes}


\docType{data}

\title{Some of authors' personal beer tasting notes.}

\description{
Tasting notes, preferences, breweries and styles of 38 different craft beers from various breweries, across various styles.
}

\usage{data(beer.tasting.notes)}

\format{
beer.tasting.notes$data: Data matrix. Tasting notes (ratings) of 38 different beers (rows) described by 16 different flavor profiles (columns).\cr
beer.tasting.notes$brewery.design: Design matrix. Source brewery of 38 different beers (rows) across 26 breweries (columns). \cr
beer.tasting.notes$style.design: Design matrix. Style of 38 different beers (rows) across 20 styles (columns) (styles as listed from Beer Advocate website). \cr
beer.tasting.notes$sup.data: Supplementary data matrix. ABV and overall preference ratings of 38 beers described by two features (ABV & overall) in original value and rounded value.
}


\references{
	http://www.beeradvocate.com
}
\source{
Jenny Rieck and Derek Beaton laboriously ``collected'' these data for ``experimental purposes''.
}

\keyword{datasets}
