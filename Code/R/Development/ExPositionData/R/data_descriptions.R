#' Marvel Cinematic Universe (actors x movies; 2017).
#'
#' These data were created because Derek is a nerd. See <https://github.com/derekbeaton/Marvel-Cinematic-Universe_Network>
#'
#' @format A list with two items:
#' \describe{
#' \item{marvel_movies_actors.by.movies}{a 938 actors x 12806 movies matrix; values are 1 (actor was in a movie) or 0 (actor was not in a movie).}
#' \item{movie.info}{a 12806 movies x 2 attributes matrix; a simple list to map movie name (year) to the IMDB short codes.}
#' }
"mcu.movie.actors"

#' Dallas-Forth Worth beer and brewery survey (autumn/winter 2014)
#'
#' These data were created because Derek was bored and loves beer. See <http://www.dallasobserver.com/restaurants/dfws-favorite-local-beer-as-proven-by-an-actual-scientist-7024953> and <http://www.derekbeaton.com/dallass-favorite-beers/>
#'
#' @format A list with two items:
#' \describe{
#' \item{original}{A list with many items exported from Google Sheets. Contains information on respondents and beers for the survey.}
#' \item{mca.ex}{A list with three items that are the prepare data (for use with multiple correspondence analysis or similar technique)}
#' }
"dfw.beer.survey"
