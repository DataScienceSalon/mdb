#==============================================================================#
#                                 process                                      #
#==============================================================================#
#' process
#'
#' \code{process} Prepares the data for the bivariate analysis stage
#'
#' @param dataSets List containing the two movie data frames
#'
#' @return df Data frame of clean movie data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(dataSets) {

  d <- list()

  d[["mdb1"]] <- dataSets$mdb1 %>% filter(title_type != "TV Movie" & imdb_rating != "NC-17")
  d[["mdb2"]] <- dataSets$mdb2 %>% filter(title_type != "TV Movie" & imdb_rating != "NC-17")
  return(d)
}
