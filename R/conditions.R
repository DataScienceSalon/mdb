#==============================================================================#
#                                 process                                      #
#==============================================================================#
#' process
#'
#' \code{process} Pepares the data for the bivariate analysis stage
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(mdb) {
  mdb <- mdb %>% filter(title_type != "TV Movie" & imdb_rating != "NC-17")
  return(mdb)
}
