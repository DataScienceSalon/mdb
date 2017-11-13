#==============================================================================#
#                                 process                                      #
#==============================================================================#
#' process
#'
#' \code{process} Prepares the data for the bivariate analysis stage
#'
#' @param data Data frame containing movie data
#'
#' @return df Data frame of clean movie data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(data) {
  df <- data %>% filter(title_type != "TV Movie" & imdb_rating != "NC-17")
  return(df)
}
