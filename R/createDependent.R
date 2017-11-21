#==============================================================================#
#                             createDependent                                  #
#==============================================================================#
#' createDependent
#'
#' \code{createDependent} Creates a variable based upon the coefficients of
#' the model parameter.
#'
#'
#' @param data Data frame containing movie data
#' @param mod = Linear model used to create the dependent variable
#'
#' @return Data frame with dependent variable added.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
createDependent <- function(data, mod) {

  # Create new dependent variable
  d <- data %>% mutate(daily_log_box_office_est = mod$coefficients[1] +
                        imdb_num_votes_log * mod$coefficients[2] +
                        imdb_rating * mod$coefficients[3] +
                        audience_score * mod$coefficients[4])

  return(d)
}
