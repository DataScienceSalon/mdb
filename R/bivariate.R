#==============================================================================#
#                                 bivariate                                    #
#==============================================================================#
#' bivariate
#'
#' \code{bivariate} Performs bivariate analysis of variables
#'
#' @param data List containing two data frames, mdb, the main movie data set and mdbBox the sample with box office revenue
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
bivariate <- function(data) {

  #---------------------------------------------------------------------------#
  #                       Box Office Revenue Analysis                         #
  #---------------------------------------------------------------------------#
  #                         Categorical Predictors                            #
  #---------------------------------------------------------------------------#
  # Title Type
  data <- data$mdbBox %>% select(title_type, box_office_log)
  mod <- lm(box_office_log ~ title_type, data = data)
  type <- regressionAnalysis(mod = mod, xLab = "Type", yLab = "Log Box Office")

  # Genre
  data <- data$mdbBox %>% select(genre, box_office_log)
  mod <- lm(box_office_log ~ genre, data = data)
  genre <- regressionAnalysis(mod = mod, xLab = "Genre", yLab = "Log Box Office")


  analysis <- list(
    alpha = list(
      type = type,
      genre = genre
    )
  )
  return(analysis)

}
