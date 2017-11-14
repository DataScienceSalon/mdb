#==============================================================================#
#                                 process                                      #
#==============================================================================#
#' process
#'
#' \code{process} Prepares the data for the bivariate analysis stage. Filters
#' out non-essential data, outliers and splits the data set into a set of
#' dependent variables and set with the response variable.
#'
#'
#' @param data Data frame containing movie rating data
#' @param y Character string containing the name of the response variable
#'
#' @return List with two data frames, independent variables and the dependent
#' variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(data, y) {

  # Remove observations
  d <- data %>% filter(title_type != "TV Movie" & mpaa_rating != "NC-17")
  d$mpaa_rating <- factor(d$mpaa_rating)

  # Split Data
  qual <- c('thtr_rel_season',	'genre',	'mpaa_rating',	'studio',	'director',
            'thtr_rel_month',	'best_pic_nom',	'best_pic_win',	'best_actor_win',
            'best_actress_win',	'best_dir_win',	'top200_box')

  quant <- c('director_experience',	'cast_experience',	'cast_votes',
             'cast_votes_log',	'scores',	'scores_log',	'thtr_days',
             'imdb_num_votes',	'imdb_num_votes_log',  'imdb_rating',
             'critics_score',	 'audience_score')
  quant <- quant[!(quant %in% y)]

  pack <- list()
  pack[["dependent"]] <- as.data.frame(d[, names(d) %in% y])
  pack[["categorical"]] <- d[, names(d) %in% qual]
  pack[["numeric"]] <- d[, names(d) %in% quant]
  pack[["full"]] <- cbind(pack[["categorical"]], pack[["numeric"]], pack[["dependent"]])

  return(pack)
}
