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
  qual <- c('best_actor_win',	'best_actress_win',	'best_dir_win',
            'best_pic_nom',	'best_pic_win',	'genre',	'mpaa_rating',
            'thtr_rel_month',	'thtr_rel_season')

  quant <- c('audience_score', 'cast_experience',	'cast_experience_log',
             'cast_votes',	'cast_votes_log',	'critics_score',
             'director_experience',	'director_experience_log',	'imdb_num_votes',
             'imdb_num_votes_log',	'imdb_rating',	'runtime',	'runtime_log',
             'scores',	'scores_log',	'thtr_days',	'thtr_days_log')
  quant <- quant[!(quant %in% y)]

  pack <- list()
  pack[["dependent"]] <- as.data.frame(d[, names(d) %in% y])
  pack[["categorical"]] <- d[, names(d) %in% qual]
  pack[["numeric"]] <- d[, names(d) %in% quant]
  pack[["full"]] <- cbind(pack[["categorical"]], pack[["numeric"]], pack[["dependent"]])

  return(pack)
}
