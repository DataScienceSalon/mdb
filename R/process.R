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
#' @param outliers Vector of observations to remove
#'
#' @return List with two data frames, independent variables and the dependent
#' variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(data, y, outliers = NULL) {

  # Features not included in analysis
  keep <- c('best_actor_win',	'best_actress_win',	'best_dir_win',
            'best_pic_nom',	'best_pic_win',	'cast_experience_log',
            'cast_votes_log',	'critics_score',	'director_experience_log',
            'genre',	'imdb_num_votes_log',	'mpaa_rating',
            'runtime_log',	'votes_per_day_scores_log',
            'thtr_rel_month')


  # Remove outliers
  d <- data %>% filter(title_type != "TV Movie" & mpaa_rating != "NC-17")
  d$mpaa_rating <- factor(d$mpaa_rating)
  if (!is.null(outliers))  {
    if (class(outliers) == "numeric") {
      d <- d[-outliers, ]
    } else {
      d <- d %>% filter((!title %in% outliers))
    }
  }

  # Return full data set with outliers removed
  pack <- list()
  pack[["data"]] <- d

  # Split Data
  d <- d[, (names(d) %in% keep)]
  nums <- sapply(d, is.numeric)
  pack[["numeric"]] <- d[, nums]
  pack[["numeric"]] <- as.data.frame(pack[["numeric"]][, !(names(pack[["numeric"]]) %in% y)])
  pack[["categorical"]] <- d[, !nums]
  pack[["dependent"]] <- as.data.frame(d[, names(d) %in% y])
  pack[["full"]] <- cbind(pack[["categorical"]], pack[["numeric"]], pack[["dependent"]])

  return(pack)
}
