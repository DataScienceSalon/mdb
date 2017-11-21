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
#' @param stage Character indicating the stage for which the data is prepared
#'   a - used for the exploratory data analysis
#'   m - used for modeling stage
#' @param y Character string containing the name of the response variable
#' @param outliers Vector of observations to remove
#'
#' @return List with two data frames, independent variables and the dependent
#' variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(data, stage = "a", y, outliers = NULL) {

  # Define data stages in terms of the variables to omit
  omitA <- c('title',	'title_type',	'imdb_url',	'rt_url',
             'thtr_rel_year',	'thtr_rel_day',	'dvd_rel_year',
             'dvd_rel_month',	'dvd_rel_day',	'scores',	'scores_log',
             'votes_per_day_scores',	'votes_per_day_scores_log',
             'daily_box_office', "box_office", 'actor1', 'actor2',
             'actor3', 'actor4', 'actor5', 'director', 'studio')

  omitM <- c( 'thtr_days',	'thtr_days_log', 'imdb_num_votes',
             'cast_experience',	'imdb_rating', 'votes_per_day', 'critics_score',
             'audience_score',	 'thtr_rel_date', 'votes_per_day_log',
             'thtr_days', 'daily_log_box_office_est', 'cast_experience_log',
             'thtr_days_log', 'runtime','imdb_rating', 'best_pic_nom',
             'best_dir_win', 'best_pic_win' , 'director_experience',
             'best_actor_win', 'best_actress_win', 'thtr_rel_season',
             'daily_box_office_log', 'critics_rating',
             'audience_rating', 'top200_box')
  keep <- c("daily_box_office_log", "audience_score", "imdb_rating",
             "imdb_num_votes_log", "votes_per_day",
            "votes_per_day_log")

  omit <- switch(stage,
                 a = omitA,
                 m = c(omitA, omitM))

  d <- data
  # Remove outliers
  if (!is.null(outliers))  {
    if (class(outliers) == "numeric") {
      d <- d[-outliers, ]
    } else {
      d <- d %>% filter((!title %in% outliers))
    }
  }

  # Gather features requested by the data stage
  if (stage %in% c("a", "m")) {
    d <- d[, !(names(d) %in% omit)]
  } else {
    d <- d[, (names(d) %in% keep)]
  }

  # Get Dependent variable
  yVar <- as.data.frame(d[, names(d) %in% y])
  names(yVar) <- y

  # Return full data set with outliers removed
  pack <- list()
  pack[["data"]] <- d

  # Split Data
  nums <- sapply(d, is.numeric)
  pack[["numeric"]] <- d[, nums]
  pack[["numeric"]] <- as.data.frame(pack[["numeric"]][, !(names(pack[["numeric"]]) %in% y)])
  pack[["categorical"]] <- d[, !nums]
  pack[["dependent"]] <- yVar
  pack[["full"]] <- cbind(pack[["categorical"]], pack[["numeric"]], pack[["dependent"]])

  return(pack)
}
