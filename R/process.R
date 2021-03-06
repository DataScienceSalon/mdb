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
#' @family regression functions
#' @export
process <- function(data, stage = "a", y, outliers = NULL) {

  # Define data stages in terms of the variables to omit
  omitA <- c( 'actor1', 'actor2', 'actor3', 'actor4', 'actor5', "box_office",
              'cast_dir_exp', 'cast_dir_exp_log', 'cast_dir_scores',
              'cast_dir_scores_log', 'cast_dir_votes', 'cast_dir_votes_log',
              'cast_exp_dir_scores', 'cast_exp_dir_scores_log',
              'cast_exp_dir_votes', 'cast_exp_dir_votes_log',
              'cast_experience_log', 'cast_scores_dir_exp',
              'cast_scores_dir_exp_log', 'cast_scores_dir_votes',
              'cast_scores_dir_votes_log', 'cast_scores_log',
              'cast_votes_dir_exp', 'cast_votes_dir_exp_log',
              'cast_votes_dir_scores', 'cast_votes_dir_scores_log',
              'cast_votes_log', 'director_experience_log',
              'director_scores_log', 'director_votes_log',
              'dvd_rel_day', 'dvd_rel_month', 'dvd_rel_year',
              'imdb_url', 'rt_url', 'scores', 'scores_log', 'thtr_days',
              'thtr_days_log', 'thtr_rel_day', 'thtr_rel_year',
              'title', 'title_type', 'votes_per_day', 'votes_per_day_log',
              'votes_per_day_scores', 'votes_per_day_scores_log')

  omitB <- c('votes_per_day_log', 'imdb_num_votes', 'box_office_log',
             'votes_per_day')

  omitC <- c('votes_per_day', 'votes_per_day_log', 'box_office',
             'box_office_log', 'imdb_num_votes', 'thtr_days', 'thtr_days_log')

  omitM <- c('title', 'title_type', 'runtime', 'cast_dir_exp',
             'cast_dir_exp_log', 'imdb_url', 'rt_url', 'studio', 'director',
             'actor1', 'actor2', 'actor3', 'actor4', 'actor5',
             'thtr_rel_date', 'thtr_rel_year', 'thtr_rel_season',
             'thtr_rel_day', 'thtr_days', 'thtr_days_log',
             'dvd_rel_year', 'dvd_rel_month', 'dvd_rel_day', 'cast_dir_scores',
             'cast_dir_scores_log', 'cast_dir_votes', 'imdb_num_votes',
             'imdb_rating', 'cast_exp_dir_scores', 'critics_rating',
             'audience_rating', 'critics_score', 'audience_score',
             'best_pic_nom', 'best_pic_win', 'best_actor_win',
             'best_actress_win', 'best_dir_win', 'top200_box',
             'cast_exp_dir_votes', 'cast_exp_dir_votes_log', 'cast_experience',
             'cast_scores_dir_exp', 'director_scores_log', 'cast_scores_dir_votes',
             'cast_scores_dir_votes_log', 'cast_votes', 'cast_votes_dir_exp',
             'cast_votes_dir_scores', 'cast_votes_dir_scores_log',
             'cast_votes_log', 'director_experience', 'director_experience_log',
             'director_votes', 'director_votes_log', 'votes_per_day',
             'votes_per_day_log', 'box_office', 'box_office_log',
             'cast_scores_dir_exp_log',
             'cast_votes_dir_exp_log', 'cast_scores_log', 'cast_exp_dir_scores_log',
             'cast_dir_votes_log', 'director_scores', 'cast_experience_log')


  keep <- c("box_office_log", "audience_score", "imdb_rating",
             "imdb_num_votes_log", "votes_per_day",
            "votes_per_day_log")

  omit <- switch(stage,
                 a = omitA,
                 b = c(omitA, omitB),
                 c = c(omitA, omitB, omitC),
                 m = c(omitA, omitB, omitC, omitM))


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
  if (stage %in% c("a","b", "c", "m")) {
    d <- d[, (!(names(d) %in% omit))]
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
