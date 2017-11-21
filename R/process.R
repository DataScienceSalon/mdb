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
#' @param group Character indicating the data group to return.
#'   a - Most conservative. Includes almost all variables
#'   b <- Moderate data set with some data variables remomved
#'   c >- Liberat data set focusing only on variables most predictive of log IMDB votes
#' @param y Character string containing the name of the response variable
#' @param outliers Vector of observations to remove
#'
#' @return List with two data frames, independent variables and the dependent
#' variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
process <- function(data, group = "a", y, outliers = NULL) {

  # Define data groups in terms of the variables to omit
  omitA <- c('title',	'title_type',	'imdb_url',	'rt_url',
             'thtr_rel_year',	'thtr_rel_day',	'dvd_rel_year',
             'dvd_rel_month',	'dvd_rel_day',	'scores',	'scores_log',
             'votes_per_day_scores',	'votes_per_day_scores_log',
             'daily_box_office', "box_office", 'actor1', 'actor2',
             'actor3', 'actor4', 'actor5', 'director', 'studio')

  omitB <- c("imdb_num_votes", 'votes_per_day',	'votes_per_day_log',
             'votes_per_day_scores',	'votes_per_day_scores_log')

  omitC <- c('runtime',  'thtr_days',	'thtr_days_log', 	'director_experience',
             'cast_experience',	'imdb_num_votes',	'imdb_rating',
             'audience_score',	'cast_votes',	'scores',	'scores_log',
             'votes_per_day',	'votes_per_day_log', 'thtr_rel_date',
             'thtr_rel_season', 'daily_box_office_log', 'critics_rating',
             'audience_rating', 'top200_box')

  omit <- switch(group,
                 a = omitA,
                 b = c(omitA, omitB),
                 c = c(omitA, omitB, omitC))

  d <- data
  # Remove outliers
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

  # Gather features requested by the data group
  d <- d[, !((names(data) %in% omit))]

  # Get Dependent variable
  yVar <- as.data.frame(d[, names(d) %in% y])
  names(yVar) <- y

  # Split Data
  nums <- sapply(d, is.numeric)
  pack[["numeric"]] <- d[, nums]
  pack[["numeric"]] <- as.data.frame(pack[["numeric"]][, !(names(pack[["numeric"]]) %in% y)])
  pack[["categorical"]] <- d[, !nums]
  pack[["dependent"]] <- yVar
  pack[["full"]] <- cbind(pack[["categorical"]], pack[["numeric"]], pack[["dependent"]])

  return(pack)
}
