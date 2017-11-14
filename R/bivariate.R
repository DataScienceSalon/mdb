#==============================================================================#
#                                 correlate                                    #
#==============================================================================#
#' correlate
#'
#' \code{correlate} Given two data frames, one containing data for the explanatory
#' variables (x data frame) and another containing the data for the response
#' (y data frame), this function conducts correlation tests and produces
#' correlation plots.
#'
#' @param x Data frame containing the explanatory variable data
#' @param y Data frame containing the response data
#' @param yLab Character string containing label for the response variable
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
correlate <- function(x, y, yLab = NULL) {

  # Remove categorical variables
  drop <- c('title',	'title_type',	'genre',	'mpaa_rating',	'imdb_url',
            'rt_url',	'studio',	'director',	'actor1',	'actor2',	'actor3',
            'actor4',	'actor5',	'thtr_rel_year',	'thtr_rel_season',
            'thtr_rel_month',	'thtr_rel_day',	'dvd_rel_year',	'dvd_rel_month',
            'dvd_rel_day',	'critics_rating',	'audience_rating',	'best_pic_nom',
            'best_pic_win',	'best_actor_win',	'best_actress_win',	'best_dir_win',
            'top200_box',	'top200_box')
  x <- x[, !(names(x) %in% drop)]


  # Get variable names
  xNames <- colnames(x)
  yName <- ifelse(is.null(yLab), colnames(y), yLab)

  # Create correlation plot
  cPlot <- corrplot::corrplot(cor(x, y))

  # Conduct correlation tests
  cTests <- data.table::rbindlist(lapply(seq_along(xNames), function(idx) {
    t <- cor.test(x[[idx]], y)
    data.frame(Variable = xNames[[idx]],
               Correlation = round(t$estimate, 3),
               Statistic = round(t$statistic, 3),
               df = t$parameter,
               `p-value` = ifelse(round(t$p.value, 3) < 0.05, "< 0.05",
                                  round(t$p.value, 3)),
               `95% CI` = paste("[ ", round(t$conf.int[1], 2), ", ",
                                round(t$conf.int[2], 2), " ]")
               )
  }))
  cTests <- cTests %>% arrange(desc(abs(Correlation)))

  # Produce scatter plots
  sorted <- cTests$Variable
  sPlots <- lapply(seq_along(sorted), function(idx) {
    df <- data.frame(x = x[[sorted[idx]]],
                     y = y)
    plotScatter(data = df, xLab = sorted[idx], yLab = yName)
  })

  analysis <- list(
    tests = cTests,
    cPlot = cPlot,
    sPlots = sPlots
  )

  return(analysis)
}
