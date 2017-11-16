#==============================================================================#
#                                Full Model                                    #
#==============================================================================#
#' fullModel
#'
#' \code{fullModel} Summarizes the variables used in the full model
#'
#' @param data Data set containing the unfiltered full model
#'
#' @return Data frame listing variables selected for the full model.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
fullModel <- function(data) {

  fModel <- list()

  # Indicate variables in the full model
  selected <- c('best_actor_win',	'best_actress_win',	'best_dir_win',	'best_pic_nom',
                'best_pic_win',	'genre',	'mpaa_rating',	'thtr_rel_month', "imdb_rating",
                'thtr_rel_season',	'cast_experience_log', 'cast_votes_log',
                'critics_score',	'director_experience_log', 'runtime_log')

  # Extract Data
  fModel[["categorical"]] <- data$categorical[names(data$categorical) %in% selected]
  fModel[["quantitative"]] <- data$numeric[names(data$numeric) %in% selected]
  fModel[["dependent"]] <- as.data.frame(data$dependent)
  fModel[["independent"]] <- cbind(fModel$categorical, fModel$quantitative)
  fModel[["all"]] <- cbind(fModel$independent, fModel$dependent)

  # Compute associations for categorical variables
  associations <- associate(x = fModel$categorical, y = fModel$dependent, yLab = "Log IMDB Votes")
  categorical <- data.frame(Variable = associations$tests$Independent,
                            Type = rep("Categorical", nrow(associations$tests)),
                            Measure = rep("R-squared", nrow(associations$tests)),
                            Value = associations$tests$R.squared,
                            `p-value` = associations$tests$p.value,
                            row.names = NULL)

  # Compute correlations for quantitative variables
  correlations <- correlate(x = fModel$quantitative, y = fModel$dependent)
  quantitative <- data.frame(Variable = correlations$tests$Variable,
                             Type = rep("Quantitative", nrow(correlations$tests)),
                             Measure = rep("Correlation Coef.", nrow(correlations$tests)),
                             Value = correlations$tests$Correlation,
                             `p-value` = correlations$tests$p.value,
                             row.names = NULL)

  fModel[["report"]] <- rbind(categorical, quantitative, row.names = NULL)

  return(fModel)
}



