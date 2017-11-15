#==============================================================================#
#                                Full Model                                    #
#==============================================================================#
#' fullModel
#'
#' \code{fullModel} Summarizes the variables used in the full model
#'
#' @param data Data set containing the unfiltered full model
#' @param associations Data frame containing associations between the categorical variables and the dependent variable.
#' @param correlations Data frame containing correlations between the quantitative variables and the dependent variable.
#'
#' @return Data frame listing variables selected for the full model.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
fullModel <- function(data, associations, correlations) {

  fModel <- list()

  # Indicate variables in the full model
  selected <- c('best_actor_win',	'best_actress_win',	'best_dir_win',	'best_pic_nom',
                'best_pic_win',	'genre',	'mpaa_rating',	'thtr_rel_month',
                'thtr_rel_season',	'cast_experience_log', 'cast_votes_log',
                'critics_score',	'director_experience_log', 'runtime_log',
                'thtr_days_log')

  # Extract Data
  fModel[["categorical"]] <- data$categorical[names(data$categorical) %in% selected]
  fModel[["quantitative"]] <- data$numeric[names(data$numeric) %in% selected]
  fModel[["dependent"]] <- as.data.frame(data$dependent)
  independent <- cbind(data$categorical, data$numeric)
  fModel[["independent"]] <- independent[names(independent) %in% selected]
  fModel[["all"]] <- cbind(fModel[["independent"]], data$dependent)

  # Format Report Data
  categorical <- data.frame(Variable = associations$Independent,
                            Type = rep("Categorical", nrow(associations)),
                            Measure = rep("R-squared", nrow(associations)),
                            Value = associations$R.squared,
                            `p-value` = associations$p.value,
                            row.names = NULL)
  quantitative <- data.frame(Variable = correlations$Variable,
                             Type = rep("Quantitative", nrow(correlations)),
                             Measure = rep("Correlation Coef.", nrow(correlations)),
                             Value = correlations$Correlation,
                             `p-value` = correlations$p.value,
                             row.names = NULL)

  report <- rbind(categorical, quantitative, row.names = NULL)
  fModel[["report"]] <- report %>% filter(Variable %in% selected)


  return(fModel)
}



