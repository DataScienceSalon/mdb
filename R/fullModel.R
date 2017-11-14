#==============================================================================#
#                                Full Model                                    #
#==============================================================================#
#' fullModel
#'
#' \code{fullModel} Summarizes the variables used in the full model
#'
#' @param associations Data frame containing associations between the categorical variables and the dependent variable.
#' @param correlations Data frame containing correlations between the quantitative variables and the dependent variable.
#'
#' @return Data frame listing variables selected for the full model.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
fullModel <- function(associations, correlations) {

  categorical <- data.frame(Variable = associations$Independent,
                            Type = "Categorical",
                            Measure = "R-squared",
                            Value = associations$R.squared,
                            `p-value` = associations$p.value,
                            row.names = NULL)
  quantitative <- data.frame(Variable = correlations$Variable,
                             Type = "Quantitative",
                             Measure = "Correlation Coef.",
                             Value = correlations$Correlation,
                             `p-value` = correlations$p.value,
                             row.names = NULL)
  model <- rbind(categorical, quantitative)
  return(model)
}



