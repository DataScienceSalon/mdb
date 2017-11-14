#==============================================================================#
#                           forward Selection                                  #
#==============================================================================#

#------------------------------------------------------------------------------#
#                          Evaluate Models Univariate                          #
#------------------------------------------------------------------------------#
#' evaluate
#'
#' \code{evaluate} Evaluates linear models for all remaining predictors, one
#' at a time, then returns the variable and adjusted R2 score for the best
#' model.
#'
#' @param data Data frame containing the full model
#' @param model, Data frame containing the variables and adjusted R2 values for the best model as of..
#' @param y Character string containing the name of the response variable
#' @param remaining Vector containing column names for the remaining predictors
#'
#' @return Single row data frame containing the variable and adjusted R2 for the best model evaluated
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
evalModel <- function(data, model, y, remaining) {

  models <- rbindlist(lapply(remaining, function(x) {
    newPredictors <- c(model$Selected, x)
    lrFormula <- formula(paste(y, " ~ ",  paste(colnames(data[newPredictors]), collapse=" + ")))
    mod <- summary(lm(lrFormula, data))
    data.frame(Selected = x,
               Adjusted.R2 = mod$adj.r.squared,
               stringsAsFactors = FALSE)
  }))
  best <- models %>% filter(Adjusted.R2 == max(Adjusted.R2))
  return(best)
}
#------------------------------------------------------------------------------#
#                          Forward Select Main Function                        #
#------------------------------------------------------------------------------#
#' forward
#'
#' \code{forward} Creates prediction model using forward selection
#'
#' @param data Data frame containing the full model
#' @param y Character string containing the name of the response variable
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
forward <- function(data, y) {

  # Initialize key variables
  bestAdjR2 <- newAdjR2 <- 0
  predictors <- colnames(data)
  predictors <- predictors[!(predictors == y)]
  model <- data.frame()
  remaining <- predictors

  # Iterate until all predictors are evaluated and return the variables for the best model
  while (length(remaining) > 0) {
    best <- evalModel(data, model, y, remaining)
    remaining <- remaining[!(remaining == best$Selected)]
    if (best$Adjusted.R2 > bestAdjR2) {
      model <- rbind(model, best)
      bestAdjR2 <- best$Adjusted.R2
    }
  }

  # Return final model
  lrFormula <- formula(paste(y, " ~ ",
                             paste(colnames(data)[model$Selected], collapse=" + ")))
  finalModel <- lm(lrFormula, data)

  return(finalModel)
}
