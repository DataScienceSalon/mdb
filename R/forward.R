#==============================================================================#
#                           forward Selection                                  #
#==============================================================================#

#------------------------------------------------------------------------------#
#                          Evaluate Models Univariate                          #
#------------------------------------------------------------------------------#
#' evalForwardModel
#'
#' \code{evalForwardModel} Evaluates linear models for all remaining predictors, one
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
evalForwardModel <- function(data, model, y, remaining) {

  models <- rbindlist(lapply(seq_along(remaining), function(x) {
    newPredictors <- c(model$Selected, remaining[x])
    lrFormula <- formula(paste(y, " ~ ",  paste(colnames(data[newPredictors]), collapse=" + ")))
    mod <- lm(lrFormula, data)
    data.frame(Selected = remaining[x],
               Adjusted.R2 = summary(mod)$adj.r.squared,
               stringsAsFactors = FALSE)
  }))
  best <- models %>% filter(Adjusted.R2 == max(Adjusted.R2))
  return(head(best, 1))
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

  final <- list()

  # Initialize key variables
  bestAdjR2 <- newAdjR2 <- 0
  predictors <- colnames(data)
  predictors <- predictors[!(predictors == y)]
  model <- data.frame()
  remaining <- predictors

  # Iterate until all predictors are evaluated and return the variables for the best model
  while (length(remaining) > 0) {
    best <- evalForwardModel(data, model, y, remaining)
    remaining <- remaining[remaining != best$Selected]
    if (best$Adjusted.R2[1] > bestAdjR2) {
      model <- rbind(model, best)
      bestAdjR2 <- best$Adjusted.R2
    }
  }

  # Format report
  Step <- c(1:nrow(model))
  model <- cbind(Step, model)
  v <- c(0)
  for (i in 2:nrow(model)) {
    delta <- (model$Adjusted.R2[i] - model$Adjusted.R2[i-1]) / model$Adjusted.R2[i-1] * 100
    v <- c(v, delta)
  }
  model$`Pct Chg` <- round(v, 2)
  final[["steps"]] <- model

  # Return final model
  lrFormula <- formula(paste(y, " ~ ",
                             paste(model$Selected, collapse=" + ")))
  final[["model"]] <- lm(lrFormula, data)

  return(final)
}
