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
#' @family regression functions
#' @export
evalForwardModel <- function(data, model, y, remaining) {

  models <- data.table::rbindlist(lapply(seq_along(remaining), function(x) {
    newPredictors <- c(model$Selected, remaining[x])
    lrFormula <- formula(paste(y, " ~ ",  paste(colnames(data[newPredictors]), collapse=" + ")))
    m <- lm(lrFormula, data)
    a <- anova(m)
    s <- summary(m)
    data.frame(Selected = remaining[x],
               `Model Size` = length(newPredictors),
               DF = paste(s$df[1], s$df[2], collapse = ","),
               `F-statistic` = round(s$fstatistic[1], 2),
               `R-Squared` = round(s$r.squared, 3),
               Adjusted.R2 = round(s$adj.r.squared, 3),
               `p-value` = a$`Pr(>F)`[1],
               stringsAsFactors = FALSE,
               row.names = NULL)
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
#' @family regression functions
#' @export
forward <- function(data, y) {

  final <- list()
  final[["data"]] <- data

  # Initialize key variables
  bestAdjR2 <- newAdjR2 <- 0
  predictors <- colnames(data)
  predictors <- predictors[!(predictors == y)]
  model <- data.frame()
  remaining <- predictors

  # Iterate until all predictors are evaluated and return the variables for the best model
  for (i in 1:length(predictors)) {
    best <- evalForwardModel(data, model, y, remaining)
    remaining <- remaining[remaining != best$Selected]
    if (best$Adjusted.R2[1] > bestAdjR2) {
      model <- rbind(model, best)
      bestAdjR2 <- best$Adjusted.R2
    }
  }

  # Format report
  Step <- c(1:nrow(model))
  model <- cbind(Step, model, row.names = NULL)
  v <- c(0)
  for (i in 2:nrow(model)) {
    delta <- (model$Adjusted.R2[i] - model$Adjusted.R2[i-1]) / model$Adjusted.R2[i-1] * 100
    v <- c(v, delta)
  }
  model$`Pct Chg` <- round(v, 2)
  final[["build"]] <- model

  # Return final model
  lrFormula <- formula(paste(y, " ~ ",
                             paste(model$Selected, collapse=" + ")))
  m <- lm(lrFormula, data)
  a <- anova(m)
  s <- summary(m)
  final[["model"]] <- m

  return(final)
}
