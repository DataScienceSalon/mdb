#==============================================================================#
#                       Backward Elmination Regression                         #
#==============================================================================#

#------------------------------------------------------------------------------#
#                 Univariate Backward Elmination Regression                    #
#------------------------------------------------------------------------------#
#' evalBackwardModel
#'
#' \code{evalBackwardModel} Performs step-wise backward elimination of univariate
#' linear regression models.  From the full model, each variable is removed
#' from the model, one-at-a-time, and the model is re-evaluated. The model
#' with the highest adjusted r-squared is retained.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
evalBackwardModel <- function(data, y, predictors) {

  predictorSet <- lapply(predictors, function(x) {
    p <- list(
      omit = x,
      predictors = predictors[(!(predictors %in% x))]
    )
    p
  })
  models <- rbindlist(lapply(seq_along(predictorSet), function(x) {
    newPredictors <- predictorSet[[x]][predictorSet[[x]] != predictorSet[[x]]$omit]$predictors
    lrFormula <- formula(paste(y, " ~ ",  paste(newPredictors, collapse=" + ")))
    s <- summary(lm(lrFormula, data))
    data.frame(Removed = predictorSet[[x]]$omit,
               Predictors = length(newPredictors),
               Adjusted.R2 = s$adj.r.squared,
               stringsAsFactors = FALSE)
  }))
  best <- models %>% filter(Adjusted.R2 == max(Adjusted.R2))
  return(head(best, 1))
}



#------------------------------------------------------------------------------#
#                       Backward Elmination Regression                         #
#------------------------------------------------------------------------------#
#' backward
#'
#' \code{backward} Creates prediction model using backward elimination
#'
#' @param data Data frame containing the full model#'
#' @param y Character string containing the name of the response variable
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
back <- function(data, y, alpha = 0.05) {

  final <- list()

  # Initialize key variables
  predictors <- colnames(data)
  predictors <- predictors[!(predictors == y)]
  lrFormula <- formula(paste(y, " ~ ",
                             paste(predictors, collapse=" + ")))
  t <- anova(lm(lrFormula, data = data))
  t$Predictor <- rownames(t)
  remove <- head(t %>% filter(t$`Pr(>F)` < alpha) %>% arrange(desc(`Pr(>F)`)) %>% select(Predictor), 1)

  baseModel <- summary(lm(lrFormula, data))
  newAdjR2 <- baseModel$adj.r.squared

  while(length(removed) < length(predictors)) {
    bestAdjR2 <- newAdjR2
    best <- evalBackwardModel(data, y, predictors)
    removed <- c(removed, best$Removed)
  }

  finalPredictors <- predictors[!(predictors %in% removed)]
  lrFormula <- formula(paste(y, " ~ ",  finalPredictors, collapse=" + "))
  finalModel <- lm(lrFormula, data)


  return(finalModel)

}
