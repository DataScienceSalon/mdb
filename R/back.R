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
    newPredictors <- predictorSet[[x]]$predictors
    f <- lrFormula <- formula(paste(y, " ~ ",  paste(newPredictors, collapse=" + ")))
    m <- lm(f, data = data)
    a <- anova(m)
    s <- summary(m)
    data.frame(removed = predictorSet[[x]]$omit,
               size = length(predictorSet[[x]]$predictors),
               p = broom::glance(m)$p.value,
               stringsAsFactors = FALSE)
  }))
  remove <- models %>% filter(p ==max(p))
  return(head(remove, 1))
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
back <- function(data, y, alpha = 0.15) {

  final <- list()

  # Initialize key variables
  predictors <- colnames(data)
  predictors <- predictors[!(predictors == y)]


  # Iterate through predictor sets
  p <- 1
  f <- lrFormula <- formula(paste(y, " ~ ",  paste(newPredictors, collapse=" + ")))
  m <- lm(f, data = data)

  while(length(removed) < length(predictors)) {
    f <- lrFormula <- formula(paste(y, " ~ ",  paste(newPredictors, collapse=" + ")))
    m <- lm(f, data = data)
    a <- as.data.table(broom::tidy(anova(m)))
    a <- a %>% filter(!is.na(p.value))
    r <- a %>% filter(p.value == max(p.value)) %>% select(term, p.value)
  }



  finalPredictors <- predictors[!(predictors %in% removed)]
  lrFormula <- formula(paste(y, " ~ ",  finalPredictors, collapse=" + "))
  finalModel <- lm(lrFormula, data)


  return(finalModel)

}
