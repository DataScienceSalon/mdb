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

  f <- lrFormula <- formula(paste(y, " ~ ",  paste(predictors, collapse=" + ")))
  m <- lm(f, data = data)
  s <- summary(m)
  a <- broom::tidy(anova(m))
  a <- a %>% filter(!is.na(p.value))
  p <- a %>% filter(p.value == max(p.value)) %>% select(term, p.value)
  r <- data.frame(Removed = p$term,
                  Size = nrow(a),
                  Adj.R2 = s$adj.r.squared,
                  p.value = p$p.value)
  return(r)
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
  final[["data"]] <- data

  # Initialize with full model
  predictors <- colnames(data)                  # Initialize predictors
  predictors <- predictors[!(predictors == y)]  # Remove response from list of predictors
  r <- evalBackwardModel(data = data, predictors = predictors, y = y)
  b <- data.frame()                             # Intialize build data frame

  # Iterate through predictor sets
  while(r$Size > 1 & r$p > alpha) {
    b <- rbind(b, r)
    predictors <- predictors[!(predictors == r$Removed)]
    r <- evalBackwardModel(data = data, predictors = predictors, y = y)
  }

  final[["selected"]] <- predictors

  # Format Build
  b$Steps <- c(1:nrow(b))
  final[["build"]] <- b %>% select(Steps, Removed, p.value)


  # Return final model
  f <- formula(paste(y, " ~ ", paste(predictors, collapse=" + ")))
  m <- lm(f, data)
  final[["model"]] <- m

  return(final)

}
