#==============================================================================#
#                          Simple Linear Regression                            #
#==============================================================================#
#' slr
#'
#' \code{slr} Performs simple linear regression of all quantitative variables
#' on the designated response variable.
#'
#' @param data Data frame containing movie data
#' @param y Character string containing the name of the response variable
#'
#' @return Data frame containing summary statistics for each model
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
slr <- function(data, y) {

  # Select quantitative predictors
  omit <- c("box_office", "box_office", y)
  nums <- sapply(data, is.numeric)
  d <- data[, nums]
  p <- colnames(d)
  predictors <- p[!(p %in% omit)]

  # Run regresssion models and report regression summary
  s <- data.table::rbindlist(lapply(predictors, function(x) {
    m <- list()
    f <-  formula(paste(y, " ~ ", paste(x, collapse=" + ")))
    m[["model"]] <- lm(f, data)
    a <- regressionAnalysis(mod = m, mName = x, yVar = "box_office_log",
                       yLab = "Log of Daily Box Office", full = FALSE)
    a$glance
  }))

  # Select significant models (p-value < 0.05) sorted by adjusted R2
  s <- s %>% filter(`p-value` < .05) %>% arrange(desc(.[[9]]))

  # Return final model
  f <- formula(paste(y, " ~ ", paste(s$Model[1], collapse=" + ")))
  m <- lm(f, data)

  # Return results
  analysis <- list()
  analysis[["summary"]] <- s
  analysis[["model"]] <- m
  return(analysis)
}
