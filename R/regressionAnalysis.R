#==============================================================================#
#                             regressionAnalysis                               #
#==============================================================================#
#' regressionAnalysis
#'
#' \code{regressionAnalysis} Performs regression analysis
#'
#' @param mod Linear model
#' @param mName Capitalized character string for the name of the linear model
#' @param yVar Character string containing the name of the dependent variable
#' @param yLab Capitalized character string for the dependent variable
#'
#' @return analysis plots, summary statistics, test results and interpretive text
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
regressionAnalysis <- function(mod, mName, yVar, yLab) {

  #---------------------------------------------------------------------------#
  #                             Format Data                                   #
  #---------------------------------------------------------------------------#
  # Residuals vs Fit
  rvf <- data.frame(Residuals = mod$residuals, Fitted = mod$fitted.values)
  # Residuals vs Predictor
  res <- mod$residuals

  #---------------------------------------------------------------------------#
  #                                 Summary                                   #
  #---------------------------------------------------------------------------#
  overview <- summary(mod)

  #---------------------------------------------------------------------------#
  #                                  Plots                                    #
  #---------------------------------------------------------------------------#
  plots <- list()
  # Linear Regression Plot
  plots[["regression"]] <- plotScatter(data = mod$model, xLab = mName, yLab = yLab,
                                       plotTitle = paste0(mName, ": Linear Regression"))

  plots[["linearity"]] <- plotLinear(mod = mod, yVar = "imdb_num_votes_log",
                                     yLab = "Log IMDB Votes")

  plots[["multicollinearity"]] <- plotCorr(mod = mod, yVar = "imdb_num_votes_log")

  # Residuals vs Fitted
  plots[["res_fitted"]] <- plotScatter(data = rvf, xLab = mName, yLab = "Residuals",
                                       plotTitle = paste0(mName, ": Residuals vs. Fitted"))
  # Residuals vs Predictor
  plots[["res_predictors"]] <- plotResAll(mod = mod, mName, xLab)

  # Residuals Histogram
  plots[["res_hist"]] <- plotHist(data = as.data.frame(res), yLab = paste("Residuals:", yLab),
                      plotTitle = paste("Distribution of Residuals:", yLab))

  # Residuals Normal QQ Plot
  plots[["res_qq"]] <- plotResQQ(mod = mod, mName, xLab = mName)

  # Residuals vs Leverage Plot
  plots[["res_leverage"]] <- plotResLeverage(mod = mod, mName = mName)

  # Cooks Distance
  plots[["cooks"]] <- plotCooks(mod = mod, mName = mName)

  #---------------------------------------------------------------------------#
  #                           Assumptions Tests                               #
  #---------------------------------------------------------------------------#
  tests <- list()
  # Normality Test (nt)
  tests[["normal_res"]] <- shapiro.test(res)

  # Equal Variance Test (Levene's assumes Normality)
  tests[["equal_variance"]] <- car::ncvTest(mod)

  # Multi-collinearity if greater than 1 variable
  if (length(mod$model) > 2) {
    tests[["collinearity"]] <- car::vif(mod)
  }

  # Correlation Test (ct) if more than 1 factor and all numeric
  classes <- lapply(mod$model, class)
  if ((isTRUE(grepl("numeric", classes))) & length(mod$model) > 2) {
    tests[["correlation"]] <- psych::corr.test(mod$model)
  }

  analysis <- list(
    mod = mod,
    overview = overview,
    plots = plots,
    tests = tests
  )

  return(analysis)
}
