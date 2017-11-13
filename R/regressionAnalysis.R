#==============================================================================#
#                             regressionAnalysis                               #
#==============================================================================#
#' regressionAnalysis
#'
#' \code{regressionAnalysis} Performs regression analysis
#'
#' @param mod Linear model
#' @param xLab Capitalized character string for the categorical variable
#' @param yLab Capitalized character string for the quantitative variable
#'
#' @return analysis plots, summary statistics, test results and interpretive text
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
regressionAnalysis <- function(mod, xLab, yLab) {

  #---------------------------------------------------------------------------#
  #                             Format Data                                   #
  #---------------------------------------------------------------------------#
  # Residuals vs Fit
  rvf <- data.frame(Fitted = mod$fitted.values, Residuals = mod$residuals)
  # Residuals vs Predictor
  res <- mod$residuals

  #---------------------------------------------------------------------------#
  #                                  Plots                                    #
  #---------------------------------------------------------------------------#
  plots <- list()
  # Box plot
  plots[["boxplot"]] <- plotBox(data = mod$model, xLab = xLab, yLab = yLab)

    # Linear Regression Plot
  plots[["regression"]] <- plotScatter(data = mod$model, xLab = xLab, yLab = yLab, title = "Linear Regression")

  # Residuals vs Fitted
  plots[["res_fitted"]] <- plotScatter(data = rvf, xLab = xLab, yLab = paste("Residuals:", yLab),
                                    title = paste("Residuals vs. Fitted:", yLab, "by", xLab))
  # Residuals vs Predictor
  plots[["res_predictors"]] <- plotResAll(mod = mod)

  # Residuals Histogram
  plots[["res_hist"]] <- plotHist(data = as.data.frame(res), yLab = paste("Residuals:", yLab),
                      title = paste("Distribution of Residuals:", yLab))

  # Residuals Normal QQ Plot
  plots[["res_qq"]] <- plotResQQ(mod = mod, yLab = yLab)

  # Residuals vs Leverage Plot
  plots[["res_leverage"]] <- plotResLeverage(mod = mod, yLab = yLab)

  # Cooks Distance
  plots[["cooks"]] <- plotCooks(mod = mod, yLab = yLab)

  # Correlation Plot
  classes <- lapply(mod$model, class)
  if ((isTRUE(grepl("numeric", classes))) & length(mod$model) > 1) {
    mtx <- cor(mod$model)
    col <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
    plots[["correlation"]] <- corrplot::corrplot(mtx, method="color", col=col(200),
             type="upper", order="hclust",
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt=45, #Text label color and rotation
             # Combine with significance
             #p.mat = p.mat, sig.level = 0.01, insig = "blank",
             # hide correlation coefficient on the principal diagonal
             diag=FALSE
    )
  }

  #---------------------------------------------------------------------------#
  #                           Assumptions Tests                               #
  #---------------------------------------------------------------------------#
  tests <- list()
  # Normality Test (nt)
  tests[["normal_res"]] <- shapiro.test(res)

  # Equal Variance Test (Levene's assumes Normality)
  tests[["eq_var_res1"]] <- leveneTest(Residuals ~ Predictor, data = rvp)

  # Equal Variance Test (Bartlett Test)
  tests[["eq_var_res2"]] <- bartlett.test(Residuals ~ Predictor, data = rvp)

  # Multi-collinearity if greater than 1 variable
  if (length(mod$model) > 2) {
    tests[["collinearity"]] <- vif(mod)
  }

  # Correlation Test (ct) if more than 1 factor and all numeric
  classes <- lapply(mod$model, class)
  if ((isTRUE(grepl("numeric", classes))) & length(mod$model) > 2) {
    tests[["correlation"]] <- psych::corr.test(mod$model)
  }

  analysis <- list(
    mod = mod,
    plots = plots,
    tests = tests
  )

  return(analysis)
}
