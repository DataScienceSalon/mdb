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
#' @param full Logical indicating whether a full analysis should be conducted
#'
#' @return analysis plots, summary statistics, test results and interpretive text
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
regressionAnalysis <- function(mod, mName, yVar, yLab, full = TRUE) {

  #---------------------------------------------------------------------------#
  #                                 Summary                                   #
  #---------------------------------------------------------------------------#
  a <- broom::tidy(anova(mod$model))
  g <- broom::glance(mod$model)
  s <- summary(mod$model)
  a <- a %>% mutate(p = round(sumsq / sum(sumsq) * 100, 2))

  # Format model summary at a glance
  g$name <- mName
  g$ve <- (1 - (a$sumsq[nrow(a)] / sum(a$sumsq))) * 100
  g$size <- nrow(a) - 1
  g$rmse <- sqrt(a$meansq[nrow(a)])
  g <- g %>% select(name, size, df, df.residual, statistic, rmse, sigma, r.squared, adj.r.squared, p.value, ve)

  # Pretty for anova and glance
  colnames(a) <- c("Term", "Df", "Sum Sq", "Mean Sq", "F Statistic", "Pr(>F)", "% Var")

  colnames(g) <- c("Model", "Size", "df", "df Residuals", "F Statistic", "RMSE", "Residual SE", "R-Squared",
                   "Adj R-Squared", "p-value", "% Variance")

  analysis <- list()
  analysis[["name"]] <- mName
  analysis[["mod"]] <- mod$model
  analysis[["build"]] <- mod$build
  analysis[["anova"]] <- a
  analysis[["coefficients"]] <- broom::tidy(mod$model)
  analysis[["glance"]] <- g
  analysis[["summary"]] <- s
  mod <- mod$model

  if (full == TRUE) {

    #---------------------------------------------------------------------------#
    #                             Format Data                                   #
    #---------------------------------------------------------------------------#
    # Residuals vs Fit
    rvf <- data.frame(Residuals = mod$residuals, Fitted = mod$fitted.values)
    # Residuals vs Predictor
    res <- mod$residuals


    #---------------------------------------------------------------------------#
    #                                  Plots                                    #
    #---------------------------------------------------------------------------#
    plots <- list()
    # Linear Regression Plot
    plots[["regression"]] <- plotScatter(data = mod$model, xLab = mName, yLab = yLab,
                                         plotTitle = paste0(mName, ": Linear Regression"))

    plots[["linearity"]] <- plotLinear(mod = mod, yVar = yVar,
                                       yLab = yLab)

    plots[["multicollinearity"]] <- plotCorr(mod = mod, yVar = yVar)

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

    analysis[["plots"]] <- plots

    #---------------------------------------------------------------------------#
    #                           Assumptions Tests                               #
    #---------------------------------------------------------------------------#
    tests <- list()
    # Linearity Tests
    coefs <- names(coef(mod))
    tests[["linearity"]] <- car::linearHypothesis(mod, coefs, singular.ok = TRUE)

    # Normality Test (nt)
    tests[["normal_res"]] <- shapiro.test(res)

    # Equal Variance Test (Levene's assumes Normality)
    tests[["homoscedasticity"]] <- car::ncvTest(mod)

    # Multi-collinearity if greater than 1 variable
    if (g$Size > 1) {
      tests[["collinearity"]] <- car::vif(mod)
    }

    # Correlation Test (ct) if more than 1 factor and all numeric
    classes <- lapply(mod$model, class)
    if ((isTRUE(grepl("numeric", classes))) & length(mod$model) > 2) {
      tests[["correlation"]] <- psych::corr.test(mod$model)
    }

    # Influential Points
    cd <- stats::cooks.distance(mod)
    tests[["influential"]] <- as.numeric(names(cd)[(cd > 4 * mean(cd, na.rm=T))])

    analysis[["tests"]] <- tests

    #---------------------------------------------------------------------------#
    #                             Format Writeup                                #
    #---------------------------------------------------------------------------#

    comments <- list()

    comments[["apa"]] <- paste0("the model was significant (F(", s$df[1], ", ", s$df[2], ")",
                                    " = ", round(s$fstatistic[1], 3), ", ", "p < ",
                                    ifelse(a$`Pr(>F)`[1] < .001, ".001",
                                           ifelse(a$`Pr(>F)`[1] < .01, ".01",
                                                  ifelse(a$`Pr(>F)`[1] < .05, ".05", round(a$`Pr(>F)`[1], 3)))),
                                    "), with an adjusted R-squared of ", round(s$adj.r.squared, 3),
                                    ". ")

    phrase0 <- c("The effect of ", "The influence of ", "The significance of ", "The force of ")
    phrase1 <- c(" yielded an F statistic of F(", " presented an F statistic of F(",
                 " indicated an F statistic of F(", " produced an F statistic of F(")
    phrase2 <- c(" representing ", " accounting for ", " expressing ", " exhibiting ")
    phrase3 <- c(" represented ", " accounted for ", " expressed ", " exhibited ")
    phrase4 <- c("some ", "approximately ", "a ")
    intro <- paste0("A two-way analysis of variance was conducted on the influence ",
                    "of ", (nrow(a)-1), " independent ", ifelse(nrow(a)-1 == 1, "variable", "variables"),
                    " on the ", tolower(yLab), ". ")
    close <- paste0("The model was significant (F(", s$df[1], ", ", s$df[2], ")",
                    " = ", round(s$fstatistic[1], 3), ", ", "p < ",
                    ifelse(a$`Pr(>F)`[1] < .001, ".001",
                           ifelse(a$`Pr(>F)`[1] < .01, ".01",
                                  ifelse(a$`Pr(>F)`[1] < .05, ".05", round(a$`Pr(>F)`[1], 3)))),
                    "), with an adjusted R-squared of ", round(s$adj.r.squared, 3),
                    ". ")
    anovaSentences <- lapply(seq_along(a$Term), function(x) {
      if (x != nrow(a)) {
        paste0(sample(phrase0, 1), a$Term[x], " on the ", tolower(yLab), sample(phrase1, 1), a$Df[x],
               ", ", a$Df[nrow(a)], "), = ", round(a$`F Statistic`[x], 3), ", p < ",
               ifelse(a$`Pr(>F)`[x] < .001, ".001",
                      ifelse(a$`Pr(>F)`[x] < .01, ".01",
                             ifelse(a$`Pr(>F)`[x] < .05,
                                    ".05", round(a$`Pr(>F)`[x], 3)))), ",", sample(phrase2, 1),
                                    a$`% Var`[x], "% of the variance.  ")
      } else {
        paste0("Finally, residuals", sample(phrase3, 1),
               sample(phrase4, 1), a$`% Var`[x], "% of variance.  ")
      }
    })

    anovaSentences <- paste(anovaSentences, collapse = "")

    comments[["anova"]] <- paste(intro, anovaSentences, close, collapse = "")

    if (tests$linearity$`Pr(>F)`[2] < 0.05) {
      comments[["linearity"]] <- paste0("A review of the partial scatterplots indicated ",
                                        "that linearity was a reasonable assumption for ",
                                        "this model (despite the presence of several ",
                                        "influential points).  A linear hypothesis test was conducted to ",
                                        "test the linearity assumption.  The results ",
                                        "were significant (F(", tests$linearity$Df[2],
                                        "), p < ",
                                        ifelse(tests$linearity$`Pr(>F)`[2] < .001, ".001",
                                               ifelse(tests$linearity$`Pr(>F)`[2] < .01, ".01",
                                                      ifelse(tests$linearity$`Pr(>F)`[2] < .05,
                                                             ".05", round(tests$linearity$`Pr(>F)`[2], 3)))),
                                        ").  As such, the linearity assumption was met in this case. ")
    } else {
      comments[["linearity"]] <- paste0("A review of the partial scatterplots indicated ",
                                        "that linearity may be a concern for this model. ",
                                        "A linear hypothesis test was conducted to ",
                                        "test the linearity assumption.  The results ",
                                        "were not significant (F(", tests$linearity$Df[2],
                                        "), p < ",
                                        ifelse(tests$linearity$`Pr(>F)`[2] < .001, ".001",
                                               ifelse(tests$linearity$`Pr(>F)`[2] < .01, ".01",
                                                      ifelse(tests$linearity$`Pr(>F)`[2] < .05,
                                                             ".05", round(tests$linearity$`Pr(>F)`[2], 3)))),
                                        ").  As such the linearity assumption was not met in this case.  ")
    }

    if (tests$homoscedasticity$p  < 0.05) {
      comments[["homoscedasticity"]] <- paste0("The residuals plot above indicated equal dispersion  ",
                                               "of residuals about zero mean. ",
                                               "A Breusch–Pagan test test was conducted to ",
                                                "test the homoscedasticity assumption.  The results ",
                                                "were significant (F(", tests$homoscedasticity$Df,
                                                "), p < ",
                                                ifelse(tests$homoscedasticity$p < .001, ".001",
                                                       ifelse(tests$homoscedasticity$p < .01, ".01",
                                                              ifelse(tests$homoscedasticity$p < .05,
                                                                     ".05", round(tests$homoscedasticity$p, 3)))),
                                                ").  As such, the homoscedasticity assumption was met in this case. ")
    } else {
      comments[["homoscedasticity"]] <- paste0("An examination of the residuals plot revealed unequal ",
                                               "disperson of residuals about the mean. ",
                                               "A Breusch–Pagan test was conducted to ",
                                               "test the homoscedasticity assumption.  The results ",
                                               "were not significant (F(", tests$homoscedasticity$Df[2],
                                               "), p < ",
                                               ifelse(tests$homoscedasticity$p < .001, ".001",
                                                      ifelse(tests$homoscedasticity$p < .01, ".01",
                                                             ifelse(tests$homoscedasticity$p < .05,
                                                                    ".05", round(tests$homoscedasticity$p, 3)))),
                                               ").  As such the homoscedasticity assumption was not met in this case.  ")
    }

    if (tests$normal_res$p.value  < 0.05) {
      comments[["normality"]] <- paste0("The histogram and normal Q-Q plot suggested a nearly normal ",
                                        "distribution of residuals.  A review of the Shapiro-Wilk  ",
                                        "test (SW = ", round(tests$normal_res$statistic, 3), ", p = ",
                                        round(tests$normal_res$p.value,3),
                                        ") and the skewness (",round(moments::skewness(res), 3), ") and ",
                                        "kurtosis (", round(moments::kurtosis(res), 3), ") supported ",
                                        "the assumption of normaility.  ")
    } else {
      comments[["normality"]] <- paste0("The histogram and normal Q-Q plot did not suggest a normal ",
                                        "distribution of residuals.  A review of the Shapiro-Wilk  ",
                                        "test (SW = ", round(tests$normal_res$statistic, 3), ", p = ",
                                        round(tests$normal_res$p.value, 3),
                                        ") and the skewness (", round(moments::skewness(res), 3), ") and ",
                                        "kurtosis (", round(moments::kurtosis(res),3), ") indicated that ",
                                        "normality of residuals was not a reasonable assumption ",
                                        "for this model. ")
    }

    if (g$Size > 1) {

      if (max(tests$collinearity[,1]) < 4) {
        comments[["collinearity"]] <- paste0("collinearity did not appear extant for this model.  ",
                                             "Variance inflation factors were computed for each predictor in ",
                                             " the model.  The maximum VIF of ", round(max(tests$collinearity[,1]),0),
                                             " did not exceed the threshold of 4. As such, the absense of ",
                                             "multicollinearity was assumed for this model.  ")
      } else {
        comments[["collinearity"]] <- paste0("collinearity appeared extant for this moiiidel.  ",
                                             "Variance inflation factors were computed for each predictor in ",
                                             " the model.  The maximum VIF of ", round(max(tests$collinearity[,1]),0),
                                             " exceeded the threshold of 4. As such, the correlation among the  ",
                                             "predictors would require further consideration.  ",
                                             "The multicollinearity assumption was not met for this model.  ")
      }
    }

    if (length(tests$influential) > 0) {
      comments[["outliers"]] <- paste0("Examination of the residuals versus leverage plot and case-wise ",
                                       "diagnostics such as Cook's distance revealed ", length(tests$influential),
                                       " cases exerting undue influence on the model.  ")
    } else {
      comments[["outliers"]] <- paste0("Examination of the residuals versus leverage plot and case-wise ",
                                       "diagnostics such as Cook's distance revealed no ",
                                       "cases exerting undue influence on the model.  ")
    }



    analysis[["comments"]] <- comments
  }
  return(analysis)
}
