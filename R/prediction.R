#==============================================================================#
#                                 prediction                                   #
#==============================================================================#
#' prediction
#'
#' \code{prediction} Performs prediction on an randomly sampled individual observation
#'
#' @param mod Linear regression model
#' @param mName Character string containing the model name
#' @param case Data frame containing single case to predict
#' @param unit Character string indicating the unit for the value being predicted (w/o log)
#' @param conf Numeric indicator (0 to 1) of the confidence level for the prediction interval.
#' @param actual Logical indicating whether the dependent variable is actual or predicted.
#'
#' @return
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
prediction <- function(mod, mName, case, unit, conf = .90, actual = TRUE) {

  # Get summmary statistics for model
  s <- broom::glance(mod)
  a <- broom::tidy(anova(mod))

  # Extract name of response variable
  yVar <- mod$terms[[2]]

  # Set predictor variable
  if (nrow(a) == 2) {
    predictor <- a$term[1]
  } else {
    predictor <- paste0(nrow(a) - 1, " predictors")
  }

  # Perform prediction and summarize results
  p <- predict(mod, case, se.fit = TRUE, scale = NULL, df = Inf,
               interval = "prediction", level = conf)

  # Compute error figures in log and linear scales
  y <- case[,(colnames(case) == yVar)]
  yLin <- 2^y

  # Set indicator if actual was within 95% CI of prediction
  if (y > p$fit[2] & y < p$fit[3]) {
    ci <- "within"
  } else {
    ci <- "outside"
  }

  # Compute prediction on linear scale
  yHat <- p$fit[1]
  yHatLin <- 2^yHat

  # Compute Errors
  e <-  y - yHat
  eLin <- yLin - yHatLin

  # Compute percent errors
  pe <- abs(e / y * 100)
  peLin <- abs(yLin - yHatLin) / yLin * 100

  overUnder <- ifelse(e > 0, "under", "over")

  comment <- paste0(mName, " regression model (F=(", s$df, ",", s$df.residual,
                    ") = ", round(s$statistic, 2), " p-value < ",
                    ifelse(s$p.value < .001, ".001",
                           ifelse(s$p.value < .01, ".01",
                                  ifelse(s$p.value < .05, ".05"))),
                    ") was used to predict the ", yVar,  " of the film '",
                    case$title, "' based upon ", predictor,
                    ". The actual value of ", yVar, " was ", round(y, 2),
                    " log ", unit, ". ", "The model predicted ", round(yHat, 2), " log ",
                    unit, ", ", ci, " the 95% prediction interval CI[",
                    round(p$fit[2], 2), ",", round(p$fit[3], 2),"] log ", unit,
                    ", equating to an ", overUnder, " prediction of ",
                    round(pe, 2), "%. ")

  analysis <- list()
  analysis[["y"]] <- y
  analysis[["yLin"]] <- yLin
  analysis[["yHat"]] <- yHat
  analysis[["yHatLin"]] <- yHatLin
  analysis[["e"]] <- e
  analysis[["eLin"]] <- eLin
  analysis[["pe"]] <- pe
  analysis[["peLin"]] <- peLin
  analysis[["comment"]] <- comment

  return(analysis)
}
