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
#' @param units Character string indicating the unit for the value being predicted (w/o log)
#' @param conf Numeric indicator (0 to 1) of the confidence level for the prediction interval.
#'
#' @return
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
prediction <- function(mod, mName, case, units, conf = .90) {

  # Get summmary statistics for model
  s <- broom::glance(mod)

  # Extract name of response variable
  yVar <- mod$terms[[2]]

  # Perform prediction and summarize results
  p <- predict(mod, case, se.fit = TRUE, scale = NULL, df = Inf,
               interval = "prediction", level = conf)

  # Compute error figures in log and linear scales
  y <- case[,(colnames(case) == yVar)]
  yLin <- 2^y

  yHat <- p$fit[1]
  yHatLin <- 2^yHat

  e <-  y - yHat
  eLin <- yLin - yHatLin

  pe <- abs(e / y * 100)
  peLin <- abs(yLin - yHatLin) / yLin * 100

  overUnder <- ifelse(e > 0, "under", "over")

  comment <- paste0(mName, " regression model (F=(", s$df, ",", s$df.residual,
                    ") = ", round(s$statistic, 2), " p-value < ",
                    ifelse(s$p.value < .001, ".001",
                           ifelse(s$p.value < .01, ".01",
                                  ifelse(s$p.value < .05, ".05"))),
                    ") was used to predict the ", yVar,  " of the film '",
                    case$title, "'. The actual value was ", round(y, 2), " log ",
                    units, " or ", round(yLin, 2), " ", units, " on the linear scale. ",
                    "The model predicted ", round(yHat, 2), " log ", units,
                    " with a 95% prediction interval CI[",
                    round(p$fit[2], 2), ",", round(p$fit[3], 2),"] log ", units,
                    ", equating to a prediction of ", round(yHatLin, 0), " ", units,
                    " with a range from ", round(2^p$fit[2], 0), " to ",
                    round(2^p$fit[3], 0)," ", units, ". The model ", overUnder,
                    " predicted by ", round(peLin, 2), "% or ",
                    abs(round(eLin, 0)), " ", units, " on the linear scale. ")

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
