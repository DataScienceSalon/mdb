#==============================================================================#
#                                 prediction                                   #
#==============================================================================#
#' prediction
#'
#' \code{prediction} Performs prediction on an randomly sampled individual observation
#'
#' @param mod Linear regression model
#' @param mName Character string containing the model name
#' @param cases Data frame containing cases to predict
#' @param conf Numeric indicator of desired confidence
#'
#' @return Data frame with summary of predictions
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
prediction <- function(mod, cases, conf = 0.95) {

  # Extract name of response variable
  yVar <- mod$terms[[2]]
  yVarActual <- paste0(yVar, "_actual")

  # Perform prediction
  p <- predict(mod, cases, se.fit = TRUE, scale = NULL, df = Inf,
               interval = "prediction", level = conf)

  # Summarize
  predictions <- data.frame(title = cases$title)
  predictions <- predictions %>%
    mutate(actual = cases[,(colnames(cases) == yVarActual)],
           pred = p$fit[,1],
           lower = p$fit[,2],
           upper = p$fit[,3],
           interval = ifelse(actual > lower & actual < upper, "Yes", "No"),
           e = actual - pred,
           pe = (actual - pred) / actual * 100,
           se = e^2)

  analysis <- data.frame(Variable = as.character(yVar),
                         MAPE = mean(abs(predictions$pe)),
                         MSE = mean(predictions$se),
                         RMSE = sqrt(mean(predictions$se)),
                         pct = nrow(subset(predictions, interval == "Yes")) /
                           nrow(predictions) * 100)

  colnames(predictions) <- c("Title", paste('Actual', yVar), paste('Predicted', yVar),
                          'Lower', 'Upper', 'Interval', 'Error',
                          '% Error', 'Sq. Error')

   result <- list(
     predictions = predictions,
     analysis = analysis
   )

  return(result)
}
