#==============================================================================#
#                           comparePredictions                                 #
#==============================================================================#
#' comparePredictions
#'
#' \code{comparePredictions} Performs predictions on the list of regression
#' models and returns the summary statistics
#'
#' @param mods List containing linear models used to perform predictions
#' @param test Data frame containing the movies data set
#'
#' @return Data frame with regression summary statistics for each model
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
comparePredictions <- function(mods, test) {

  # Perform comparative analysis
  analysis <- data.table::rbindlist(lapply(mods, function(m) {
    p <- predict(m$mod, test, se.fit = TRUE, scale = NULL, df = Inf,
                 interval = "prediction", level = 0.95)
    d <- data.frame(Title = test$title,
                    Y = test$imdb_num_votes_log,
                    Predicted =  p$fit[,1],
                    Lower = p$fit[,2],
                    Upper = p$fit[,3],
                    SE = p$se.fit,
                    E = test$imdb_num_votes_log - p$fit[,1],
                    PE = (test$imdb_num_votes_log - p$fit[,1]) / test$imdb_num_votes_log * 100,
                    AE = abs(p$fit[,1] - test$imdb_num_votes_log),
                    APE = abs(p$fit[,1] - test$imdb_num_votes_log) /
                      test$imdb_num_votes_log * 100,
                    SQRE = (test$imdb_num_votes_log - p$fit[,1])^2,
                    LnQ = log(p$fit[,1] / test$imdb_num_votes_log))
    a <- data.frame(MAPE = mean(d$APE),
                    MPE = mean(d$PE),
                    MSE = mean(d$SQRE),
                    RMSE =  sqrt(mean(d$SQRE)))
    pa <- data.frame(x = (nrow(test %>% filter(imdb_num_votes_log > p$fit[,2] & imdb_num_votes_log < p$fit[,3])) /
                            nrow(test)) * 100)
    colnames(pa) <- c("% Accuracy")
    r <- cbind(m$glance,a, pa)
    r[, c(1,2,5,8,9,11,12,13,14,15, 16)]
  }))

  return(analysis)
}
