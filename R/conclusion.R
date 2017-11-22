#==============================================================================#
#                                Conclusion                                    #
#==============================================================================#
#' conclusion
#'
#' \code{conclusion} Prepares summary data for conclusion section
#'
#' @param slr The model object for the simple linear regresssion
#' @param mlr The model object for the multiregression
#' @param deta <- Data frame containing training data
#'
#' @return List containing data frames of summary statistics
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
conclusion <- function(slr, mlr, data) {

  analysis <- list()
  # Extract slr slope for log imdb num votes
  slrEst <- slr$coefficients$estimate

  # Extract genres in order of influence
  genres <- mlr$coefficients %>% filter(grepl("genre", term)) %>% select(term, estimate)
  aa <- data.frame(term = 'Action and Adventure',
                   estimate = 0)
  genres <- rbind(aa, genres)
  genres$term <- gsub('genre', "", genres$term)
  genres <- genres %>%  arrange(desc(estimate))
  genres <- genres %>% mutate(logVotes = mlr$coefficients[1,2] + estimate,
                              logBoxOffice = slrEst[1] + logVotes * slrEst[2],
                              boxOffice = 2^logBoxOffice)
  genres <- genres %>% mutate(pctInc = (boxOffice - min(boxOffice)) /
                                 min(boxOffice) * 100)
  genres <- genres %>% select(term, estimate, logVotes, logBoxOffice, pctInc)
  colnames(genres) <- c("Term", "Estimate", "Log IMDB Votes (Est)",
                        "Log Daily Box Office (Est)",  "% Increase Daily Box Office")

  # Cast Scores
  s <- summary(data$cast_scores)
  castScores <- rbindlist(lapply(seq_along(s), function(x) {
    cs <- list()
    cs[['qty']] <- names(s[x])
    cs[['scores']] <- s[x]
    cs[['logVotes']] <- mlr$coefficients[1,2] + s[x] * mlr$coefficients[2,2]
    cs[['logBoxOffice']] <- slrEst[1] + cs$logVotes * slrEst[2]
    cs[['boxOffice']] <- 2^cs$logBoxOffice
    cs
  }))
  castScores <- castScores %>% arrange(desc(scores))
  castScores <- castScores %>% mutate(boxOfficeTtlPct = (boxOffice - min(boxOffice))
                                      / min(boxOffice) * 100,
                                      scoresTtlPct = (scores - min(scores))
                                      / min(scores) * 100)

  castScores <- castScores %>% mutate(pct = (boxOffice - mean(boxOffice)) / mean(boxOffice) * 100)
  castScores <- castScores %>% select(qty, scores, logVotes, logBoxOffice, boxOfficeTtlPct, scoresTtlPct)
  colnames(castScores) <- c('Qty', 'Cast Scores', 'Log Votes' , 'Log Daily Box Office (est)',
                            '% Increase Daily Box Office', '% Increase Scores')

  analysis[['genres']] <- genres
  analysis[['castScores']] <- castScores

  return(analysis)

}
