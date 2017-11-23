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
  gv <- genres %>% select(term, logVotes)
  gb <- genres %>% select(term, boxOffice)

  gvp <- plotBar(data = gv, xLab = 'Genre', yLab = 'Log IMDB Votes',
                 plotTitle = 'Genre vs Log IMDB Votes')
  gbp <- plotBar(data = gb, xLab = 'Genre', yLab = 'Daily Box Office',
                 plotTitle = 'Genre vs Daily Box Office')
  genre <- list()
  data <- list()
  plots <- list()
  data[['genreVotes']] <- gv
  data[['genreDBO']] <- gb
  plots[['genreVotes']] <- gvp
  plots[['genreDBO']] <- gbp
  genre[['data']] <- data
  genre[['plots']] <- plots


  # Simulate Cast Votes data and compute imdb votes
  csCoef <- as.numeric(subset(mlr$coefficients, term == "cast_scores", select = estimate))
  d <- data.frame(scores = exp(seq(1, 8, by = .001 )))
  d <- d %>% mutate(votes = mlr$coefficients$estimate[1] +
                    csCoef * scores)
  d <- d %>% mutate(dbol = slr$coefficients$estimate[1] +
                      slr$coefficients$estimate[2] * votes,
                    dbo = 2^dbol)
  # Extract Data
  sv <- d %>% select(scores, votes)
  sb <- d %>% select(scores, dbo)

  # Render Plots
  svp <- plotLine(data = sv, xLab = 'Cast Scores', yLab = 'Log IMDB Votes',
                 plotTitle = 'Cast Scores vs. Log IMDB Votes')
  sbp <- plotLine(data = sb, xLab = 'Cast Scores ', yLab = 'Daily Box Office',
                  plotTitle = 'Cast Scores vs. Daily Box Office')

  scores <- list()
  data <- list()
  plots <- list()
  data[['scoresVotes']] <- sv
  data[['scoresDBO']] <- sb
  plots[['scoresVotes']] <- svp
  plots[['scoresDBO']] <- sbp
  scores[['data']] <- data
  scores[['plots']] <- plots

  analysis <- list()
  analysis[['genre']] <- genre
  analysis[['scores']] <- scores

  return(analysis)

}
