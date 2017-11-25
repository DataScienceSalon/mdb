#==============================================================================#
#                                Conclusion                                    #
#==============================================================================#
#' conclusion
#'
#' \code{conclusion} Prepares summary data for conclusion section
#'
#' @param slr The model object for the simple linear regresssion
#' @param mlr The model object for the multiregression
#' @param data Data frame containing training data
#'
#' @return List containing data frames of summary statistics
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
conclusion <- function(slr, mlr, data) {

  #---------------------------------------------------------------------------#
  #                        Obtain Parameter Estimates                         #
  #---------------------------------------------------------------------------#
  # Obtain MSE
  mseSlr <- slr$glance$RMSE^2
  mseMlr <- mlr$glance$RMSE^2

  # Extract means for numeric predictors
  muCastScores <- mean(train$cast_scores)
  muRunTimeLog <- mean(train$runtime_log)

  # Reformat coefficients matrix
  coefs <- mlr$coefficients %>% select(term, estimate)
  refCats <- c("Action & Adventure", "Jan", "G")
  refCats <- data.frame(term = refCats,
                        estimate = 0)
  coefs <- rbind(coefs, refCats)
  coefs$term <- gsub('genre', "", coefs$term)
  coefs$term <- gsub('thtr_rel_month', "", coefs$term)
  coefs$term <- gsub('mpaa_rating', "", coefs$term)

  # Obtain parameter estimates
  slrEstIntercept <- slr$coefficients$estimate[1]
  slrEstLogVotes <- slr$coefficients$estimate[2]
  mlrEstIntercept <- mlr$coefficients[1,2]
  mlrEstScores <- as.numeric(coefs %>% filter(term == "cast_scores") %>% select(estimate))
  mlrEstRuntime <- as.numeric(coefs %>% filter(term == "runtime_log") %>% select(estimate))
  mlrEstMPAA <- as.numeric(coefs %>% filter(term == "R") %>% select(estimate))
  mlrEstMonth <- as.numeric(coefs %>% filter(term == "Oct") %>% select(estimate))
  mlrEstGenre <- as.numeric(coefs %>% filter(term == "Drama") %>% select(estimate))

  #---------------------------------------------------------------------------#
  #                        Prepare Genre Analysis                             #
  #---------------------------------------------------------------------------#
  genres <- mlr$coefficients[grepl("genre", mlr$coefficients$term, fixed = TRUE), ]
  genres <- c(gsub('genre', "", genres$term), "Action & Adventure")
  genres <- data.frame(Genre = genres)

  genre <- genres %>% group_by(Genre) %>%
    summarize(logVotes = mlrEstIntercept +
             coefs[coefs$term == Genre, 2] +
             mlrEstScores * muCastScores +
             mlrEstRuntime * muRunTimeLog +
             mlrEstMPAA,
           logBoxOffice = slrEstIntercept +
             logVotes * slrEstLogVotes,
           votes = exp(mean(logVotes + (1/2 * mseMlr))),
           boxOffice = exp(mean(logBoxOffice + (1/2 * mseMlr))))
  refVotes <- as.numeric(genre %>% filter(Genre == "Comedy") %>% select(votes))
  refBoxOffice <- as.numeric(genre %>% filter(Genre == "Comedy") %>% select(boxOffice))

  genre <- genre %>% mutate(pctChgVotes = (votes - refVotes) / refVotes * 100)
  genre <- genre %>% mutate(pctChgRevenue = (boxOffice - refBoxOffice) / refBoxOffice * 100)



  # Extract Data
  gvLog <- genre %>% select(Genre, logVotes)
  gbLog <- genre %>% select(Genre, logBoxOffice)
  gv <- genre %>% select(Genre, votes)
  gb <- genre %>% select(Genre, boxOffice)

  # Render plots
  gvLogp <- plotBar(data = gvLog, xLab = 'Genre', yLab = 'Log IMDB Votes',
                 plotTitle = 'Log IMDB Votes vs. Genre')
  gbLogp <- plotBar(data = gbLog, xLab = 'Genre', yLab = 'Log Box Office',
                 plotTitle = 'Log Box Office vs. Genre')

  gvp <- plotBar(data = gv, xLab = 'Genre', yLab = 'IMDB Votes',
                 plotTitle = 'IMDB Votes vs. Genre')
  gbp <- plotBar(data = gb, xLab = 'Genre', yLab = 'Box Office',
                 plotTitle = 'Box Office vs. Genre')
  genreAnalysis <- list()
  plots <- list()
  plots[['gvLogp']] <- gvLogp
  plots[['gbLogp']] <- gbLogp
  plots[['gvp']] <- gvp
  plots[['gbp']] <- gbp
  genreAnalysis[['data']] <- genre
  genreAnalysis[['plots']] <- plots


  #---------------------------------------------------------------------------#
  #                        Prepare Caat Score Analysis                        #
  #---------------------------------------------------------------------------#

  d <- data.frame(scores = seq(1, 1000, by = 10 ))
  cast <- d %>%
    mutate(logVotes = mlrEstIntercept +
                mlrEstGenre +
                mlrEstScores * scores +
                mlrEstRuntime * muRunTimeLog +
                mlrEstMPAA,
              logBoxOffice = slrEstIntercept +
                logVotes * slrEstLogVotes,
              votes = exp(mlrEstScores * scores + (1/2 * mlr$glance$RMSE^2)),
              boxOffice = exp(slrEstLogVotes * logVotes + (1/2 * slr$glance$RMSE^2)))

  # Calculate percent change
  pctChgVotes <- mlrEstScores * 100
  pctChgRevenue <-  slrEstLogVotes

  # Extract Data
  svLog <- cast %>% select(scores, logVotes)
  sbLog <- cast %>% select(scores, logBoxOffice)
  sv <- cast %>% select(scores, votes)
  sb <- cast %>% select(scores, boxOffice)

  # Render Plots
  svLogp <- plotLine(data = svLog, xLab = 'Cast Scores', yLab = 'Log IMDB Votes',
                  plotTitle = 'Log IMDB Votes vs. Cast Scores')
  sbLogp <- plotLine(data = sbLog, xLab = 'Cast Scores ', yLab = 'Log Box Office',
                  plotTitle = 'Log Box Office vs. Cast Scores')

  svp <- plotLine(data = sv, xLab = 'Cast Scores', yLab = 'IMDB Votes',
                 plotTitle = 'IMDB Votes vs. Cast Scores')
  sbp <- plotLine(data = sb, xLab = 'Cast Scores ', yLab = 'Box Office',
                  plotTitle = 'Box Office vs. Cast Scores')

  castAnalysis <- list()
  plots <- list()
  pctChg <- list()
  plots[['svLogp']] <- svLogp
  plots[['sbLogp']] <- sbLogp
  plots[['svp']] <- svp
  plots[['sbp']] <- sbp
  pctChg[['votes']] <- pctChgVotes
  pctChg[['revenue']] <- pctChgRevenue
  castAnalysis[['data']] <- cast
  castAnalysis[['plots']] <- plots
  castAnalysis[['pctChg']] <- pctChg

  #---------------------------------------------------------------------------#
  #                        Prepare Run Time Analysis                          #
  #---------------------------------------------------------------------------#

  d <- data.frame(runTimeLog = seq(4, 5, by = .001))
  runtime <- d %>%
    mutate(logVotes = mlrEstIntercept +
             mlrEstGenre +
             mlrEstScores * muCastScores +
             mlrEstRuntime * runTimeLog +
             mlrEstMPAA,
           logBoxOffice = slrEstIntercept +
             logVotes * slrEstLogVotes,
           runtime = exp(runTimeLog),
           votes = exp(mlrEstRuntime * runTimeLog + (1/2 * mlr$glance$RMSE^2)),
           boxOffice = exp(slrEstLogVotes * logVotes + (1/2 * slr$glance$RMSE^2)))

  # Calculate percent change
  pctChgVotes <- mlrEstRuntime
  pctChgRevenue <- slrEstLogVotes


  # Extract Data
  rtvLog <- runtime %>% select(runtime, logVotes)
  rtbLog <- runtime %>% select(runtime, logBoxOffice)
  rtv <- runtime %>% select(runtime, votes)
  rtb <- runtime %>% select(runtime, boxOffice)

  # Render Plots
  rtvLogp <- plotLine(data = rtvLog, xLab = 'Runtime', yLab = 'Log IMDB Votes',
                     plotTitle = 'Log IMDB Votes vs. Log Runtime')
  rtbLogp <- plotLine(data = rtbLog, xLab = 'Runtime', yLab = 'Log Box Office',
                     plotTitle = 'Log Box Office vs. Log Runtime')

  rtvp <- plotLine(data = rtv, xLab = 'Runtime', yLab = 'IMDB Votes',
                  plotTitle = 'IMDB Votes vs. Runtime')
  rtbp <- plotLine(data = rtb, xLab = 'Runtime', yLab = 'Box Office',
                  plotTitle = 'Box Office vs. Runtime')

  runtimeAnalysis <- list()
  plots <- list()
  pctChg <- list()
  plots[['rtvLogp']] <- rtvLogp
  plots[['rtbLogp']] <- rtbLogp
  plots[['rtvp']] <- rtvp
  plots[['rtbp']] <- rtbp
  pctChg[['votes']] <- pctChgVotes
  pctChg[['revenue']] <- pctChgRevenue
  runtimeAnalysis[['data']] <- runtime
  runtimeAnalysis[['plots']] <- plots
  runtimeAnalysis[['pctChg']] <- pctChg

  analysis <- list()
  analysis[['genre']] <- genreAnalysis
  analysis[['cast']] <- castAnalysis
  analysis[['runtime']] <- runtimeAnalysis


  return(analysis)

}
