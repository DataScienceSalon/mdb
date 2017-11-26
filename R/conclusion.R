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
  #                   Prepare IMDB Num Votes Analysis                         #
  #---------------------------------------------------------------------------#
  d <- data.frame(logVotes = seq(1, 13.702, by = .001 ))
  votes <- d %>%
    mutate(logBoxOffice = slrEstIntercept + logVotes * slrEstLogVotes,
           votes = exp(logVotes),
           boxOffice = exp(logBoxOffice))

  # Calculate percent change
  pctChgLvLb <- slrEstLogVotes
  pctChgvb <- (1.01^slrEstLogVotes - 1) * 100

  # Extract Data
  lvlb <- votes %>% select(logVotes, logBoxOffice)
  vb <- votes %>% select(votes, boxOffice)

  # Render Plots
  lvlbp <- plotLine(data = lvlb, xLab = 'Log IMDB Votes', yLab = 'Log Box Office',
                   plotTitle = 'Log Box Office vs. Log IMDB Votes')
  vbp <- plotLine(data = vb, xLab = 'IMDB Votes', yLab = 'Box Office',
                  plotTitle = 'Box Office vs. IMDB Votes')

  votesAnalysis <- list()
  votesData <- list()
  plots <- list()
  pctChg <- list()
  votesData[['lvlb']] <- lvlb
  votesData[['vb']] <- vb
  pctChg[['lvlb']] <- pctChgLvLb
  pctChg[['vb']] <- pctChgvb
  plots <- list()
  plots[['lvbp']] <- lvlbp
  plots[['vbp']] <- vbp
  votesAnalysis[['data']] <- votesData
  votesAnalysis[['pctChg']] <- pctChg
  votesAnalysis[['plots']] <- plots




  #---------------------------------------------------------------------------#
  #                        Prepare Genre Analysis                             #
  #---------------------------------------------------------------------------#
  genres <- mlr$coefficients[grepl("genre", mlr$coefficients$term, fixed = TRUE), ]
  genres <- c(gsub('genre', "", genres$term), "Action & Adventure")
  genres <- data.frame(Genre = genres)

  genre <- genres %>% group_by(Genre) %>%
    summarize(estimate = coefs[coefs$term == Genre, 2],
              logVotes = mlrEstIntercept +
                coefs[coefs$term == Genre, 2] +
                mlrEstScores * muCastScores +
                mlrEstRuntime * muRunTimeLog +
                mlrEstMPAA,
              logBoxOffice = slrEstIntercept +
                logVotes * slrEstLogVotes,
              votes = exp(logVotes),
              boxOffice = exp(logBoxOffice),
              pctChgGenreVotes = (exp(estimate) - 1) * 100,
              pctChgVotesRev = ((1.01^slrEstLogVotes) - 1) * 100,
              pctChgGenreRev = (1 + pctChgGenreVotes)  * (1 + pctChgVotesRev) - 1)

  refVotes <- as.numeric(genre %>% filter(Genre == "Comedy") %>% select(votes))
  refBoxOffice <- as.numeric(genre %>% filter(Genre == "Comedy") %>% select(boxOffice))

  genre <- genre %>% mutate(pctChgGenreVotes = (votes - refVotes) / refVotes * 100)
  genre <- genre %>% mutate(pctChgGenreRev = (boxOffice - refBoxOffice) / refBoxOffice * 100)

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
  #                        Prepare Cast Score Analysis                        #
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
              votes = exp(logVotes),
              boxOffice = exp(logBoxOffice))

  # Calculate percent change
  pctChgCsVotes <- (exp(mlrEstScores) - 1) * 100
  pctChgVotesRev <- (1.01^slrEstLogVotes - 1) * 100
  pctChgCsRev <- (1 + pctChgCsVotes)  * (1 + pctChgVotesRev) - 1

  # Extract Data
  sv <- cast %>% select(scores, votes)
  sb <- cast %>% select(scores, boxOffice)

  # Render Plots
  svp <- plotLine(data = sv, xLab = 'Cast Scores ', yLab = 'IMDB Votes',
                  plotTitle = 'IMDB Votes vs. Cast Scores')

  sbp <- plotLine(data = sb, xLab = 'Cast Scores ', yLab = 'Box Office',
                  plotTitle = 'Box Office vs. Cast Scores')

  castAnalysis <- list()
  plots <- list()
  pctChg <- list()
  plots[['svp']] <- svp
  plots[['sbp']] <- sbp
  pctChg[['csVotes']] <- pctChgCsVotes
  pctChg[['votesRev']] <- pctChgVotesRev
  pctChg[['csRev']] <- pctChgCsRev
  castAnalysis[['data']] <- cast
  castAnalysis[['plots']] <- plots
  castAnalysis[['pctChg']] <- pctChg

  #---------------------------------------------------------------------------#
  #                        Prepare Run Time Analysis                          #
  #---------------------------------------------------------------------------#

  d <- data.frame(runTimeLog = seq(4, 5.6, by = .001))
  runtime <- d %>%
    mutate(logVotes = mlrEstIntercept +
             mlrEstGenre +
             mlrEstScores * muCastScores +
             mlrEstRuntime * runTimeLog +
             mlrEstMPAA,
           logBoxOffice = slrEstIntercept +
             logVotes * slrEstLogVotes,
           runtime = exp(runTimeLog),
           votes = exp(logVotes),
           boxOffice = exp(logBoxOffice))

  # Calculate percent change
  pctChgRtVotes <- (1.01^mlrEstRuntime - 1) * 100
  pctChgVotesRev <- (1.01^slrEstLogVotes - 1) * 100
  pctChgRtRev <- (1 + pctChgRtVotes)  * (1 + pctChgVotesRev) - 1


  # Extract Data
  rtv <- runtime %>% select(runtime, votes)
  sb <- runtime %>% select(runtime, boxOffice)

  # Render Plots
  rtvp <- plotLine(data = rtv, xLab = 'Run Time ', yLab = 'IMDB Votes',
                  plotTitle = 'IMDB Votes vs. Run Time')
  sbp <- plotLine(data = sb, xLab = 'Run Time ', yLab = 'Box Office',
                  plotTitle = 'Box Office vs. Run Time')

  runtimeAnalysis <- list()
  plots <- list()
  pctChg <- list()
  plots[['rtvp']] <- rtvp
  plots[['sbp']] <- sbp
  pctChg[['rtVotes']] <- pctChgRtVotes
  pctChg[['votesRev']] <- pctChgVotesRev
  pctChg[['rtRev']] <- pctChgRtRev
  runtimeAnalysis[['data']] <- runtime
  runtimeAnalysis[['plots']] <- plots
  runtimeAnalysis[['pctChg']] <- pctChg

  #---------------------------------------------------------------------------#
  #                    Prepare MPAA Rating Analysis                           #
  #---------------------------------------------------------------------------#
  mpaaCoef <- mlr$coefficients[grepl("mpaa", mlr$coefficients$term, fixed = TRUE), ]
  mpaaCoef <- c(gsub('mpaa_rating', "", mpaaCoef$term), "G")
  mpaaCoef <- data.frame(mpaa = mpaaCoef)

  mpaa <- mpaaCoef %>% group_by(mpaa) %>%
    summarize(estimate = coefs[coefs$term == mpaa, 2],
              logVotes = mlrEstIntercept +
                coefs[coefs$term == mpaa, 2] +
                mlrEstScores * muCastScores +
                mlrEstRuntime * muRunTimeLog +
                mlrEstGenre,
              logBoxOffice = slrEstIntercept +
                logVotes * slrEstLogVotes,
              votes = exp(logVotes),
              boxOffice = exp(logBoxOffice),
              pctChgMPAAVotes = (exp(estimate) - 1) * 100,
              pctChgVotesRev = ((1.01^slrEstLogVotes) - 1) * 100,
              pctChgMPAARev = (1 + pctChgMPAAVotes)  * (1 + pctChgVotesRev) - 1)

  # Extract Data
  mlv <- mpaa %>% select(mpaa, logVotes)
  mlb <- mpaa %>% select(mpaa, logBoxOffice)
  mv <- mpaa %>% select(mpaa, votes)
  mb <- mpaa %>% select(mpaa, boxOffice)

  # Render plots
  mlvp <- plotBar(data = mlv, xLab = 'MPAA Rating', yLab = 'Log IMDB Votes',
                    plotTitle = 'Log IMDB Votes vs. MPAA Rating')
  mlbp <- plotBar(data = mlb, xLab = 'MPAA Rating', yLab = 'Log Box Office',
                    plotTitle = 'Log Box Office vs. MPAA Rating')

  mvp <- plotBar(data = mv, xLab = 'MPAA Rating', yLab = 'IMDB Votes',
                 plotTitle = 'IMDB Votes vs. MPAA Rating')
  mbp <- plotBar(data = mb, xLab = 'MPAA Rating', yLab = 'Box Office',
                 plotTitle = 'Box Office vs. MPAA Rating')
  mpaaAnalysis <- list()
  plots <- list()
  plots[['mlvp']] <- mlvp
  plots[['mlbp']] <- mlbp
  plots[['mvp']] <- mvp
  plots[['mbp']] <- mbp
  mpaaAnalysis[['data']] <- mpaa
  mpaaAnalysis[['plots']] <- plots


  analysis <- list()
  analysis[['votes']] <- votesAnalysis
  analysis[['genre']] <- genreAnalysis
  analysis[['cast']] <- castAnalysis
  analysis[['runtime']] <- runtimeAnalysis
  analysis[['mpaa']] <- mpaaAnalysis


  return(analysis)

}
