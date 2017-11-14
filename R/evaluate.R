#==============================================================================#
#                                 evaluate                                    #
#==============================================================================#
#' evaluate
#'
#' \code{evaluate} Performs evaluate analysis of variables
#'
#' @param data List containing two data frames, mdb, the main movie data set and mdbBox the sample with box office revenue
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
evaluate <- function(data) {

  #---------------------------------------------------------------------------#
  #                       Box Office Revenue Analysis                         #
  #---------------------------------------------------------------------------#
  # Director Experience
  df <- data %>% select(imdb_num_votes_log, director_experience)
  mod <- lm(imdb_num_votes_log ~ director_experience, data = df)
  directorExperience <- regressionAnalysis(mod = mod, xLab = "Director Experience",
                                         yLab = "Log Votes x Scores")

  # Cast Experience
  df <- data %>% select(imdb_num_votes_log, cast_experience)
  mod <- lm(imdb_num_votes_log ~ cast_experience, data = df)
  castExperience <- regressionAnalysis(mod = mod, xLab = "Cast Experience",
                                           yLab = "Log Votes x Scores")

  # IMDB Rating
  df <- data %>% select(imdb_num_votes_log, imdb_rating)
  mod <- lm(imdb_num_votes_log ~ imdb_rating, data = df)
  imdbRating <- regressionAnalysis(mod = mod, xLab = "IMDB Rating",
                                  yLab = "Log Votes x Scores")

  # Critics Score
  df <- data %>% select(imdb_num_votes_log, critics_score)
  mod <- lm(imdb_num_votes_log ~ critics_score, data = df)
  criticsScore <- regressionAnalysis(mod = mod, xLab = "Critics Score",
                                   yLab = "Log Votes x Scores")

  # Audience Score
  df <- data %>% select(imdb_num_votes_log, audience_score)
  mod <- lm(imdb_num_votes_log ~ audience_score, data = df)
  audienceScore <- regressionAnalysis(mod = mod, xLab = "Audience Score",
                                   yLab = "Log Votes x Scores")

  # Cast Votes
  df <- data %>% select(imdb_num_votes_log, cast_votes)
  mod <- lm(imdb_num_votes_log ~ cast_votes, data = df)
  castVotes <- regressionAnalysis(mod = mod, xLab = "Cast Votes",
                                    yLab = "Log Votes x Scores")

  # Log Cast Votes
  df <- data %>% select(imdb_num_votes_log, cast_votes_log)
  mod <- lm(imdb_num_votes_log ~ cast_votes_log, data = df)
  castVotesLog <- regressionAnalysis(mod = mod, xLab = "Log Cast Votes",
                                       yLab = "Log Votes x Scores")

  # Scores
  df <- data %>% select(imdb_num_votes_log, scores)
  mod <- lm(imdb_num_votes_log ~ scores, data = df)
  scores <- regressionAnalysis(mod = mod, xLab = "Scores",
                                  yLab = "Log Votes x Scores")

  # Log Scores
  df <- data %>% select(imdb_num_votes_log, scores_log)
  mod <- lm(imdb_num_votes_log ~ scores_log, data = df)
  scoresLog <- regressionAnalysis(mod = mod, xLab = "Log Scores",
                                     yLab = "Log Votes x Scores")

  analysis <- list(
    part1 = list(
      directorExperience = directorExperience,
      castExperience = castExperience,
      imdbRating = imdbRating,
      criticsScore = criticsScore,
      audienceScore = audienceScore,
      castVotes = castVotes,
      castVotesLog = castVotesLog,
      scores = scores,
      scoresLog = scoresLog
    )
  )
  return(analysis)

}
