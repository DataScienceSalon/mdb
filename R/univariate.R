#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @param mdb Data frame containing movie information
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
univariate <- function(mdb) {

  #---------------------------------------------------------------------------#
  #                       Conduct Qualitative Analysis                        #
  #---------------------------------------------------------------------------#
  genre <- univariateQual(as.data.frame(mdb$genre), xLab = "Genre")
  mpaa <- univariateQual(as.data.frame(mdb$mpaa_rating), xLab = "MPAA Rating")
  season <- univariateQual(as.data.frame(mdb$thtr_rel_season), xLab = "Season")
  month <- univariateQual(as.data.frame(mdb$thtr_rel_month), xLab = "Month")
  bestPicNom <- univariateQual(as.data.frame(mdb$best_pic_nom), xLab = "Best Picture Oscar Nomination")
  bestPicWin <- univariateQual(as.data.frame(mdb$best_pic_win), xLab = "Best Picture Oscar")
  bestDirWin <- univariateQual(as.data.frame(mdb$best_dir_win), xLab = "Best Director Oscar")
  bestActorWin <- univariateQual(as.data.frame(mdb$best_actor_win), xLab = "Best Actor Oscar")
  bestActressWin <- univariateQual(as.data.frame(mdb$best_actress_win), xLab = "Best Actress Oscar")
  top200Box <- univariateQual(as.data.frame(mdb$top200_box), xLab = "Top 200 Daily Box Office")

  #---------------------------------------------------------------------------#
  #                        Conduct Quantative Analysis                        #
  #---------------------------------------------------------------------------#
  directorExperience <- univariateQuant(data.frame(mdb$title, mdb$director_experience),
                                        yLab = "Director Experience", units = "films")
  directorExperienceLog <- univariateQuant(data.frame(mdb$title, mdb$director_experience_log),
                                           yLab = "Director Experience", units = "log(films)")
  directorScores <- univariateQuant(data.frame(mdb$title, mdb$director_scores),
                                        yLab = "Director Scores", units = "points")
  directorScoresLog <- univariateQuant(data.frame(mdb$title, mdb$director_scores_log),
                                           yLab = "Director Scores", units = "log(points)")
  directorVotes <- univariateQuant(data.frame(mdb$title, mdb$director_votes),
                                        yLab = "Director Votes", units = "films")
  directorVotesLog <- univariateQuant(data.frame(mdb$title, mdb$director_votes_log),
                                           yLab = "Director Votes", units = "log(films)")
  castExperience <- univariateQuant(data.frame(mdb$title, mdb$cast_experience),
                                    yLab = "Cast Experience", units = "films")
  castExperienceLog <- univariateQuant(data.frame(mdb$title, mdb$cast_experience_log),
                                       yLab = "Cast Experience", units = "log(films)")
  runtime <- univariateQuant(data.frame(mdb$title, mdb$runtime),
                             yLab = "Runtime", units = "minutes")
  runtimeLog <- univariateQuant(data.frame(mdb$title, mdb$runtime_log),
                                yLab = "Log Runtime", units = "minutes")
  thtrDays <- univariateQuant(data.frame(mdb$title, mdb$thtr_days),
                              yLab = "Days in Theatre", units = "days")
  thtrDaysLog <- univariateQuant(data.frame(mdb$title, mdb$thtr_days_log),
                                 yLab = "Days in Theatre", units = "days")
  imdbVotes <- univariateQuant(data.frame(mdb$title, mdb$imdb_num_votes),
                               yLab = "IMDB Votes", units = "votes")
  imdbVotesLog <- univariateQuant(data.frame(mdb$title, mdb$imdb_num_votes_log),
                                  yLab = "IMDB Log Votes", units = "log votes")
  imdbRating <- univariateQuant(data.frame(mdb$title, mdb$imdb_rating),
                                yLab = "IMDB Rating", units = "points")
  criticsScores <- univariateQuant(data.frame(mdb$title, mdb$critics_score),
                                   yLab = "Critics Score", units = "points")
  audienceScores <- univariateQuant(data.frame(mdb$title, mdb$critics_score),
                                    yLab = "Audience Score", units = "points")
  castScores <- univariateQuant(data.frame(mdb$title, mdb$cast_scores),
                                yLab = "Cast Votes", units = "scores")
  castScoresLog <- univariateQuant(data.frame(mdb$title, mdb$cast_scores_log),
                                   yLab = "Log Cast Votes", units = "log(scores)")
  castVotes <- univariateQuant(data.frame(mdb$title, mdb$cast_votes),
                               yLab = "Cast Votes", units = "votes")
  castVotesLog <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_log),
                                  yLab = "Log Cast Votes", units = "log(votes)")
  votesPerDay <- univariateQuant(data.frame(mdb$title, mdb$votes_per_day),
                                 yLab = "Votes per Day", units = "votes")
  votesPerDayLog <- univariateQuant(data.frame(mdb$title, mdb$votes_per_day_log),
                                    yLab = "Log Votes per Day", units = "votes")

  #---------------------------------------------------------------------------#
  #                 Director Cast Interaction Variables                       #
  #---------------------------------------------------------------------------#
  castDirExp <- univariateQuant(data.frame(mdb$title, mdb$cast_dir_exp),
                                        yLab = "Cast & Director Experience", units = "films")
  castDirExpLog <- univariateQuant(data.frame(mdb$title, mdb$cast_dir_exp_log),
                                yLab = "Cast & Director Experience (log)", units = "log(films)")

  castDirScores <- univariateQuant(data.frame(mdb$title, mdb$cast_dir_scores),
                                yLab = "Cast & Director Scores", units = "points")
  castDirScoresLog <- univariateQuant(data.frame(mdb$title, mdb$cast_dir_scores_log),
                                   yLab = "Cast & Director Scores (log)", units = "log(points)")

  castDirVotes <- univariateQuant(data.frame(mdb$title, mdb$cast_dir_votes),
                                   yLab = "Cast & Director Votes", units = "films")
  castDirVotesLog <- univariateQuant(data.frame(mdb$title, mdb$cast_dir_votes_log),
                                      yLab = "Cast & Director Votes (log)", units = "log(films)")

  castExpDirScores <- univariateQuant(data.frame(mdb$title, mdb$cast_exp_dir_scores),
                                  yLab = "Cast Experience & Director Scores", units = "films * points")
  castExpDirScoresLog <- univariateQuant(data.frame(mdb$title, mdb$cast_exp_dir_scores_log),
                                     yLab = "Cast Experience & Director Scores (log)", units = "log(films * points)")

  castExpDirVotes <- univariateQuant(data.frame(mdb$title, mdb$cast_exp_dir_votes),
                                      yLab = "Cast Experience & Director Votes", units = "films * votes")
  castExpDirVotesLog <- univariateQuant(data.frame(mdb$title, mdb$cast_exp_dir_votes_log),
                                         yLab = "Cast Experience & Director Votes (log)", units = "log(films * votes)")

  castScoresDirExp <- univariateQuant(data.frame(mdb$title, mdb$cast_scores_dir_exp),
                                     yLab = "Cast Scores & Director Experience", units = "points * films")
  castScoresDirExpLog <- univariateQuant(data.frame(mdb$title, mdb$cast_scores_dir_exp_log),
                                        yLab = "Cast Scores & Director Experience (log)", units = "log(points * films)")

  castScoresDirVotes <- univariateQuant(data.frame(mdb$title, mdb$cast_scores_dir_votes),
                                      yLab = "Cast Scores & Director Votes", units = "points * films")
  castScoresDirVotesLog <- univariateQuant(data.frame(mdb$title, mdb$cast_scores_dir_votes_log),
                                         yLab = "Cast Scores & Director Votes (log)", units = "log(points * films)")

  castVotesDirExp <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_dir_exp),
                                      yLab = "Cast Votes & Director Experience", units = "points * films")
  castVotesDirExpLog <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_dir_exp_log),
                                         yLab = "Cast Votes & Director Experience (log)", units = "log(points * films)")

  castVotesDirScores <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_dir_scores),
                                     yLab = "Cast Votes & Director Scores", units = "points * films")
  castVotesDirScoresLog <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_dir_scores_log),
                                        yLab = "Cast Votes & Director Scores (log)", units = "log(points * films)")






  #---------------------------------------------------------------------------#
  #                   Obtain statistics on box office revenue                 #
  #---------------------------------------------------------------------------#
  mdb2 <- mdb %>% filter(!is.na(box_office))
  boxOffice <- univariateQuant(data.frame(mdb2$title, mdb2$box_office),
                                    yLab = "Box Office", units = "dollars")
  boxOfficeLog <- univariateQuant(data.frame(mdb2$title, mdb2$box_office_log),
                                       yLab = "Log Box Office", units = "log(dollars)")

  #---------------------------------------------------------------------------#
  #                            Capture Outliers                               #
  #---------------------------------------------------------------------------#
  outliers <- rbind(directorExperience$outliers, directorExperienceLog$outliers,
                    castExperience$outliers, castExperienceLog$outliers,
                    runtime$outliers, runtimeLog$outliers,
                    thtrDays$outliers, thtrDaysLog$outliers,
                    imdbVotes$outliers, imdbVotesLog$outliers,
                    imdbRating$outliers, criticsScores$outliers,
                    audienceScores$outliers,
                    castScores$outliers, castScoresLog$outliers,
                    castVotes$outliers, castVotesLog$outliers,
                    votesPerDay$outliers, votesPerDayLog$outliers,
                    boxOffice$outliers, boxOffice$outliers)

  # Return analysis
  analysis <- list(
    genre = genre,
    mpaa = mpaa,
    season = season,
    month = month,
    bestPicNom = bestPicNom,
    bestPicWin = bestPicWin,
    bestDirWin = bestDirWin,
    bestActorWin = bestActorWin,
    bestActressWin = bestActressWin,
    top200Box = top200Box,
    directorExperience = directorExperience,
    directorExperienceLog = directorExperienceLog,
    directorScores = directorScores,
    directorScoresLog = directorScoresLog,
    directorVotes = directorVotes,
    directorVotesLog = directorVotesLog,
    castExperience = castExperience,
    castExperienceLog = castExperienceLog,
    castScores = castScores,
    castScoresLog = castScoresLog,
    castVotes = castVotes,
    castVotesLog = castVotesLog,
    castDirExp = castDirExp,
    castDirExpLog = castDirExpLog,
    castDirScores = castDirScores,
    castDirScoresLog = castDirScoresLog,
    castDirVotes = castDirVotes,
    castDirVotesLog = castDirVotesLog,
    castExpDirScores = castExpDirScores,
    castExpDirScoresLog = castExpDirScoresLog,
    castExpDirVotes = castExpDirVotes,
    castExpDirVotesLog = castExpDirVotesLog,
    castScoresDirExp = castScoresDirExp,
    castScoresDirExpLog = castScoresDirExpLog,
    castScoresDirVotes = castScoresDirVotes,
    castScoresDirVotesLog = castScoresDirVotesLog,
    castVotesDirExp = castVotesDirExp,
    castVotesDirExpLog = castVotesDirExpLog,
    castVotesDirScores = castVotesDirScores,
    castVotesDirScoresLog = castVotesDirScoresLog,
    runtime = runtime,
    runtimeLog = runtimeLog,
    thtrDays = thtrDays,
    thtrDaysLog = thtrDaysLog,
    imdbVotes = imdbVotes,
    imdbVotesLog = imdbVotesLog,
    imdbRating = imdbRating,
    criticsScores = criticsScores,
    audienceScores = audienceScores,
    votesPerDay = votesPerDay,
    votesPerDayLog = votesPerDayLog,
    boxOffice = boxOffice,
    boxOfficeLog = boxOfficeLog,
    outliers = outliers
  )
  return(analysis)
}
