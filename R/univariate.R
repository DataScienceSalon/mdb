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
#' @family movies functions
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
  castVotes <- univariateQuant(data.frame(mdb$title, mdb$cast_votes),
                               yLab = "Cast Votes", units = "votes")
  castVotesLog <- univariateQuant(data.frame(mdb$title, mdb$cast_votes_log),
                               yLab = "Log Cast Votes", units = "log(votes)")
  votesPerDay <- univariateQuant(data.frame(mdb$title, mdb$votes_per_day),
                            yLab = "Votes per Day", units = "votes")
  votesPerDayLog <- univariateQuant(data.frame(mdb$title, mdb$votes_per_day_log),
                                 yLab = "Log Votes per Day", units = "votes")

  #---------------------------------------------------------------------------#
  #                   Obtain statistics on box office revenue                 #
  #---------------------------------------------------------------------------#
  mdb2 <- mdb %>% filter(!is.na(box_office))
  dailyBoxOffice <- univariateQuant(data.frame(mdb2$title, mdb2$daily_box_office),
                                 yLab = "Daily Box Office", units = "dollars")
  dailyBoxOfficeLog <- univariateQuant(data.frame(mdb2$title, mdb2$daily_box_office_log),
                               yLab = "Log Daily Box Office", units = "log(dollars)")

  #---------------------------------------------------------------------------#
  #                            Capture Outliers                               #
  #---------------------------------------------------------------------------#
  outliers <- rbind(directorExperience$outliers, directorExperienceLog$outliers,
                    castExperience$outliers, castExperienceLog$outliers,
                    runtime$outliers, runtimeLog$outliers,
                    thtrDays$outliers, thtrDaysLog$outliers,
                    imdbVotes$outliers, imdbVotesLog$outliers,
                    imdbRating$outliers, criticsScores$outliers,
                    audienceScores$outliers, castVotes$outliers,
                    castVotesLog$outliers, votesPerDay$outliers,
                    votesPerDayLog$outliers, dailyBoxOffice$outliers,
                    dailyBoxOffice$outliers)

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
    castExperience = castExperience,
    castExperienceLog = castExperienceLog,
    runtime = runtime,
    runtimeLog = runtimeLog,
    thtrDays = thtrDays,
    thtrDaysLog = thtrDaysLog,
    imdbVotes = imdbVotes,
    imdbVotesLog = imdbVotesLog,
    imdbRating = imdbRating,
    criticsScores = criticsScores,
    audienceScores = audienceScores,
    castVotes = castVotes,
    castVotesLog = castVotesLog,
    votesPerDay = votesPerDay,
    votesPerDayLog = votesPerDayLog,
    dailyBoxOffice = dailyBoxOffice,
    dailyBoxOfficeLog = dailyBoxOfficeLog,
    outliers = outliers
  )
  return(analysis)
}
