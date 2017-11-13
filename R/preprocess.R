#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Performs preprocessing of data for analysis
#'
#' @param movies Data frame containing the movies data set
#' @param mdb2 Data set of 100 randomly selected observations from the movies data set, in which total box office revenue was added.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
preprocess <- function(movies, mdb2) {

  #---------------------------------------------------------------------------#
  #                        Extract Variables of Interest                      #
  #---------------------------------------------------------------------------#
  mdb1 <- movies %>% select(title, title_type, genre, mpaa_rating, studio,
                           thtr_rel_year, thtr_rel_month, imdb_rating,
                           imdb_num_votes, critics_score, audience_score,
                           best_pic_nom, best_pic_win, best_actor_win,
                           best_actress_win, best_dir_win, top200_box,
                           director, actor1, actor2, actor3, actor4, actor5)
  mdb1 <- mdb1[complete.cases(mdb1),]


  #---------------------------------------------------------------------------#
  #                        Create Season and Month                            #
  #---------------------------------------------------------------------------#

  mdb1 <- mdb1 %>% mutate(
    imdb_num_votes_log = log2(imdb_num_votes),
    thtr_rel_season = ifelse(thtr_rel_month %in% c(3:5), "Spring",
                             ifelse(thtr_rel_month %in% c(6:8), "Summer",
                                    ifelse(thtr_rel_month %in% c(9:11), "Fall",
                                           ifelse(thtr_rel_month %in% c(12), "Holidays", "Winter")))),
    thtr_rel_month =
      ifelse(thtr_rel_month == 1, "Jan",
             ifelse(thtr_rel_month == 2, "Feb",
                    ifelse(thtr_rel_month == 3, "Mar",
                           ifelse(thtr_rel_month == 4, "Apr",
                                  ifelse(thtr_rel_month == 5, "May",
                                         ifelse(thtr_rel_month == 6, "Jun",
                                                ifelse(thtr_rel_month == 7, "Jul",
                                                       ifelse(thtr_rel_month == 8, "Aug",
                                                              ifelse(thtr_rel_month == 9, "Sep",
                                                                     ifelse(thtr_rel_month == 10, "Oct",
                                                                            ifelse(thtr_rel_month == 11, "Nov",
                                                                                   "Dec"))))))))))),
    scores = 10 * imdb_rating + critics_score + audience_score,
    scores_log = log2(10 * imdb_rating + critics_score + audience_score),
    votes_imdb_rating = imdb_num_votes * imdb_rating,
    votes_imdb_rating_log = log2(imdb_num_votes * imdb_rating),
    votes_critics_score = imdb_num_votes * critics_score,
    votes_critics_score_log = log2(imdb_num_votes * critics_score),
    votes_audience_score = imdb_num_votes * audience_score,
    votes_audience_score_log = log2(imdb_num_votes * audience_score),
    votes_scores = imdb_num_votes * scores,
    votes_scores_log = log2(imdb_num_votes * scores))


  mdb1$thtr_rel_month <- factor(mdb1$thtr_rel_month, levels = c("Jan", "Feb", "Mar", "Apr",
                                                              "May", "Jun", "Jul", "Aug",
                                                              "Sep", "Oct", "Nov", "Dec"))

  #---------------------------------------------------------------------------#
  #                        Create Studio Votes                                #
  #---------------------------------------------------------------------------#
  studio <- movies %>% group_by(studio) %>%
    summarize(studio_votes = sum(imdb_num_votes))
  mdb1 <- left_join(mdb1, studio)
  mdb1 <- mdb1 %>% mutate(studio_votes_log = log2(studio_votes))

  #---------------------------------------------------------------------------#
  #                           Create Cast Vote                                #
  #---------------------------------------------------------------------------#
  # Create actor score share data frames
  actor1 <- mdb1 %>% mutate(actor = actor1, votes = .40 * imdb_num_votes) %>%
    select(actor, votes)
  actor2 <- mdb1 %>% mutate(actor = actor2, votes = .30 * imdb_num_votes) %>%
    select(actor, votes)
  actor3 <- mdb1 %>% mutate(actor = actor3, votes = .15 * imdb_num_votes) %>%
    select(actor, votes)
  actor4 <- mdb1 %>% mutate(actor = actor4, votes = .10 * imdb_num_votes) %>%
    select(actor, votes)
  actor5 <- mdb1 %>% mutate(actor = actor5, votes = .05 * imdb_num_votes) %>%
    select(actor, votes)
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>% summarize(votes = sum(votes))
  actors <- actors[complete.cases(actors),]

  # Merge actor votes into main data frame
  mdb1 <- left_join(mdb1, actors, by = c("actor1" = "actor"))
  names(mdb1)[names(mdb1) == "votes"] <- "votes1"
  mdb1 <- left_join(mdb1, actors, by = c("actor2" = "actor"))
  names(mdb1)[names(mdb1) == "votes"] <- "votes2"
  mdb1 <- left_join(mdb1, actors, by = c("actor3" = "actor"))
  names(mdb1)[names(mdb1) == "votes"] <- "votes3"
  mdb1 <- left_join(mdb1, actors, by = c("actor4" = "actor"))
  names(mdb1)[names(mdb1) == "votes"] <- "votes4"
  mdb1 <- left_join(mdb1, actors, by = c("actor5" = "actor"))
  names(mdb1)[names(mdb1) == "votes"] <- "votes5"

  # Create cast votes variable
  mdb1 <- mdb1 %>% mutate(cast_votes = votes1 + votes2 + votes3 + votes4 + votes5)
  mdb1 <- mdb1 %>% mutate(cast_votes_log = log2(cast_votes))

  #---------------------------------------------------------------------------#
  #                        Create Studio Experience                           #
  #---------------------------------------------------------------------------#
  studioExperience <- mdb1 %>% group_by(studio) %>%
    summarize(studio_experience = n())
  mdb1 <- left_join(mdb1, studioExperience)
  mdb1 <- mdb1 %>% mutate(studio_experience_log = log2(studio_experience))

  #---------------------------------------------------------------------------#
  #                        Create Director Experience                         #
  #---------------------------------------------------------------------------#
  directorExperience <- mdb1 %>% group_by(director) %>%
    summarize(director_experience = n())
  mdb1 <- left_join(mdb1, directorExperience)
  mdb1 <- mdb1 %>% mutate(director_experience_log = log2(director_experience))
  #---------------------------------------------------------------------------#
  #                        Create Cast Experience                             #
  #---------------------------------------------------------------------------#
  actor1 <- mdb1 %>% mutate(actor = actor1) %>% group_by(actor) %>%  summarize(N = n())
  actor2 <- mdb1 %>% mutate(actor = actor2) %>% group_by(actor) %>%  summarize(N = n())
  actor3 <- mdb1 %>% mutate(actor = actor3) %>% group_by(actor) %>%  summarize(N = n())
  actor4 <- mdb1 %>% mutate(actor = actor4) %>% group_by(actor) %>%  summarize(N = n())
  actor5 <- mdb1 %>% mutate(actor = actor5) %>% group_by(actor) %>%  summarize(N = n())
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>%  summarize(cast_experience = n())
  mdb1 <- left_join(mdb1, actors, by = c("actor1" = "actor"))
  names(mdb1)[names(mdb1) == "cast_experience"] <- "ce1"
  mdb1 <- left_join(mdb1, actors, by = c("actor2" = "actor"))
  names(mdb1)[names(mdb1) == "cast_experience"] <- "ce2"
  mdb1 <- left_join(mdb1, actors, by = c("actor3" = "actor"))
  names(mdb1)[names(mdb1) == "cast_experience"] <- "ce3"
  mdb1 <- left_join(mdb1, actors, by = c("actor4" = "actor"))
  names(mdb1)[names(mdb1) == "cast_experience"] <- "ce4"
  mdb1 <- left_join(mdb1, actors, by = c("actor5" = "actor"))
  names(mdb1)[names(mdb1) == "cast_experience"] <- "ce5"
  mdb1 <- mdb1 %>% mutate(cast_experience = ce1 + ce2 + ce3 + ce4 + ce5)
  mdb1 <- mdb1 %>% mutate(cast_experience_log = log2(cast_experience))

  #---------------------------------------------------------------------------#
  #                     Create Box Office Sample Set                          #
  #---------------------------------------------------------------------------#
  mdb2 <- inner_join(mdb1, mdb2)
  mdb2 <- mdb2 %>% mutate(box_office_log = log2(box_office))

  data <- list(
    mdb1 = mdb1,
    mdb2 = mdb2
  )


  return(data)
}
