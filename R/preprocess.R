#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Performs preprocessing of data for analysis
#'
#' @param movies Data frame containing the movies data set
#' @param fin1 Data set containing movie financial information
#' @param fin2 Data set containing movie financial information
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
preprocess <- function(movies, fin1, fin2) {

  #---------------------------------------------------------------------------#
  #                        Extract Variables of Interest                      #
  #---------------------------------------------------------------------------#
  drops <- c("critics_rating", "audience_rating", "imdb_url", "rt_url")
  mdb <- movies[,!(names(movies) %in% drops)]
  mdb <- mdb[complete.cases(mdb),]

  #---------------------------------------------------------------------------#
  #     Remove TV Movies (out of scope) and NC-17 (too few observations)      #
  #     and films released prior to launch of Rottentomatoes                  #
  #---------------------------------------------------------------------------#
  mdb <-  mdb %>% filter(title_type != "TV Movie" & mpaa_rating != "NC-17"
                         & thtr_rel_year > 1989)
  mdb$mpaa_rating <- factor(mdb$mpaa_rating)

  #---------------------------------------------------------------------------#
  #                                Dedupe                                     #
  #---------------------------------------------------------------------------#
  dupes <- mdb %>% group_by(title) %>% summarize(total = n())
  dupes <- dupes %>% filter(total > 1) %>% select(title)
  mdb <- mdb[!(mdb$title %in% dupes$title), ]

  #---------------------------------------------------------------------------#
  #               Create Days Since Theatrical Release                        #
  #---------------------------------------------------------------------------#
  mdb <- mdb %>% mutate(thtr_rel_date = as.Date(paste(mdb$thtr_rel_year,
                                                      mdb$thtr_rel_month,
                                                      mdb$thtr_rel_day,
                                                      sep = "-"), "%Y-%m-%d"))
  thruDate <- as.Date("2016-01-01", "%Y-%m-%d")

  mdb <- mdb %>% mutate(thtr_days = as.numeric(thruDate - thtr_rel_date))

  mdb <- mdb %>% mutate(thtr_days_log = log(thtr_days))


  #---------------------------------------------------------------------------#
  #             Update date factors and created season variable               #
  #---------------------------------------------------------------------------#

  mdb <- mdb %>% mutate(
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
                                                                                   "Dec"))))))))))))


  mdb$thtr_rel_month <- factor(mdb$thtr_rel_month, levels = c("Jan", "Feb", "Mar", "Apr",
                                                              "May", "Jun", "Jul", "Aug",
                                                              "Sep", "Oct", "Nov", "Dec"))
  mdb$thtr_rel_season <- factor(mdb$thtr_rel_season, levels = c("Spring", "Summer", "Fall",
                                                                "Holidays", "Winter"))


  #---------------------------------------------------------------------------#
  #                         Create log of runtime                             #
  #---------------------------------------------------------------------------#
  mdb <- mdb %>% mutate(runtime_log = log2(runtime))

  #---------------------------------------------------------------------------#
  #                         Add Votes per Day                                 #
  #---------------------------------------------------------------------------#
  mdb <- mdb %>% mutate(votes_per_day = (imdb_num_votes / thtr_days),
                        votes_per_day_log = log2(votes_per_day))

  #---------------------------------------------------------------------------#
  #                       Add Box Office Information                          #
  #---------------------------------------------------------------------------#
  keep <- c("title", "box_office")
  f1 <- fin1[, names(fin1) %in% keep]
  keep <- c("Movie", "Domestic")
  f2 <- fin2[, names(fin2) %in% keep]
  colnames(f2) <- c("title", "box_office")
  fin <- rbind(f1, f2)
  fin <- fin[!duplicated(fin$title),]
  fin <- fin %>% filter((round(box_office, 0) > 0) &  !is.na(box_office))
  mdb <- merge(mdb, fin, all.x = TRUE)
  mdb <- mdb %>% mutate(box_office_log = log2(box_office))

  #---------------------------------------------------------------------------#
  #                        Create Director Scores                             #
  #---------------------------------------------------------------------------#
  directorScores <- mdb %>% group_by(director) %>%
    summarize(director_scores = sum(((10 * imdb_rating) + audience_score)))
  mdb <- left_join(mdb, directorScores)
  mdb <- mdb %>% mutate(director_scores = round(director_scores - (10 * imdb_rating) + audience_score, 0))
  mdb <- mdb %>% mutate(director_scores_log = ifelse(director_scores == 0, 0, log2(director_scores)))


  #---------------------------------------------------------------------------#
  #                           Create Cast Scores                              #
  #---------------------------------------------------------------------------#
  # Create actor score share data frames
  actor1 <- mdb %>% mutate(actor = actor1, scores = .40 * (imdb_rating * 10) +
                             audience_score) %>%
    select(actor, scores)
  actor2 <- mdb %>% mutate(actor = actor2, scores = .30 * (imdb_rating * 10) +
                             audience_score) %>%
    select(actor, scores)
  actor3 <- mdb %>% mutate(actor = actor3, scores = .15 * (imdb_rating * 10) +
                             audience_score) %>%
    select(actor, scores)
  actor4 <- mdb %>% mutate(actor = actor4, scores = .10 * (imdb_rating * 10) +
                             audience_score) %>%
    select(actor, scores)
  actor5 <- mdb %>% mutate(actor = actor5, scores = .05 * (imdb_rating * 10) +
                             audience_score) %>%
    select(actor, scores)
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>% summarize(scores = sum(scores))
  actors <- actors[complete.cases(actors),]

  # Merge actor votes into main data frame
  mdb <- left_join(mdb, actors, by = c("actor1" = "actor"))
  names(mdb)[names(mdb) == "scores"] <- "scores1"
  mdb <- left_join(mdb, actors, by = c("actor2" = "actor"))
  names(mdb)[names(mdb) == "scores"] <- "scores2"
  mdb <- left_join(mdb, actors, by = c("actor3" = "actor"))
  names(mdb)[names(mdb) == "scores"] <- "scores3"
  mdb <- left_join(mdb, actors, by = c("actor4" = "actor"))
  names(mdb)[names(mdb) == "scores"] <- "scores4"
  mdb <- left_join(mdb, actors, by = c("actor5" = "actor"))
  names(mdb)[names(mdb) == "scores"] <- "scores5"

  # Create cast votes variable and clean up
  mdb <- mdb %>% mutate(cast_scores = scores1 + scores2 + scores3 + scores4 + scores5)
  mdb <- mdb %>% mutate(cast_scores = cast_scores - ((10 * imdb_rating) + audience_score))
  mdb <- mdb %>% mutate(cast_scores_log =
                          ifelse(cast_scores > 0, log2(cast_scores), 0))
  drops <- c("scores1", "scores2", "scores3", "scores3", "scores4", "scores5")
  mdb <- mdb[,!(names(mdb) %in% drops)]

  #---------------------------------------------------------------------------#
  #                        Create Director Votes                              #
  #---------------------------------------------------------------------------#
  directorVotes <- mdb %>% group_by(director) %>%
    summarize(director_votes = sum(imdb_num_votes))
  mdb <- left_join(mdb, directorVotes)
  mdb <- mdb %>% mutate(director_votes = round(director_votes - imdb_num_votes, 0))
  mdb <- mdb %>% mutate(director_votes_log = ifelse(director_votes == 0, 0, log2(director_votes)))

  #---------------------------------------------------------------------------#
  #                           Create Cast Vote                                #
  #---------------------------------------------------------------------------#
  # Create actor score share data frames
  actor1 <- mdb %>% mutate(actor = actor1, votes = .40 * imdb_num_votes) %>%
    select(actor, votes)
  actor2 <- mdb %>% mutate(actor = actor2, votes = .30 * imdb_num_votes) %>%
    select(actor, votes)
  actor3 <- mdb %>% mutate(actor = actor3, votes = .15 * imdb_num_votes) %>%
    select(actor, votes)
  actor4 <- mdb %>% mutate(actor = actor4, votes = .10 * imdb_num_votes) %>%
    select(actor, votes)
  actor5 <- mdb %>% mutate(actor = actor5, votes = .05 * imdb_num_votes) %>%
    select(actor, votes)
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>% summarize(votes = sum(votes))
  actors <- actors[complete.cases(actors),]

  # Merge actor votes into main data frame
  mdb <- left_join(mdb, actors, by = c("actor1" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes1"
  mdb <- left_join(mdb, actors, by = c("actor2" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes2"
  mdb <- left_join(mdb, actors, by = c("actor3" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes3"
  mdb <- left_join(mdb, actors, by = c("actor4" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes4"
  mdb <- left_join(mdb, actors, by = c("actor5" = "actor"))
  names(mdb)[names(mdb) == "votes"] <- "votes5"

  # Create cast votes variable and clean up
  mdb <- mdb %>% mutate(cast_votes = votes1 + votes2 + votes3 + votes4 + votes5)
  mdb <- mdb %>% mutate(cast_votes = cast_votes - imdb_num_votes)
  mdb <- mdb %>% mutate(cast_votes_log =
                          ifelse(cast_votes > 0, log2(cast_votes), 0))
  drops <- c("votes1", "votes2", "votes3", "votes3", "votes4", "votes5")
  mdb <- mdb[,!(names(mdb) %in% drops)]

  #---------------------------------------------------------------------------#
  #                        Create Director Experience                         #
  #---------------------------------------------------------------------------#
  directorExperience <- mdb %>% group_by(director) %>%
    summarize(director_experience = n())
  mdb <- left_join(mdb, directorExperience)
  mdb <- mdb %>% mutate(director_experience_log =
                          ifelse(director_experience == 0, 0, log2(director_experience)))

  #---------------------------------------------------------------------------#
  #                        Create Cast Experience                             #
  #---------------------------------------------------------------------------#
  actor1 <- mdb %>% mutate(actor = actor1) %>% group_by(actor) %>%  summarize(N = n())
  actor2 <- mdb %>% mutate(actor = actor2) %>% group_by(actor) %>%  summarize(N = n())
  actor3 <- mdb %>% mutate(actor = actor3) %>% group_by(actor) %>%  summarize(N = n())
  actor4 <- mdb %>% mutate(actor = actor4) %>% group_by(actor) %>%  summarize(N = n())
  actor5 <- mdb %>% mutate(actor = actor5) %>% group_by(actor) %>%  summarize(N = n())
  actors <- rbind(actor1, actor2, actor3, actor4, actor5)
  actors <- actors %>% group_by(actor) %>%  summarize(cast_experience = n())
  mdb <- left_join(mdb, actors, by = c("actor1" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce1"
  mdb <- left_join(mdb, actors, by = c("actor2" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce2"
  mdb <- left_join(mdb, actors, by = c("actor3" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce3"
  mdb <- left_join(mdb, actors, by = c("actor4" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce4"
  mdb <- left_join(mdb, actors, by = c("actor5" = "actor"))
  names(mdb)[names(mdb) == "cast_experience"] <- "ce5"
  mdb <- mdb %>% mutate(cast_experience = ce1 + ce2 + ce3 + ce4 + ce5)
  mdb <- mdb %>% mutate(cast_experience_log =
                          ifelse(cast_experience > 0, log2(cast_experience), 0))
  drops <- c("ce1", "ce2", "ce3", "ce3", "ce4", "ce5")
  mdb <- mdb[,!(names(mdb) %in% drops)]

  #---------------------------------------------------------------------------#
  #                      Create Interaction Variables                         #
  #---------------------------------------------------------------------------#
  mdb <- mdb %>% mutate(cast_dir_exp = round(cast_experience * director_experience, 0),
                        cast_dir_exp_log = ifelse(cast_dir_exp == 0, 0, log2(cast_dir_exp)),
                        cast_dir_scores= round(cast_scores * director_scores, 0),
                        cast_dir_scores_log = ifelse(cast_dir_scores == 0, 0, log2(cast_dir_scores)),
                        cast_dir_votes = round(cast_votes * director_votes, 0),
                        cast_dir_votes_log = ifelse(cast_dir_votes == 0, 0, log2(cast_dir_votes)),
                        cast_exp_dir_scores= round(cast_experience * director_scores, 0),
                        cast_exp_dir_scores_log = ifelse(cast_exp_dir_scores == 0, 0, log2(cast_exp_dir_scores)),
                        cast_exp_dir_votes = round(cast_experience * director_votes, 0),
                        cast_exp_dir_votes_log = ifelse(cast_exp_dir_votes == 0, 0, log2(cast_exp_dir_votes)),
                        cast_scores_dir_exp = round(cast_scores * director_experience, 0),
                        cast_scores_dir_exp_log = ifelse(cast_scores_dir_exp == 0, 0, log2(cast_scores_dir_exp)),
                        cast_scores_dir_votes = round(cast_scores * director_votes, 0),
                        cast_scores_dir_votes_log = ifelse(cast_scores_dir_votes == 0, 0, log2(cast_scores_dir_votes)),
                        cast_votes_dir_exp = round(cast_votes * director_experience, 0),
                        cast_votes_dir_exp_log = ifelse(cast_votes_dir_exp == 0, 0, log2(cast_votes_dir_exp)),
                        cast_votes_dir_scores = round(cast_votes * director_scores, 0),
                        cast_votes_dir_scores_log = ifelse(cast_votes_dir_scores == 0, 0, log2(cast_votes_dir_scores)))


  #---------------------------------------------------------------------------#
  #                      Create mdb2 from complete cases                      #
  #---------------------------------------------------------------------------#
  mdb2 <- mdb[complete.cases(mdb), ]

  #---------------------------------------------------------------------------#
  #                 Split Data into Training and Test Sets                    #
  #---------------------------------------------------------------------------#
  # Ensure sample has all genres and MPAA ratings.
  genres <- unique(movies$genre)
  mpaa <- unique(mdb$mpaa_rating)

  # Ensure that the test set contains complete cases
  representative <- FALSE
  seed <- unclass(Sys.time())
  set.seed(seed)
  while(representative == FALSE) {
    set.seed(seed)
    idx <- sample(1:nrow(mdb), nrow(mdb) * .8)
    train <- mdb[idx, ]
    test <- mdb[-idx, ]
    if (setequal(genres, unique(train$genre)) &
        setequal(mpaa, unique(train$mpaa_rating)) &
        setequal(genres, unique(test$genre)) &
        setequal(mpaa, unique(test$mpaa_rating)) &
        nrow(test %>% filter(title %in% mdb2$title)) > 0) {
      representative <- TRUE
    } else {
      seed <- seed + sample(1:100, 1)
    }
  }

  #---------------------------------------------------------------------------#
  #         Designate one cases from training set for prediction              #
  #---------------------------------------------------------------------------#
  cases <- train[complete.cases(train),]
  seed <- unclass(Sys.time())
  set.seed(seed)
  case <- cases[sample(1:nrow(cases), 1), ]
  train <- train %>% filter(title != case$title)
  mdb2 <- mdb2 %>% filter(title != case$title)

  data <- list(
    train = train,
    test = test,
    case = case,
    mdb2 = mdb2
  )


  return(data)
}
