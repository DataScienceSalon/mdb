#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Performs preprocessing of data for analysis
#'
#' @param movies Data frame containing the movies data set
#' @param mdb2 Data set of 100 randomly selected observations from the movies data set, in which total daily box office revenue was added.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
preprocess <- function(movies, mdb2) {

  #---------------------------------------------------------------------------#
  #                        Extract Variables of Interest                      #
  #---------------------------------------------------------------------------#
  drops <- c("critics_rating", "audience_rating", "imdb_url", "rt_url")
  mdb1 <- movies[,!(names(movies) %in% drops)]
  mdb1 <- mdb1[complete.cases(mdb1),]

  #---------------------------------------------------------------------------#
  #              Remove TV Movies and NC-17 (too few observations)            #
  #---------------------------------------------------------------------------#
  mdb1 <-  mdb1 %>% filter(title_type != "TV Movie" & mpaa_rating != "NC-17")
  mdb1$mpaa_rating <- factor(mdb1$mpaa_rating)

  #---------------------------------------------------------------------------#
  #               Create Days Since Theatrical Release                        #
  #---------------------------------------------------------------------------#
  mdb1 <- mdb1 %>% mutate(thtr_rel_date = as.Date(paste(mdb1$thtr_rel_year,
                                                       mdb1$thtr_rel_month,
                                                       mdb1$thtr_rel_day,
                                                      sep = "-"), "%Y-%m-%d"))
  thruDate <- as.Date("2016-01-01", "%Y-%m-%d")

  mdb1 <- mdb1 %>% mutate(thtr_days = as.numeric(thruDate - thtr_rel_date))

  mdb1 <- mdb1 %>% mutate(thtr_days_log = log(thtr_days))


  #---------------------------------------------------------------------------#
  #                        Create Various Mutations                           #
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
    scores = (((10 * imdb_rating) + audience_score) / 2),
    scores_log = log2(scores),
    runtime_log = log2(runtime),
    votes_per_day = (imdb_num_votes / thtr_days),
    votes_per_day_log = log2(votes_per_day),
    votes_per_day_scores = (votes_per_day * scores),
    votes_per_day_scores_log = log2(votes_per_day_scores))


  mdb1$thtr_rel_month <- factor(mdb1$thtr_rel_month, levels = c("Jan", "Feb", "Mar", "Apr",
                                                              "May", "Jun", "Jul", "Aug",
                                                              "Sep", "Oct", "Nov", "Dec"))
  mdb1$thtr_rel_season <- factor(mdb1$thtr_rel_season, levels = c("Spring", "Summer", "Fall",
                                                                  "Holidays", "Winter"))



  #---------------------------------------------------------------------------#
  #                           Create Cast Vote                                #
  #---------------------------------------------------------------------------#
  # Create actor score share data frames
  actor1 <- mdb1 %>% mutate(actor = actor1, votes = .40 * votes_per_day) %>%
    select(actor, votes)
  actor2 <- mdb1 %>% mutate(actor = actor2, votes = .30 * votes_per_day) %>%
    select(actor, votes)
  actor3 <- mdb1 %>% mutate(actor = actor3, votes = .15 * votes_per_day) %>%
    select(actor, votes)
  actor4 <- mdb1 %>% mutate(actor = actor4, votes = .10 * votes_per_day) %>%
    select(actor, votes)
  actor5 <- mdb1 %>% mutate(actor = actor5, votes = .05 * votes_per_day) %>%
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
  drops <- c("votes1", "votes2", "votes3", "votes3", "votes4", "votes5")
  mdb1 <- mdb1[,!(names(mdb1) %in% drops)]


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
  drops <- c("ce1", "ce2", "ce3", "ce3", "ce4", "ce5")
  mdb1 <- mdb1[,!(names(mdb1) %in% drops)]


  #---------------------------------------------------------------------------#
  #                 Split Data into Training and Test Sets                    #
  #---------------------------------------------------------------------------#
  # Ensure sample has all genres and MPAA ratings.
  genres <- unique(mdb1$genre)
  mpaa <- unique(mdb1$mpaa_rating)
  representative <- FALSE
  seed <- 232
  while(representative == FALSE) {
    set.seed(seed)
    idx <- sample(1:nrow(mdb1), nrow(mdb1) * .8)
    train <- mdb1[idx, ]
    test <- mdb1[-idx, ]
    if ((setequal(genres, unique(train$genre)) &
         setequal(mpaa, unique(train$mpaa_rating)))) {
      representative <- TRUE
    } else {
      seed <- seed + sample(1:100, 1)
    }
  }

  #---------------------------------------------------------------------------#
  #                     Create Daily Box Office Sample Set                    #
  #---------------------------------------------------------------------------#
  keep <- c("title", "box_office")
  mdb2 <- mdb2[, names(mdb2) %in% keep]
  mdb2 <- inner_join(mdb2, train)
  mdb2 <- mdb2 %>% mutate(daily_box_office = box_office / thtr_days)
  mdb2 <- mdb2 %>% mutate(daily_box_office_log = log2(daily_box_office))
  mdb2 <- mdb2 %>% filter(title %in% train$title)


  data <- list(
    mdb = train,
    mdb2 = mdb2,
    test = test
  )


  return(data)
}
