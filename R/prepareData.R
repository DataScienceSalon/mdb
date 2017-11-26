#==============================================================================#
#                                 prepareData                                  #
#==============================================================================#
#' prepareData
#'
#' \code{prepareData} Prepares data for prediction
#'
#' @param cases Data frame containing cases to predict
#' @param actors Data set containing actor score data
#'
#' @return Data frame containing prediction data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
prepareData <- function(cases, actors) {

  #---------------------------------------------------------------------------#
  #                         Create Log Transformations                        #
  #---------------------------------------------------------------------------#
  cases <- cases %>% mutate(runtime_log = log(runtime),
                          box_office_log_actual = log(box_office_actual),
                          imdb_num_votes_log_actual = log(imdb_num_votes_actual))


  #---------------------------------------------------------------------------#
  #                           Create Cast Scores                              #
  #---------------------------------------------------------------------------#
  actors <- cases %>% select(title, actor1, actor2, actor3, actor4, actor5)
  actors <- reshape2::melt(actors, id.vars = c('title'))
  actors <- actors[complete.cases(actors),]
  colnames(actors) <- c('title', 'variable', 'actor')
  actors <- merge(actors, actorScores, by = 'actor')
  movieScores <- actors %>% group_by(title) %>% summarise(cast_scores = sum(scores)) %>%
    select(title, cast_scores)
  cases <- merge(cases, movieScores, by = 'title', all.x = TRUE)



  return(cases)
}
