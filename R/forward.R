#==============================================================================#
#                                     forward                                  #
#==============================================================================#
#' forward
#'
#' \code{forward} Creates prediction model using forward selection
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
forward <- function(movies) {


  evalModel <- function(data, selected, predictors) {

    model <- rbindlist(lapply(predictors, function(x) {
      newPredictors <- c(selected, x)
      lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(data)[newPredictors], collapse=" + ")))
      eval <- summary(lm(lrFormula, data))
      data.frame(Selected = x,
                 Adjusted.R2 = eval$adj.r.squared,
                 stringsAsFactors = FALSE)
    }))
    model <- model %>% filter(Adjusted.R2 == max(Adjusted.R2))
    score <- list(
      selected = model$Selected,
      AR2 = model$Adjusted.R2[1]
    )
    return(score)
  }


  data <- movies %>% select(title_type, genre, runtime,
                            mpaa_rating, thtr_rel_month, dvd_rel_month,
                            imdb_rating, imdb_num_votes, critics_score,
                            best_pic_nom, best_pic_win, best_actor_win,
                            best_actress_win, best_dir_win, top200_box,
                            audience_score)

  bestAR2 <- newAR2 <- 0
  predictors <- c(1:15)
  selected <- c()
  for (i in 1:length(predictors)) {
    score <- evalModel(data, selected, predictors)
    if (score$AR2 > bestAR2) {
      selected <- c(selected, score$selected)
      bestAR2 <- score$AR2
    }
  }
  lrFormula <- formula(paste("audience_score ~ ",
                             paste(colnames(data)[unique(selected)], collapse=" + ")))
  finalModel <- lm(lrFormula, data)


  return(finalModel)

}
