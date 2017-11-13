#==============================================================================#
#                                     back                                     #
#==============================================================================#
#' back
#'
#' \code{back} Creates prediction model using backward elimination
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
back <- function(movies) {


  evalModel <- function(data, predictors) {

    predictorSet <- lapply(predictors, function(x) {
      p <- list(
        omit = x,
        predictors = predictors[(!(predictors %in% x))]
      )
      p
    })
    model <- rbindlist(lapply(seq_along(predictorSet), function(x) {
      lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(data)[predictorSet[[x]]$predictors], collapse=" + ")))
      eval <- summary(lm(lrFormula, data))
      data.frame(iOmit = predictorSet[[x]]$omit,
                 Adjusted.R2 = eval$adj.r.squared,
                 stringsAsFactors = FALSE)
    }))
    model <- model %>% filter(Adjusted.R2 == max(Adjusted.R2))
    score <- list(
      omit = model$iOmit,
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

  bestAR2 <- 0
  omits <- list()
  predictors <- c(1:15)
  lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(data)[predictors], collapse=" + ")))
  baseModel <- summary(lm(lrFormula, data))
  newAR2 <- baseModel$adj.r.squared
  while(bestAR2 < newAR2) {
    bestAR2 <- newAR2
    score <- evalModel(data, predictors)
    omits <- c(omits, score$omit)
    newAR2 <- score$AR2
    predictors <- predictors[(!(predictors %in% omits))]
  }
  lrFormula <- formula(paste("audience_score ~ ",  paste(colnames(data)[predictors], collapse=" + ")))
  finalModel <- lm(lrFormula, data)


  return(finalModel)

}
