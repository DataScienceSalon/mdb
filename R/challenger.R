#==============================================================================#
#                              challenger                                      #
#==============================================================================#
#' challenger
#'
#' \code{challenger} Iterates through correlated variables, runs all models,
#' and compiles results
#'
#' @return Data frame accuracy results for each variable added
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
challenger <- function() {

  r <- openxlsx::read.xlsx(xlsxFile = "./data/features.xlsx")
  r <- r %>% filter(c == "yes" & !(is.na(Correlation))) %>% arrange(desc(Correlation)) %>%
    select(Variable, Correlation)

  newVars <- c()
  analysis <- data.frame()
  for (i in 1:nrow(r)) {
    newVars <- c(newVars, r$Variable[i])

    # Model A
    mData <- process(train, stage = "m", y = "imdb_num_votes_log", newVars = newVars)
    m <- forward(data = mData$full, y = "imdb_num_votes_log")
    mod <- regressionAnalysis(mod = m, mName = "Model Alpha", yVar  = 'imdb_num_votes_log',
                              yLab = "Log IMDB Votes")
    accuracy <- comparePredictions(mods = list(mod), test = test)
    accuracy <- cbind(r[i,], accuracy)
    analysis <- rbind(analysis, accuracy)
  }


  return(analysis)
}
