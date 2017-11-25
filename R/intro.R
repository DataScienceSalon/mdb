#------------------------------------------------------------------------------#
#                           Introduction Function                              #
#------------------------------------------------------------------------------#
#' intro
#'
#' \code{intro} Provides descriptive statistics for intro section.
#'
#' @param data Data frame containing the full model#'
#'
#' @return Data frame and histogram
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @export
intro <- function(data) {

  domestic <- as.data.frame(data$Domestic)
  domesticLog <- as.data.frame(log(data$Domestic))

  df <- getSummaryStats(domestic)
  p1 <- plotHist(domestic, xLab = "Domestic Box Office", yLab = "Frequency",
                   plotTitle = 'Domestic Box Office ')
  p2 <- plotHist(domesticLog, xLab = "Domestic Box Office (Log)", yLab = "Frequency",
                   plotTitle = 'Domestic Box Office (Log)')
  top5 <- data %>% arrange(desc(Domestic)) %>%  select(Movie, Domestic)
  top5 <- head(top5, 5)

  analysis <- list()
  analysis[['stats']] <- df
  analysis[['p1']] <- p1
  analysis[['p2']] <- p2
  analysis[['top5']] <- top5

  return(analysis)

}
