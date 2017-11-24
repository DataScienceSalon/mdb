#==============================================================================#
#                                 correlate                                    #
#==============================================================================#
#' correlate
#'
#' \code{correlate} Performs the correlate analysis between a set of
#' independent variables (x) and a dependent variable (y).  Conducts a
#' correlation test and renders correlation and scatter plots.
#'
#' @param x Data frame containing the explanatory variable data
#' @param y Data frame containing the response data
#' @param yLab Character string containing label for the response variable
#' @param plot Logical indicating whether to include a correlation plot
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
correlate <- function(x, y, yLab = NULL, plot = FALSE) {

  analysis <- list()

  # Get variable names
  xNames <- colnames(x)
  yName <- ifelse(is.null(yLab), colnames(y), yLab)

  # Create correlation plot of all x
  if (plot == TRUE) {
    m <- cor(x)
    m <- as.data.frame(as.table(m))
    analysis[["cMatrix"]] <- m %>% filter(Freq < 1)
    analysis[["cPlot"]] <- corrplot::corrplot(cor(x), diag = FALSE,
                                              order = "hclust", number.cex = .7,
                                              addCoef.col = "black", tl.col = "black",
                                              tl.srt = 90, tl.pos = "td", tl.cex = 0.5,
                                              method = "color", type = "upper",
                                              col = RColorBrewer::brewer.pal(n = 11,
                                                                             name = "PiYG"))
  }

  # Conduct correlation tests of x with y
  cTests <- data.table::rbindlist(lapply(seq_along(xNames), function(idx) {
    t <- cor.test(x[[idx]], y[[1]])
    data.frame(Variable = xNames[[idx]],
               Correlation = round(t$estimate, 3),
               Statistic = round(t$statistic, 3),
               df = t$parameter,
               `p-value` = ifelse(t$p.value < 0.001, "p < .001",
                                  ifelse(t$p.value < 0.01, "p < .01",
                                         ifelse(t$p.value < 0.05, "p < .05",
                                                round(t$p.value, 3)))),
               `CI` = paste("[ ", round(t$conf.int[1], 2), ", ",
                                round(t$conf.int[2], 2), " ]")
               )
  }))
  colnames(cTests) <- c("Variable", "Correlation", "Statistic", "df", "p-value", "95% CI")
  analysis[["tests"]] <- cTests %>% arrange(desc(abs(Correlation)))

  # Produce scatter plots
  sorted <- cTests$Variable[1:4]
  analysis[["sPlots"]] <- lapply(seq_along(sorted), function(idx) {
    df <- data.frame(y = y,
                     x = x[[sorted[idx]]])
    plotScatter(data = df, xLab = sorted[idx], yLab = yName)
  })

  return(analysis)
}
