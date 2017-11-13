#==============================================================================#
#                             multivariateQuant                                #
#==============================================================================#
#' multivariateQuant
#'
#' \code{multivariateQuant} Performs multivariate analysis on a single quantitative
#' variable.  The function returns descriptive statistics, a histogram
#' and a box plot to check outliers.
#'
#' @param data Single column data frame containing the quantitative variable
#' @param yLab Capitalized character string for the variable name or label
#'
#' @return analysis List containing:
#'  1. Summary Statistics
#'  2. Histogram
#'  3. Normal quantile plot
#'  4. Box plot
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
multivariateQuant <- function(data, yLab) {

  # Validate input
  if (length(data) > 2) stop("Error in plotBox: input data dimensions greater than 2")

  if (length(data) == 1) {
    if (!(class(data[[1]]) %in% c("numeric"))) stop("Error in plotBox: Numeric vector or data frame required.")
  }

  # Determine index for numeric variable
  idxNum <- ifelse((class(data[[1]]) %in% c("character", "factor")), 2, 1)
  idxCat <- ifelse(idxNum == 1, 2, 1)

  # Obtain summary statistics for each group
  groups <-  unique(data[[idxCat]])
  df <- rbindlist(lapply(seq_along(groups), function(x) {
    d <- data %>% filter(data[[idxCat]] == groups[[x]]) %>% select(idxNum)
    getSummaryStats(d, groups[[x]])
  }))


  stats <- getSummaryStats(data)
  hist <- plotHist(data, yLab = yLab)
  qq <- plotQQ(data, yLab = yLab)
  box  <- plotBox(data, yLab = yLab)

  analysis <- list(
    stats = stats,
    hist = hist,
    qq = qq,
    box = box
  )

  return(analysis)
}
