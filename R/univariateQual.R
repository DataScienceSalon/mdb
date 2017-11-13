#==============================================================================#
#                             univariateQual                                   #
#==============================================================================#
#' univariateQual
#'
#' \code{univariateQual} Performs univariate analysis on a single qualitative
#' or categorical variable. The function returns a contingency table as well
#' as a stacked bar plot showing frequencies and percentages.
#'
#' @param data Single column data frame containing the categorical variable
#' @param xLab Capitalized character string for the variable name or label
#'
#' @return analysis List containing:
#'  1. Contingency table
#'  2. Frequency Proportion Barplot
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family movies functions
#' @export
univariateQual <- function(data, xLab) {

  bp <- plotFreqProp(data, xLab = xLab, order = "d")

  return(bp)
}
