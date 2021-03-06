#==============================================================================#
#                             Visualization Functions                          #
#==============================================================================#

#------------------------------------------------------------------------------#
#                     Frequency and Proportion Bar Plot                        #
#------------------------------------------------------------------------------#
#' plotFreqProp
#'
#' \code{plotFreqPropTbl} Renders a stacked bar plot and a contingency table
#' showing frequencies and proportions
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#' @param order Character c('a', 'd') indicating whether to order by frequency ascending (a), or descending (d)
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotFreqProp <- function(data, yLab = "Movies", xLab, plotTitle = NULL, order = NULL) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  # Format Data
  df <- as.data.frame(data)
  df <- df %>%  group_by(.[[1]]) %>%
    summarize(N = n()) %>%
    mutate(`Min N` = getPSampleSize(N / sum(N)),
           Proportion = round(N / sum(N), 2),
           Cumulative = round(cumsum(Proportion), 2),
           pos = N / 2)
  colnames(df)[1] <- xLab

  # Order data
  if (!is.null(order)) {
    if (order ==  "a") {
      df <- df %>% arrange(N)
      } else if (order == "d") {
        df <- df %>% arrange(desc(N))
      }
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = df,
                             ggplot2::aes(x = df[[1]],
                                          y = df[[2]],
                                          fill = df[[1]]))  +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = df[[1]],
                   y = df[[6]],
                   label = paste0(df[[2]], " (",
                                  round(df[[4]] * 100, 0),
                                  "%)")),
      colour="black", family="Tahoma", size = 8) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(values = myPal(length(df[[1]]))) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::labs(fill = xLab)

    visual <- list(
      stats = df[,1:5],
      plot = barPlot
    )
    return(visual)
}


#------------------------------------------------------------------------------#
#                               Plot Histogram                                 #
#------------------------------------------------------------------------------#
#' plotHist
#'
#' \code{plotHist} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame containing a single numeric variable
#' @param xLab Capital case character string the group or subset of data being printed (optional)
#' @param yLab Capital case character string describing y is being printed
#' @param plotTitle Capital case character string for the plotTitle of the plot
#'
#' @return List containing a summary statistics and a histogram
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotHist <- function(data, xLab = NULL, yLab, plotTitle = NULL) {

  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- yLab
    } else {
      plotTitle <- paste(yLab, "by", xLab)
    }
  }

  hist <- ggplot2::ggplot(data = data,
                        ggplot2::aes(x = data[[1]])) +
    ggplot2::geom_histogram(position ='identity', color = "white", fill = "palegreen4", bins = 30) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::scale_x_continuous(labels = scales::comma)

  return(hist)
}

#------------------------------------------------------------------------------#
#                               Plot Quantile                                  #
#------------------------------------------------------------------------------#
#' plotQQ
#'
#' \code{plotQQ} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame or vector containing a single numeric variable
#' @param xLab Capital case character string containing the name of the grouping or subset variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return List containing a summary statistics and QQ plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotQQ <- function(data, xLab = NULL, yLab, plotTitle = NULL) {

  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- paste("Normal Q-Q Plot:", yLab)
    } else {
      plotTitle <- paste("Normal Q-Q Plot:", yLab, "by", xLab)
    }
  }


  # Render QQ Plot
  qq <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(sample = data[[1]])) +
    qqplotr::stat_qq_band() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_point() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::ggtitle(plotTitle)

  return(qq)
}

#------------------------------------------------------------------------------#
#                                 Boxplot                                      #
#------------------------------------------------------------------------------#
#' plotBox
#'
#' \code{plotBox} Renders a single or grouped box plot.
#'
#' @param data Data frame or vector containing two columns:
#'          1: Numeric response variable (y)
#'          2: Categorical independent variable (x)
#' @param xLab Capital case character string containing the name of the x variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Character case character string containing the title for the plot
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBox <- function(data, xLab = NULL, yLab, plotTitle = NULL) {

  if (length(data) > 2) stop(paste("Error in plotBox: Dimension of data frame must be 1 or 2, not", length(data)))
  if (length(data) == 1) {
    data <- data.frame(x = rep("x", nrow(data)),
                       y = data[[1]], row.names = NULL)
  } else {
    data <- data.frame(y = data[[1]],
                       x = data[[2]],
                       row.names = NULL)
  }

  # Format title
  if (is.null(plotTitle)) {
   if (is.null(xLab)) {
     plotTitle <- yLab
   } else {
     plotTitle <- paste(yLab, "by", xLab)
   }
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  bp <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = data$x,
                                          y = data$y,
                                          fill = data$x))  +
    ggplot2::geom_boxplot(outlier.colour = "black") +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "right") +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::scale_fill_manual(values = myPal(length(unique(data$x))))

  if (is.null(xLab)) {
    bp <- bp + ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  } else {
    bp <- bp + ggplot2::labs(y = yLab, fill = xLab)
  }

  return(bp)
}


#------------------------------------------------------------------------------#
#                               Plot Scatterplot                               #
#------------------------------------------------------------------------------#
#' plotScatter
#'
#' \code{plotScatter} Renders a scatterplot for two numerical variablesa
#'
#' @param data Data frame containing the quantitative variables
#' @param xLab Capital case character string containing the name of the grouping or subset variable
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Capital case character string for title of plot
#'
#' @return Scatterplot object
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotScatter <- function(data, xLab, yLab, plotTitle = NULL) {

  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  scatter <- ggplot2::ggplot(data = data,
                             ggplot2::aes(y = as.numeric(unlist(data[,1])),
                                          x = as.numeric(unlist(data[,2])))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "right") +
    ggplot2::labs(x = xLab, y = yLab) +
    ggplot2::ggtitle(plotTitle)

  return(scatter)

}

#------------------------------------------------------------------------------#
#                             Plot Residual QQ                                 #
#------------------------------------------------------------------------------#
#' plotResQQ
#'
#' \code{plotResQQ} Plots regression diagnostics
#'
#' @param mod Linear model object
#' @param mName Capital case character string for the model name
#' @param xLab Capital case character string for the independent variable
#'
#' @return resQQ Residual Normal QQ Plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotResQQ <- function(mod, mName, xLab) {

  # Obtain diagnostics and render plot
  resQQ <- lindia::gg_diagnose(mod, theme = ggplot2::theme_minimal(), plot.all = FALSE)
  resQQ <- resQQ$qqplot +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::ggtitle(paste0(mName, ": Normal-QQ Plot"))

  return(resQQ)
}

#------------------------------------------------------------------------------#
#                           Plot Residual vs. Fit                              #
#------------------------------------------------------------------------------#
#' plotResFit
#'
#' \code{plotResFit} Plots regression diagnostics
#'
#' @param mod Linear model object
#' @param yLab Capital case character string for response variable
#'
#' @return resFit Residual vs. Fit Plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotResFit <- function(mod, yLab) {

  # Obtain diagnostics and render plot
  resFit <- lindia::gg_diagnose(mod, theme = ggplot2::theme_minimal(), plot.all = FALSE)
  resFit <- resFit$res_fitted +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::ggtitle(paste("Residuals vs. Fit:", yLab))

  return(resFit)
}

#------------------------------------------------------------------------------#
#                           Plot Residual vs. Leverage                         #
#------------------------------------------------------------------------------#
#' plotResLeverage
#'
#' \code{plotResLeverage} Plots regression diagnostics
#'
#' @param mod Linear model object
#' @param mName Capital case character string for the model name
#'
#' @return resLeverage Residual vs. Leverage Plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotResLeverage <- function(mod, mName) {

  # Obtain diagnostics and render plot
  resLeverage <- lindia::gg_diagnose(mod, theme = ggplot2::theme_minimal(), plot.all = FALSE)
  resLeverage <- resLeverage$resleverage +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::ggtitle(paste0(mName, ": Residuals vs. Leverage"))

  return(resLeverage)
}

#------------------------------------------------------------------------------#
#                           Plot Residuals Cooks Distance                      #
#------------------------------------------------------------------------------#
#' plotCooks
#'
#' \code{plotCooks} Plots cooks distance
#'
#' @param mod Linear model object
#' @param mName Capital case character string for the model name
#'
#' @return cooks Cooks distance plot for residuals
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotCooks <- function(mod, mName) {

  # Obtain diagnostics and render plot
  cooks <- lindia::gg_diagnose(mod, theme = ggplot2::theme_minimal(), plot.all = FALSE)
  cooks <- cooks$cooksd +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::ggtitle(paste0(mName, ": Cooks Distance"))

  return(cooks)
}

#------------------------------------------------------------------------------#
#                     Plot Residuals for all Predictors                        #
#------------------------------------------------------------------------------#
#' plotResAll
#'
#' \code{plotResAll} Plots residuals for each predictor
#'
#' @param mod Linear model object
#' @param mName Character string for model name
#' @param yLab Character string for the y-label
#'
#' @return list containing ressidual v predictor plots
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotResAll <- function(mod, mName = NULL, yLab = NULL) {

  # Obtain diagnostics and render plot
  plots <- lindia::gg_resX(mod, plot.all = FALSE)

  resAll <- lapply(plots, function(p) {

    b <- ggplot2::ggplot_build(p)
    if (is.null(b$data[[1]]$y)) {

      # Render box plot
      xVar <- p$labels$x
      xData <- p$data[colnames(p$data) == xVar]
      df <- data.frame(y = mod$residuals,
                       x = xData)
      plotBox(data = df, xLab = xVar, yLab = "Residuals",
              plotTitle = paste0(mName, ": Residuals vs. ", xVar))

    } else {

      # Render scatterplot
      p + ggplot2::geom_point() +
        ggplot2::geom_smooth() +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::geom_smooth(method = lm, se = FALSE) +
        ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                       legend.position = "right") +
        ggplot2::labs(y = "Residuals")
    }
  })

  return(resAll)
}

#------------------------------------------------------------------------------#
#                     Plot Regression for all Predictors                       #
#------------------------------------------------------------------------------#
#' plotLinear
#'
#' \code{plotLinear} Plots linear regression lines for each predictor
#'
#' @param mod Linear model object
#' @param yVar Character string for the dependent variable
#' @param yLab Character string for the y-label
#'
#' @return list containing ressidual v predictor plots
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotLinear <- function(mod, yVar, yLab = NULL) {

  xVars <- names(mod$model)[!(names(mod$model) == yVar)]

  plots <- lapply(xVars, function(v) {
    df <- data.frame(y = mod$model[yVar],
                     x = mod$model[v])
    plotScatter(data = df, xLab = v, yLab = yLab,
                plotTitle = paste(yLab, "by", v))
  })

  return(plots)
}

#------------------------------------------------------------------------------#
#                            Plot Correlation                                  #
#------------------------------------------------------------------------------#
#' plotCorr
#'
#' \code{plotCorr} Creates correlation plot
#'
#' @param mod Linear model object
#' @param yVar Character string for the dependent variable
#'
#' @return list containing ressidual v predictor plots
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotCorr <- function(mod, yVar) {

  plotList <- function(r) {

    plot <- function() {
      corrplot::corrplot(r, diag = FALSE,
                         order = "hclust", number.cex = 1,
                         addCoef.col = "black", tl.col = "black",
                         tl.srt = 90, tl.pos = "td", tl.cex = 1,
                         method = "color", type = "upper",
                         col = RColorBrewer::brewer.pal(n = 11,
                                                        name = "PiYG"))
    }
    list(plot = plot)
  }

  x <- mod$model[names(mod$model)[!(names(mod$model) == yVar)]]
  nums <- sapply(x, is.numeric)
  x <- x[ , nums]
  p <- plotList(r = cor(x))
  return(p)
}


#------------------------------------------------------------------------------#
#                                 Line Plot                                    #
#------------------------------------------------------------------------------#
#' plotLine
#'
#' \code{plotLine} Renders a line plot
#'
#' @param data Data frame containing plokt data
#' @param xLab Capital case character string containing the name of the x variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Character case character string containing the title for the plot
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotLine <- function(data, xLab = NULL, yLab, plotTitle = NULL) {

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  lp <- ggplot2::ggplot(data = data,
                        ggplot2::aes(x = data[1],
                                     y = data[2]))  +
    ggplot2::geom_line() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "right") +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::labs(y = yLab, x = xLab) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::scale_y_continuous(labels = scales::comma)

  return(lp)
}


#------------------------------------------------------------------------------#
#                                Bar Plot                                      #
#------------------------------------------------------------------------------#
#' plotBar
#'
#' \code{plotBar} Renders a bar plot
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return Bar plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBar <- function(data, yLab, xLab, plotTitle = NULL) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = reorder(data[[1]], -data[[2]]),
                                          y = data[[2]],
                                          fill = data[[1]]))  +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = myPal(length(data[[1]]))) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::labs(fill = xLab) +
    ggplot2::scale_y_continuous(labels = scales::comma)


  return(barPlot)
}

