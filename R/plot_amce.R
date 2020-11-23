#' Interactive Power Curves for AMCEs
#'
#' This is a convenience function to quickly visualize basic power curves for AMCEs with plotly.
#' The function will be extended to provide power curves for AMCIEs or conditional AMCEs in the future.
#' But see \link[cjpowR]{cjpowr_amcie} for plotting examples. Returns a \link[plotly]{plot_ly} object that can be easily manipulated.
#'
#' In this early version, you can specify y only as "power" or "n" and x as "amce" or "n". Whatever has been set as x, has to be passed as a sequence of values
#' specified by seq(). For instance, if you want a range of AMCEs on the x axis, argument amce has to be = seq(from = 0.02, to = 0.1, length.out = 1000).
#' Whatever has been set as y does not have to be set as argument and cannot be set to x.
#
#' To add a third dimension to the 2d plot, specify by with one of the remaining parameters. Whatever has been
#' set to by, has to be passed as a list of values.
#' For all remaining parameters, set single values. At the moment, delta0 cannot be set as "by" or "x".
#' @import plyr
#' @import plotly
#' @import tidyverse
#' @param y axis of the interactive plot.
#' @param x axis of the interactive plot.
#' @param by third dimension of the 2d plot.
#' @param power see details.
#' @param amce see details.
#' @param alpha see details.
#' @param levels see details.
#' @param n see details.
#' @param delta0 see details.
#' @keywords conjoint, power analysis, AMCE
#' @export
#' @examples
#'cjpowr_plotly(y = "n", x = "amce", by = "power",
#'amce = seq(from = 0.02, to = 0.1, length.out = 1000),
#'power = c(0.7,0.8,0.9),
#'alpha = 0.05,
#'levels = 5,
#'delta0=0.5)
#'
#' cjpowr_plotly(y = "power", x = "n", by = "amce",
#'amce = c(0.02, 0.03, 0.04),
#'n = seq(from = 100, to = 30000, by = 5),
#'alpha = 0.05,
#'levels = 5,
#'delta0=0.5)


cjpowr_plotly <- function(y, x, by, power = NULL, levels= NULL, alpha = NULL, amce = NULL, n = NULL, delta0 = 0.5){

  if (y == "n" & x == "amce" & by == "power") {

    power <- expand.grid(power=power)

    df <- plyr::mdply(power, cjpowr_amce, amce=amce, levels =levels, alpha = alpha, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~amce, y = ~n, type = 'scatter', mode = 'lines', linetype = ~power) %>%
      layout(
        xaxis = list(title = "AMCE"),
        yaxis = list(title = "Minimum Effective Sample Size"),
        legend=list(title=list(text='<b> Power </b>')),
        hovermode = "x unified"
        )

  }



  else  if (y == "n" & x == "amce" & by == "levels") {

    levels <- expand.grid(levels=levels)

    df <- plyr::mdply(levels, cjpowr_amce, amce=amce, power = power, alpha = alpha, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~amce, y = ~n, type = 'scatter', mode = 'lines', linetype = ~levels) %>%
      layout(
        xaxis = list(title = "AMCE"),
        yaxis = list(title = "Minimum Effective Sample Size"),
        legend=list(title=list(text='<b> Levels </b>')),
        hovermode = "x unified"
      )
  }


  else  if (y == "n" & x == "amce" & by == "alpha") {

    alpha <- expand.grid(alpha=alpha)

    df <- plyr::mdply(alpha, cjpowr_amce, amce=amce, power = power, levels = levels, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~amce, y = ~n, type = 'scatter', mode = 'lines', linetype = ~alpha) %>%
      layout(
        xaxis = list(title = "AMCE"),
        yaxis = list(title = "Minimum Effective Sample Size"),
        legend=list(title=list(text='<b> Alpha </b>')),
        hovermode = "x unified"
      )
  }


  else  if (y == "power" & x == "n" & by == "amce") {

    amce <- expand.grid(amce=amce)

    df <- plyr::mdply(amce, cjpowr_amce, n=n, levels =levels, alpha = alpha, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~n, y = ~power, type = 'scatter', mode = 'lines', linetype = ~amce) %>%
      layout(
        xaxis = list(title = "Effective Sample Size"),
        yaxis = list(title = "Power"),
        legend=list(title=list(text='<b> AMCE </b>')),
        hovermode = "x unified"
      )

  }

  else  if (y == "power" & x == "n" & by == "levels") {

    levels <- expand.grid(levels=levels)

    df <- plyr::mdply(levels, cjpowr_amce, n=n, amce=amce, alpha = alpha, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~n, y = ~power, type = 'scatter', mode = 'lines', linetype = ~levels) %>%
      layout(
        xaxis = list(title = "Effective Sample Size"),
        yaxis = list(title = "Power"),
        legend=list(title=list(text='<b> Levels </b>')),
        hovermode = "x unified"
      )
  }

  else  if (y == "power" & x == "n" & by == "alpha") {

    alpha <- expand.grid(alpha=alpha)

    df <- plyr::mdply(alpha, cjpowr_amce, n=n, amce=amce, levels = levels, sims = NULL, delta0=delta0)


   plot_ly(df, x = ~n, y = ~power, type = 'scatter', mode = 'lines', linetype = ~alpha) %>%
      layout(
        xaxis = list(title = "Effective Sample Size"),
        yaxis = list(title = "Power"),
        legend=list(title=list(text='<b> Alpha </b>')),
        hovermode = "x unified"
      )

  }


  else  if (y == "power" & x == "amce" & by == "n") {

    n <- expand.grid(n=n)

    df <- plyr::mdply(n, cjpowr_amce, amce=amce, alpha=alpha, levels = levels, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~amce, y = ~power, type = 'scatter', mode = 'lines', linetype = ~n) %>%
      layout(
        xaxis = list(title = "AMCE"),
        yaxis = list(title = "Power"),
        legend=list(title=list(text='<b> Effective Sample Size </b>')),
        hovermode = "x unified"
      )

  }

  else  if (y == "power" & x == "amce" & by == "levels") {

    levels <- expand.grid(levels=levels)

    df <- plyr::mdply(levels, cjpowr_amce, amce=amce, alpha=alpha, n = n, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~amce, y = ~power, type = 'scatter', mode = 'lines', linetype = ~levels) %>%
      layout(
        xaxis = list(title = "AMCE"),
        yaxis = list(title = "Power"),
        legend=list(title=list(text='<b> Levels </b>')),
        hovermode = "x unified"
      )
  }

  else  if (y == "power" & x == "amce" & by == "alpha") {

    alpha <- expand.grid(alpha=alpha)

    df <- plyr::mdply(alpha, cjpowr_amce, amce=amce, levels=levels, n = n, sims = NULL, delta0=delta0)

    plot_ly(df, x = ~amce, y = ~power, type = 'scatter', mode = 'lines', linetype = ~alpha) %>%
      layout(
        xaxis = list(title = "AMCE"),
        yaxis = list(title = "Power"),
        legend=list(title=list(text='<b> Alpha </b>')),
        hovermode = "x unified"
      )
  }

}

