#' Calculate Power or Minimum Required Sample Size for AMCEs in Conjoint Experiments
#'
#' Based on Schuessler/Freitag (2020), this function calculates the minimum effective
#' sample size (MES) required to reject a null hypothesis of no effect with probablity (power) if the alternative is true for a given average marginal component effect (AMCE) and a given alpha level.
#' The function can be manipulated to give power for a given effective sample size. Type S (incorrect sign)and the expected type M errors (exaggeration ratio) are also computed (Lu/Qui/Deng 2019).
#' The effective sample size is the number of profiles times the number of tasks. The outcome is assumed to be binary (forced-choice).
#'
#' This function assumes a binary outcome variable and the AMCE recast in a regression setting using a two-sided t-test.
#' Using conservative assumptions, the MES can simply be divided by
#' the number of profiles times the number of tasks to optimize the design vis-a-vis
#' resource constraints. The number of attributes does not affect power.
#' The function is deliberately simple and its output can easily be passed.
#'
#' Passing instead the effective sample size to the function gives power.
#' Type S (incorrect sign)and the expected type M errors (exaggeration ratio) are also computed (Lu/Qui/Deng 2019). If your study is
#' not completely underpowered, you do not need to worry about type S error. However, only high powered
#' studies result in low E(type M erorrs).
#'
#' To calculate power for conditional AMCEs or interaction effects,
#' users are referred to \link[cjpowR]{cjpowr_amcie}.
#' @param amce of a user-specified size.
#' @param alpha the significance level, 0.05 by default.
#' @param power power of the test (1 minus Type II error probability), default is NULL.
#' @param levels the number of levels of the treatment.
#' @param n effective sample size, default is NULL.
#' @param treat.prob default is 0.5.
#' @param sims number of simulation runs to compute the expected type M error ("exaggeration ratio"), default 100k. If NULL or, E(type M) is not computed (much faster).
#' @keywords conjoint, power analysis, AMCE
#' @export
#' @export
#' @export
#' @export
#' @examples
#' # This gives the minimum required effective sample size (type S, E(type M)):
#' df <- cjpowr_amce(amce = 0.05, power = 0.8, levels = 5)
#'
#' # For example, for a conjoint with 2 profiles and 4 tasks, n becomes:
#'
#' df$n / (2 * 4)
#'
#' # This gives the power (type S, E(type M)):
#' cjpowr_amce(amce = 0.05, n = 7829.258, levels = 5)
#'
#' # Generating an interactive plot for type M error:
#'
#' d <- expand.grid(
#'     amce = c(0.01, 0.02, 0.03, 0.05), 
#'     n = seq(from = 100, to = 50000, length.out = 1000), 
#'     alpha = 0.05, 
#'     levels = 2,
#'     treat.prob = 0.5,
#'     sims = 10000) #set to 0 if you want to make an interactive plot for something other than Type M error
#'
#' df <- list2DF(do.call(cjpowr_amce, d))
#'
#' library(plotly)
#'
#' plot_ly(df, x = ~n, y = ~exp_typeM, type = "scatter", mode = "lines", linetype = ~amce) %>%
#'   layout(
#'     xaxis = list(
#'       title = "Effective Sample Size",
#'       zeroline = F,
#'       hoverformat = ".0f"
#'     ),
#'     yaxis = list(
#'       title = "Exaggeration Ratio",
#'       range = c(0, 10),
#'       zeroline = F,
#'       hoverformat = ".2f"
#'     ),
#'     legend = list(title = list(text = "<b> AMCE </b>")),
#'     hovermode = "x unified"
#'   )
#' @section Literature:
#'
#' 1. Schuessler, J. and M. Freitag (2020). Power Analysis for Conjoint Experiments.
#'    Working Paper.
#'
#' 2. Lu, J., Y. Qiu, and A. Deng (2019). A note on Type S/M errors in hypothesis testing.
#'    British Journal of Mathematical and Statistical Psychology 72(1), 1–17.

cjpowr_amce <- function(amce, alpha = 0.05, power = NULL, levels = 2,
                        treat.prob = 0.5, n = NULL, sims = 100000) {
  if (sum(sapply(list(power, n), is.null)) != 1) {
    stop("either 'n' or 'power' must be provided")
  }
  if (!is.null(alpha) && !is.numeric(alpha) || any(0 > alpha | alpha > 1)) {
    stop("'sig.level' must be numeric in [0, 1]")
  }

  lengths <- c(length(amce), length(alpha), length(power), length(levels), length(treat.prob), length(n), length(sims))
  non_null_lengths <- lengths[lengths > 0]

  if (length(unique(non_null_lengths)) > 1) {
    stop("All non-NULL input parameters must be of the same length.")
  }

  length_output <- unique(non_null_lengths)

  if (is.null(sims) | any(sims == 0) == TRUE) {
    if (is.null(n)) {
      delta0 <- 0.5 - (amce * treat.prob)

      n <- (levels / 2) / (amce^2) * (qnorm(p = 1 - alpha / 2) + qnorm(p = power))^2 *
        (
          ((delta0 + amce) * (1 - (delta0 + amce))) / 0.5 +
            ((delta0) * (1 - delta0)) / 0.5
        )

      se <- (sqrt((
        ((delta0 + amce) * (1 - (delta0 + amce))) / 0.5 +
          ((delta0) * (1 - delta0)) / 0.5
      )) / sqrt(2 * (n / levels)))

      z <- amce / se

      pow <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

      type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / pow

      return(data.frame(amce = amce, n = n, type_s = type_s, power = power, alpha = alpha, levels = levels, delta0 = delta0))
    }

    else if (is.null(power)) {
      delta0 <- 0.5 - (amce * treat.prob)

      se <- (sqrt((
        ((delta0 + amce) * (1 - (delta0 + amce))) / 0.5 +
          ((delta0) * (1 - delta0)) / 0.5
      )) / sqrt(2 * (n / levels)))

      z <- amce / se

      power <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

      type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / power

      return(data.frame(power = power, type_s = type_s, amce = amce, n = n, alpha = alpha, levels = levels, delta0 = delta0))
    }
  }
  else if (is.null(n)) {
    delta0 <- 0.5 - (amce * treat.prob)

    n <- (levels / 2) / (amce^2) * (qnorm(p = 1 - alpha / 2) + qnorm(p = power))^2 *
      (
        ((delta0 + amce) * (1 - (delta0 + amce))) / 0.5 +
          ((delta0) * (1 - delta0)) / 0.5
      )

    se <- (sqrt((
      ((delta0 + amce) * (1 - (delta0 + amce))) / 0.5 +
        ((delta0) * (1 - delta0)) / 0.5
    )) / sqrt(2 * (n / levels)))

    z <- amce / se

    pow <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

    type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / pow

    exp_typeM <- vector(mode = "numeric", length = length_output)

    for (i in seq(1, length_output)) {
      est <- amce[i] + se[i] * rnorm(sims[i])

      sig <- abs(est) > se[i] * qnorm(1 - alpha[i] / 2)

      exp_typeM[i] <- abs(mean(abs(est)[sig]) / amce[i])
    }

    return(data.frame(n = n, type_s = type_s, exp_typeM = exp_typeM, amce = amce, power = power, alpha = alpha, levels = levels, delta0 = delta0))
  }

  else if (is.null(power)) {
    delta0 <- 0.5 - (amce * treat.prob)

    se <- (sqrt((
      ((delta0 + amce) * (1 - (delta0 + amce))) / 0.5 +
        ((delta0) * (1 - delta0)) / 0.5
    )) / sqrt(2 * (n / levels)))

    z <- amce / se

    power <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

    type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / power

    exp_typeM <- vector(mode = "numeric", length = length_output)

    for (i in seq(1, length_output)) {
      est <- amce[i] + se[i] * rnorm(sims[i])

      sig <- abs(est) > se[i] * qnorm(1 - alpha[i] / 2)

      exp_typeM[i] <- abs(mean(abs(est)[sig]) / amce[i])
    }

    return(data.frame(power = power, type_s = type_s, exp_typeM = exp_typeM, amce = amce, n = n, alpha = alpha, levels = levels, delta0 = delta0))
  }
}
