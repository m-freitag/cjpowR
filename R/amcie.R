#' Calculate Power or Minimum Required Sample Size for AMCIEs/Conditional AMCEs in Conjoint Experiments
#'
#' Based on Schuessler/Freitag (2020), this function calculates the minimum effective
#' sample size (MES) required to reject a null hypothesis of no effect with probablity (power) if the alternative is true for a given average marginal component interaction effect (AMCIE) and a given alpha level.
#' Function can be manipulated to give power for a given effective sample size. Type S (incorrect sign)and the expected type M errors (exaggeration ratio) are also computed (Lu/Qui/Deng 2019).
#' The effective sample size is the number of profiles times the number of tasks. The outcome is assumed to be binary (forced-choice).
#'
#' This function assumes a binary outcome variable and the AMCIE recast in a regression setting using a two-sided t-test.
#' Using conservative assumptions, the MES can simply be divided by
#' the number of profiles times the number of taks to optimize the design vis-a-vis
#' resource constraints. The number of attributes does not affect power.
#'
#' To calculate power or minimum sample size for differences in conditional AMCEs, given the known or estimated marginal distribution of the pre-treatment co-variate,
#' the arguments p00, p10, p01, p11 can easily be manipulated. For instance, interacting a uniformly randomized treatment with a
#' binary co-variate with e.g. P_covariate(W_ijk = 0) = 4/5 yields the following probabilities: p00 = 0.1, p10 = 0.1, p01 = 0.4, p11 = 0.4. Details on the formula can be found in Schuessler/Freitag 2020.
#' Shifting more probability mass on p01 and p11 (as in this example) results in marginally higher MES / lower power (see the technical Appendix of the paper).
#'
#' Passing the effective sample size to the function gives power.
#' Type S (incorrect sign)and the expected type M errors (exaggeration ratio) are also computed (Lu/Qui/Deng 2019). If your study is
#' not completely underpowered, you do not need to worry about type S error. However, only high powered
#' studies result in low E(type M errors).
#'
#' Power for AMCEs can be calculated with \link[cjpowR]{cjpowr_amce}.
#' 
#' @param delta3 amcie or conditional amce of user specified size.
#' @param delta1 default is 0.
#' @param delta0 default is 0.5.
#' @param alpha the significance level, 0.05 by default.
#' @param power power of the test (1 minus Type II error probability), default is NULL.
#' @param n effective sample size, default is NULL.
#' @param levels1 the number of levels of the first treatment. Default is 2.
#' @param levels2 the number of levels of the second treatment/subgroup variable. Default is 2.
#' @param p00 Treatment probability p00. If the interest is in causal interaction and uniform randomization is employed, then p00=p10=p01=p11= 0.25.
#' @param p10 Treatment probability p10. If the interest is in causal interaction and uniform randomization is employed, then p00=p10=p01=p11= 0.25.
#' @param p01 Treatment probability p01. If the interest is in causal interaction and uniform randomization is employed, then p00=p10=p01=p11= 0.25.
#' @param p11 Treatment probability p11. If the interest is in causal interaction and uniform randomization is employed, then p00=p10=p01=p11= 0.25.
#' @param sims number of simulation runs to compute the expected type M error ("exaggeration ratio"), default 100k. If NULL, E(type M) is not computed.
#' @keywords conjoint, power analysis, AMCE
#' @export
#' @examples
#'
#' cjpowr_amcie(delta3 = 0.05, levels1=3, levels2=3, power=0.8)
#'
#' #Generating an interactive plot for type M error:
#'
#' d <- expand.grid(
#'     delta3 = c(0.01, 0.02, 0.03, 0.05), 
#'     n = seq(from = 100, to = 50000, length.out = 1000),
#'     delta0 = 0.5, 
#'     delta1 = 0,
#'     alpha = 0.05,
#'     levels1 = 3, 
#'     levels2 = 4,
#'     p00 = 0.25, p10 = 0.25, p01 = 0.25, p11 = 0.25,
#'     sims = 100000 #set to 0 if Type-M-Error is not of interest
#'     )
#' 
#' df <- list2DF(do.call(cjpowr_amcie, d))
#' 
#' plot_ly(df, x = ~n, y = ~exp_typeM, type = 'scatter', mode = 'lines', linetype = ~delta3) %>%
#'  layout(
#'    xaxis = list(title = "Effective Sample Size",
#'                 zeroline = F,
#'                 hoverformat = '.0f'),
#'    yaxis = list(title = "Exaggeration Ratio",
#'                 range = c(0,10),
#'                 zeroline = F,
#'                 hoverformat = '.2f'),
#'    legend=list(title=list(text='<b> AMCIE </b>')),
#'    hovermode = "x unified"
#'  )
#' @section Literature:
#'
#' 1. Schuessler, J. and M. Freitag (2020). Power Analysis for Conjoint Experiments.
#'    Working Paper.
#'
#' 2. Lu, J., Y. Qiu, and A. Deng (2019). A note on Type S/M errors in hypothesis testing.
#'    British Journal of Mathematical and Statistical Psychology 72(1), 1–17.

cjpowr_amcie <- function(delta0 = 0.5, delta1 = 0,
                         delta3, alpha = 0.05, power = NULL, n = NULL,
                         levels1 = 2, levels2 = 2,
                         p00 = 0.25, p10 = 0.25, p01 = 0.25, p11 = 0.25,
                         sims = 100000) {

  if (sum(sapply(list(power, n), is.null)) != 1) {
    stop("either 'n' or 'power' must be provided")
  }
  if (!is.null(alpha) && !is.numeric(alpha) || any(0 > alpha | alpha > 1)) {
    stop("'sig.level' must be numeric in [0, 1]")
  }

  delta2 <- -((-delta3 * p01) / (p01 + p11))

  if (is.null(p00) | is.null(p10) | is.null(p01) | is.null(p11)) stop("Provide all of p00, p10, p01, p11.")

  if (any((p00 + p10 + p01 + p11) != 1)) stop("Probabilities must sum to 1.")

  if (any(p00 != p10)) stop("p00 and p10 must be equal.")

  if (any(p01 != p11)) stop("p01 and p11 must be equal.")

  if (any(p00 < p01)) warning("Note: More probability mass on p01 and p11 than on p00 and p10. This implies output is not a worst-case bound (but only marginally so). To obtain worst-case bounds, shift more mass to p00 and p10.")

  lengths <- c(length(delta0), length(delta1), length(delta3), length(alpha), length(power), length(levels1), length(levels2), length(p00), length(p10), length(p01), length(p11), length(n), length(sims))
  
  non_null_lengths <- lengths[lengths > 0]

  if (length(unique(non_null_lengths)) > 1) {
    stop("All non-NULL input parameters must be of the same length.")
  }

  length_output <- unique(non_null_lengths)

  if (is.null(sims) | any(sims == 0) == TRUE) {

    if (is.null(n)) {

      A <- ((delta0) * (1 - delta0))
      B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
      C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
      D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

      n <- (levels1 * levels2 / 4) / (delta3^2) * (qnorm(p = 1 - alpha / 2) + qnorm(p = power))^2 *
        (A / p00 +
          B / p10 +
          C / p01 +
          D / p11)


      se <- (sqrt(A / p00 +
        B / p10 +
        C / p01 +
        D / p11)) / (sqrt(4 * (n / (levels1 * levels2))))

      z <- delta3 / se

      pow <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

      type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / pow


      return(data.frame(n = n, type_s, power = power, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
    } 
    else if (is.null(power)) {

      A <- ((delta0) * (1 - delta0))
      B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
      C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
      D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

      se <- (sqrt(A / p00 +
        B / p10 +
        C / p01 +
        D / p11)) / (sqrt(4 * (n / (levels1 * levels2))))

      z <- delta3 / se

      power <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

      type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / power

      return(data.frame(power = power, type_s, n = n, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
    }

  }
  else if (is.null(n)) {

    A <- ((delta0) * (1 - delta0))
    B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
    C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
    D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

    n <- (levels1 * levels2 / 4) / (delta3^2) * (qnorm(p = 1 - alpha / 2) + qnorm(p = power))^2 *
      (A / p00 +
        B / p10 +
        C / p01 +
        D / p11)


    se <- (sqrt(A / p00 +
      B / p10 +
      C / p01 +
      D / p11)) / (sqrt(4 * (n / (levels1 * levels2))))

    z <- delta3 / se

    pow <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

    type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / pow

    exp_typeM <- vector(mode = "numeric", length = length_output)

    for (i in seq(1, length_output)) {
      est <- delta3[i] + se[i] * rnorm(sims[i])

      sig <- abs(est) > se[i] * qnorm(1 - alpha[i] / 2)

      exp_typeM[i] <- abs(mean(abs(est)[sig]) / delta3[i])
    }

    return(data.frame(n = n, type_s, exp_typeM, power = power, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
  }

  else if (is.null(power)) {

    A <- ((delta0) * (1 - delta0))
    B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
    C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
    D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

    se <- (sqrt(A / p00 +
      B / p10 +
      C / p01 +
      D / p11)) / (sqrt(4 * (n / (levels1 * levels2))))

    z <- delta3 / se

    power <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

    type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / power

    exp_typeM <- vector(mode = "numeric", length = length_output)

    for (i in seq(1, length_output)) {
      est <- delta3[i] + se[i] * rnorm(sims[i])

      sig <- abs(est) > se[i] * qnorm(1 - alpha[i] / 2)

      exp_typeM[i] <- abs(mean(abs(est)[sig]) / delta3[i])
    }


    return(data.frame(power = power, type_s, exp_typeM, n = n, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
  }
}
