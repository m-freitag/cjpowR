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
#' the argument "treat.prob" can easily be manipulated. For instance, interacting a uniformly randomized treatment with a
#' binary co-variate with e.g. P_covariate(W_ijk = 0) = 4/5 yields the following list of probabilities: list(0.1,0.1,0.4,0.4). Details on the formula can be found in Schuessler/Freitag 2020.
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
#' @param treat.prob List of treatment probabilities. If the interest is in causal interaction and uniform randomization is employed, then p00=p10=p01=p11= 0.25. Thus, the default is list(0.25,0.25,0.25,0.25)
#' @param sims number of simulation runs to compute the expected type M error ("exaggeration ratio"), default 100k. If NULL, E(type M) is not computed.
#' @keywords conjoint, power analysis, AMCE
#' @export
#' @examples
#'
#' cjpowr_amcie(delta3 = 0.05, levels1=3, levels2=3, power=0.8)
#'
#' #Generating an interactive plot for type M error:
#'
#' cjpowr_amcie_vec <- Vectorize(cjpowr_amcie)
#' d <- expand.grid(delta3 = c(0.01, 0.02, 0.03, 0.05), n = seq(from = 100, to = 50000, length.out = 1000))
#' df <- t(cjpowr_amcie_vec(delta3 = d$delta3, n = d$n, sims = 10000, levels1 = 3, levels2=4, alpha = 0.05))
#' df <- data.frame(df)
#' df[] <- lapply(df, unlist)

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
                         treat.prob = list(p00 = 0.25, p10 = 0.25, p01 = 0.25, p11 = 0.25),
                         sims = 100000) {

  if (sum(sapply(list(power, n), is.null)) != 1) {
    stop("either 'n' or 'power' must be provided")
  }
  if (!is.null(alpha) && !is.numeric(alpha) || any(0 > alpha | alpha > 1)) {
    stop("'sig.level' must be numeric in [0, 1]")
  }

  delta2 <- -((-delta3 * treat.prob[[3]]) / (treat.prob[[3]] + treat.prob[[4]]))

  if (length(treat.prob) < 4) stop("length(treat.prob) < 4. Provide all of p00, p10, p01, p11.")

  if (sum(treat.prob[[1]] + treat.prob[[2]] + treat.prob[[3]] + treat.prob[[4]]) != 1) stop("Probabilities must sum to 1.")

  if (treat.prob[[1]] != treat.prob[[2]]) stop("p00 and p10 must be equal.")

  if (treat.prob[[3]] != treat.prob[[4]]) stop("p01 and p11 must be equal.")

  if (!is.list(treat.prob)) stop("Please provide a list of treatment probabilities. For readability you can name the elements as done in the defaults.")

  if (treat.prob[[1]] < treat.prob[[3]]) warning("More probability mass on p01 and p11 than on p00 and p10. This implies output is not a worst-case bound (but only marginally so). To obtain worst-case bounds, shift more mass to p00 and p10.")

  if (is.null(sims)) {

    if (is.null(n)) {

      if (all(sapply(list(length(delta0), length(delta1), length(delta3), length(alpha), length(power), length(levels1), length(levels2)), function(x) x == 1)) == FALSE) {
        stop("Please provide scalar values or apply a functional.")
      }

      A <- ((delta0) * (1 - delta0))
      B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
      C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
      D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

      n <- (levels1 * levels2 / 4) / (delta3^2) * (qnorm(p = 1 - alpha / 2) + qnorm(p = power))^2 *
        (A / treat.prob[[1]] +
          B / treat.prob[[2]] +
          C / treat.prob[[3]] +
          D / treat.prob[[4]])


      se <- (sqrt(A / treat.prob[[1]] +
        B / treat.prob[[2]] +
        C / treat.prob[[3]] +
        D / treat.prob[[4]])) / (sqrt(4 * (n / (levels1 * levels2))))

      z <- delta3 / se

      pow <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

      type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / pow


      return(data.frame(n = n, type_s, power = power, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
    } 
    else if (is.null(power)) {

      if (all(sapply(list(length(delta0), length(delta1), length(delta3), length(alpha), length(n), length(levels1), length(levels2)), function(x) x == 1)) == FALSE) {
        stop("Please provide scalar values or apply a functional.")
      }

      A <- ((delta0) * (1 - delta0))
      B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
      C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
      D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

      se <- (sqrt(A / treat.prob[[1]] +
        B / treat.prob[[2]] +
        C / treat.prob[[3]] +
        D / treat.prob[[4]])) / (sqrt(4 * (n / (levels1 * levels2))))

      z <- delta3 / se

      power <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

      type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / power

      return(data.frame(power = power, type_s, n = n, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
    }

  }
  else if (is.null(n)) {

    if (all(sapply(list(length(delta0), length(delta1), length(delta3), length(alpha), length(power), length(levels1), length(levels2)), function(x) x == 1)) == FALSE) {
      stop("Please provide scalar values or apply a functional.")
    }

    A <- ((delta0) * (1 - delta0))
    B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
    C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
    D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

    n <- (levels1 * levels2 / 4) / (delta3^2) * (qnorm(p = 1 - alpha / 2) + qnorm(p = power))^2 *
      (A / treat.prob[[1]] +
        B / treat.prob[[2]] +
        C / treat.prob[[3]] +
        D / treat.prob[[4]])


    se <- (sqrt(A / treat.prob[[1]] +
      B / treat.prob[[2]] +
      C / treat.prob[[3]] +
      D / treat.prob[[4]])) / (sqrt(4 * (n / (levels1 * levels2))))

    z <- delta3 / se

    pow <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

    type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / pow

    est <- delta3 + se * rnorm(sims)

    sig <- abs(est) > se * qnorm(1 - alpha / 2)

    exp_typeM <- abs(mean(abs(est)[sig]) / delta3)


    return(data.frame(n = n, type_s, exp_typeM, power = power, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
  }

  else if (is.null(power)) {

    if (all(sapply(list(length(delta0), length(delta1), length(delta3), length(alpha), length(n), length(levels1), length(levels2)), function(x) x == 1)) == FALSE) {
      stop("Please provide scalar values or apply a functional.")
    }

    A <- ((delta0) * (1 - delta0))
    B <- ((delta0 + delta1) * (1 - (delta0 + delta1)))
    C <- ((delta0 + delta1 + delta2) * (1 - (delta0 + delta1 + delta2)))
    D <- ((delta0 + delta1 + delta2 + delta3) * (1 - (delta0 + delta1 + delta2 + delta3)))

    se <- (sqrt(A / treat.prob[[1]] +
      B / treat.prob[[2]] +
      C / treat.prob[[3]] +
      D / treat.prob[[4]])) / (sqrt(4 * (n / (levels1 * levels2))))

    z <- delta3 / se

    power <- pnorm(z - qnorm(1 - alpha / 2)) + pnorm(-z - qnorm(1 - alpha / 2))

    type_s <- pnorm(-z - qnorm(1 - alpha / 2)) / power

    est <- delta3 + se * rnorm(sims)

    sig <- abs(est) > se * qnorm(1 - alpha / 2)

    exp_typeM <- abs(mean(abs(est)[sig]) / delta3)


    return(data.frame(power = power, type_s, exp_typeM, n = n, delta3 = delta3, delta2 = delta2, delta1 = delta1, alpha = alpha, levels1 = levels1, levels2 = levels2, delta0 = delta0))
  }
}
