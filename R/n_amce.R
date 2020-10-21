#' Calculate Minimum Required Sample Size for AMCEs in Conjoint Experiments
#'
#' Based on Schuessler/Freitag (2020), this function calculates the minimum effective
#' sample size (MES) required to reject a null hypothesis of no effect with probablity (power) if the alternative is true for a given average marginal component effect (AMCE) and a given alpha level.
#' It assumes a binary outcome variable and the AMCE recast in a regression setting using a two-sided t-test.
#' Using conservative assumptions, the MES can simply be divided by
#' the number of profiles x the number of tasks to optimize the design vis-a-vis
#' resource constraints. For recommended visual assessments of multiple design parameters
#' function_X is available. To calculate power for conditional AMCEs or interaction effects,
#' users are referred to function_Y. Keep in mind that calculating post-hoc power is close
#' to meaningless.
#' @param amce of a user-specified size.
#' @param alpha the significance level, 0.05 by default.
#' @param power power of the test (1 minus Type II error probability)
#' @param levels the number of levels of the treatment.
#' @param delta0 probability to choose a profile with the treatment set to 0 (reference level)
#' @keywords conjoint, power analysis, AMCE
#' @export
#' @export
#' @export
#' @export
#' @examples
#' n_amce()

n_amce <- function(amce, alpha = 0.05, power = 0.8, levels = 2,
                   delta0 = 0.5){

  (levels/2)/(amce^2) * (qnorm(p = 1- alpha/2) + qnorm(p = power))^2*
    (
      ((delta0 + amce)*(1 - (delta0 + amce)))/0.5 +
        ((delta0)*(1 - delta0))/0.5
    )
}


# Solve for power and amce
