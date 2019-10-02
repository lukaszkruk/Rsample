#' Formula based on https://goo.gl/7oWC9F
#'
#' @param population Size of the entire population from which to draw the sample. Defaults to an arbitrarily large population, giving a recommend sample size for the worst case scenario.
#' @param confidence_interval Confidence that the interval within +/- margin of error contains the true parameter. Smaller interval requires larger sample size.
#' @param error_margin Statistic expressing the amount of random sampling error in a survey's results. The larger the margin of error, the less confidence one should have that the poll's reported results are close to the "true" figures. Smaller margin of error requires larger sample size.
#' @param response_distribution Expected distribution of responses, with 50/50 being the "worst case". Known expected distribution decreases the sample size.
#' @return The recommended sample size.
#' @examples
#'     rsample()
#'     rsample(population = 15000, confidence_interval = .9, error_margin = .1)
#'     {
#'       population = c(100, 200, 30000, 400000)
#'       sample_size_95_5 = rsample(population)
#'       sample_size_90_10 = rsample(population, confidence_interval = .9, error_margin = .1)
#'       data.frame(population, sample_size_95_5, sample_size_90_10)
#'     }
#'     plot(rsample(seq(1, 30000)) ~ seq(1, 30000), type = "l")
#' @export

rsample = function(population = 1000000000, confidence_interval = .95,
                   error_margin = .05, response_distribution = .5) {

  const = stats::qchisq(confidence_interval, 1) * response_distribution * (1-response_distribution)
  n = error_margin^2 * (population - 1)
  recommended_sample = population * const/(n+const)

  return(ceiling(recommended_sample))

}
