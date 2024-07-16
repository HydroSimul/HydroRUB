#' Evaluation metrics
#' @name evaluate_metrics
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{NSE_1970}{HydroRUB}
#' \insertRef{KGE_2009}{HydroRUB}
#' \insertRef{KGE_Kling_2012}{HydroRUB}
#' @description
#' Evatueltion matrics with several methods.
#'
#' @details
#' ## Mean Error (BIAS / ME)
#'
#' \mjsdeqn{
#' \mathrm{ME} = \frac{1}{N} \sum_{i=1}^N \left( S_i - O_i \right)
#' }
#'
#' ## Mean Absolute Error (MAE)
#'
#' \deqn{
#' \mathrm{MAE} = \frac{1}{N} \sum_{i=1}^N \left| S_i - O_i \right|
#' }
#'
#' ## Mean Squared Error (MSE)
#'
#' \deqn{
#' \mathrm{MSE} = \frac{1}{N} \sum_{i=1}^N \left( S_i - O_i \right)^2
#' }
#'
#' ## Root Mean Square Error (RMSE)
#'
#' \deqn{
#' \mathrm{RMSE} = \sqrt{ \frac{1}{N} \sum_{i=1}^N \left( S_i - O_i \right)^2 }
#' }
#'
#'
#' ## Coefficient of Determination (R2)
#' \deqn{
#' R^2 = \frac{\left( \sum_{i=1}^n (O_i - \bar{O})(S_i - \bar{S}) \right)^2}{\left( \sum_{i=1}^n (O_i - \bar{O})^2 \right) \cdot \left( \sum_{i=1}^n (S_i - \bar{S})^2 \right)}
#' }
#'
#' ## Nash-Sutcliffe Efficiency (NSE)
#'
#' from \insertCite{NSE_1970}{HydroRUB}
#'
#' \deqn{
#' \mathrm{NSE} = 1 - \frac{ \sum_{i=1}^N \left( S_i - O_i \right)^2 }{ \sum_{i=1}^N \left( O_i - \bar{O} \right)^2 }
#' }
#'
#' ## Kling-Gupta Efficiency (KGE)
#'
#' from \insertCite{KGE_2009,KGE_Kling_2012}{HydroRUB}
#'
#' \deqn{
#' \mathrm{KGE} = 1 - \mathrm{ED}
#' }
#'
#' \deqn{
#' \mathrm{ED} = \sqrt{ \left( s[1](r-1) \right)^2 + \left( s[2](\alpha-1) \right)^2 + \left( s[3](\beta-1) \right)^2 }
#' }
#'
#' where
#'
#' \deqn{
#' r = \frac{\text{cov}(S, O)}{\sigma_s \sigma_o}
#' }
#'
#' \deqn{
#' \alpha = \frac{\sigma_s}{\sigma_o}
#' }
#'
#' \deqn{
#' \beta = \frac{\mu_s}{\mu_o}
#' }
#'
#'
#'
#' @param num_Sim,num_Obs A numeric vector of simulated (\eqn{S_i}) and observed (\eqn{O_i}) values. Both vectors must have the same length.
#' @param num_Scal A numeric vector of length 3 with scaling factors for r, alpha, and beta in the `eva_KGE` calculation. Default is c(1, 1, 1).
#' @return A numeric value representing the calculated error metric.
#' @importFrom stats sd cor
#' @examples
#' sim <- rnorm(100)
#' obs <- rnorm(100)
#' eva_ME(sim, obs)
#' eva_MAE(sim, obs)
#' eva_MSE(sim, obs)
#' eva_RMSE(sim, obs)
#' eva_R2(sim, obs)
#' eva_NSE(sim, obs)
#' eva_KGE(sim, obs)
NULL

#' @rdname evaluate_metrics
#' @export
eva_ME <- function(num_Sim, num_Obs) {
  mean(num_Sim - num_Obs, na.rm = TRUE)
}

#' @rdname evaluate_metrics
#' @export
eva_MAE <- function(num_Sim, num_Obs) {
  mean(abs(num_Sim - num_Obs), na.rm = TRUE)
}

#' @rdname evaluate_metrics
#' @export
eva_MSE <- function(num_Sim, num_Obs) {
  mean((num_Sim - num_Obs)^2, na.rm = TRUE)
}

#' @rdname evaluate_metrics
#' @export
eva_RMSE <- function(num_Sim, num_Obs) {
  sqrt(mean((num_Sim - num_Obs)^2, na.rm = TRUE))
}

#' @rdname evaluate_metrics
#' @export
eva_R2 <- function(num_Sim, num_Obs) {
  mean_Sim <- mean(num_Sim, na.rm = TRUE)
  mean_Obs <- mean(num_Obs, na.rm = TRUE)
  numerator <- mean((num_Obs - mean_Obs) * (num_Sim - mean_Sim), na.rm = TRUE)^2
  denominator <- mean((num_Obs - mean_Obs)^2, na.rm = TRUE) * mean((num_Sim - mean_Sim)^2, na.rm = TRUE)
  numerator / denominator
}

#' @rdname evaluate_metrics
#' @export
eva_NSE <- function(num_Sim, num_Obs) {
  mean_Obs <- mean(num_Obs, na.rm = TRUE)
  1 - mean((num_Sim - num_Obs)^2, na.rm = TRUE) / mean((num_Obs - mean_Obs)^2, na.rm = TRUE)
}

#' @rdname evaluate_metrics
#' @export
eva_KGE <- function(num_Sim, num_Obs, num_Scal = c(1, 1, 1)) {
  # Remove NA values from both vectors
  valid_indices <- complete.cases(num_Sim, num_Obs)
  num_Sim <- num_Sim[valid_indices]
  num_Obs <- num_Obs[valid_indices]

  r_ <- cor(num_Sim, num_Obs)
  alpha_ <- sd(num_Sim) / sd(num_Obs)
  beta_ <- mean(num_Sim) / mean(num_Obs)

  1 - sqrt(num_Scal[1] * (r_ - 1)^2 +
             num_Scal[2] * (alpha_ - 1)^2 +
             num_Scal[3] * (beta_ - 1)^2)
}








