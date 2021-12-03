#' @title LinearRegressionNNY
#'
#' @description Simple function that computes a linear regression model.
#' Citation https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function
#'
#' @param response A \code{vector} representing the response variable.
#' @param covariates A \code{matrix} representing the covariates.
#' @param alpha The \code{numeric} value representing the significance level.
#'
#' @return A \code{list} containing :
#' \describe{
#'     \item{beta}{Estimated coefficient, beta hat}
#'     \item{sigma2}{Residuals}
#'     \item{variance_beta}{variance of the beta estimator, beta hat}
#'     \item{ci}{confidence interval, derived from alpha}
#'     \item{ggplot}{plot function}
#' }
#'
#' @author Group 09 and SMAC Group: https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function.
#'
#' @export
#' @examples
#' linear_model(cars$speed, cars$distance)
#' linear_model(cars$speed, cars$distance, 0.025)
linear_model = function(response, covariates, alpha = 0.05) {

  # Make sure data formats are appropriate
  response <- as.vector(response)
  covariates <- as.matrix(covariates)

  # Define parameters
  n <- length(response)
  p <- dim(covariates)[2]
  df <- n - p

  # Estimate beta through Eq. (6.1)
  beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response

  # Estimate of the residual variance (sigma2) from Eq. (6.3)
  # Compute residuals
  resid <- response - covariates%*%as.matrix(beta.hat)
  sigma2.hat <- (1/df)*t(resid)%*%resid

  # Estimate of the variance of the estimated beta from Eq. (6.2)
  var.beta <- sigma2.hat*solve(t(covariates)%*%covariates)

  # Estimate of the confidence interval based on alpha
  quant <- 1 - alpha/2
  ci.beta <- c(beta.hat - qnorm(p = quant)*sqrt(var.beta), beta.hat +
                 qnorm(p = quant)*sqrt(var.beta))

  # Return all estimated values
  return(list(beta = beta.hat, sigma2 = sigma2.hat,
              variance_beta = var.beta, ci = ci.beta))

  # Residuals vs fitted-value
  RF=data.frame(response,resid)
  ggplot(data = RF,aes(response,resid))+
  goem_point()+
  goem_line()

  # qqplot of residuals
  qqplot(resid)
  # histogram of residuals
  hist(resid)
}




