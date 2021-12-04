#' @title LinearRegressionplotNNY
#'
#' @description Simple function that plot a linear regression model.
#' Citation https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function
#'
#' @param response A \code{vector} representing the response variable.
#' @param resid A \code{vector} representing the covariates.
#' @param covariates A \code{matrix} representing the covariates.
#' @param alpha The \code{numeric} value representing the significance level.
#'
#'
#'
#' @author Group 09 and SMAC Group: https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function.
#'
#' @export
#' @examples
#' linear_model(cars$speed, cars$distance)
#' linear_model(cars$speed, cars$distance, 0.025)
linear_model_plot = function(response, covariates, alpha = 0.05) {

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

  # Residuals vs fitted-value
  RF=data.frame(response,resid)
  ggplot(data = RF,aes(response,resid))+
    goem_point()+
    goem_line()

  # qqplot of residuals
  qqnorm(resid)
  qqline(resid)
  # histogram of residuals
  hist(resid)
}




