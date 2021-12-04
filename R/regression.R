#' @title Fit a linear model
#'
#' @description Simple function that computes a linear regression model.
#' Reference https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function
#'
#' @param response A \code{vector} representing the response variable.
#' @param covariates A \code{matrix} representing the covariates.
#' @param alpha The \code{numeric} value representing the significance level.
#'
#' @return A \code{list} containing :
#' \describe{
#'     \item{beta}{Estimated coefficient, beta hat}
#'     \item{sigma2}{Residual variance (MSE)}
#'     \item{ci}{confidence interval, derived from alpha}
#'     \item{resid}{residuals}
#'     \item{fitted}{fitted values}
#'     \item{r2}{R squared}
#'     \item{cp}{Mallows' Cp}
#'     \item{f}{F statistic}
#'     \item{pval}{P-Value}
#' }
#'
#' @author Group 09(Alex, Nan, Khanh - Stat 6210 Auburn Univ)
#' @references SMAC Group Book: https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function.
#' @export
#' @examples
#' linear_model(cars$speed, cars$distance)
#' linear_model(cars$speed, cars$distance, 0.025)
linear_model = function(response, covariates, alpha = 0.05) {

  # Make sure data formats are appropriate
  response <- as.vector(response)
  covariates <- as.matrix(covariates)
  covariates <- cbind(1,covariates)

  # Define parameters
  n <- length(response)
  p <- dim(covariates)[2]
  df <- n - p

  # Estimate beta through Eq. (6.1)
  beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response

  # Estimate of the residual variance (sigma2) from Eq. (6.3)
  # Compute residuals and residual variance
  fitted <- covariates%*%as.matrix(beta.hat)
  resid <- response - fitted
  sigma2.hat <- (1/df)*t(resid)%*%resid

  # Estimate of the variance of the estimated beta from Eq. (6.2)
  #var.beta <- sigma2.hat*solve(t(covariates)%*%covariates)
  var.beta <- as.vector(sigma2.hat)*solve(t(covariates)%*%covariates)

  # Estimate of the confidence interval based on alpha
  quant <- 1 - alpha/2
  ci.beta <- c(beta.hat - qnorm(p = quant)*sqrt(diag(var.beta)), beta.hat +
                 qnorm(p = quant)*sqrt(diag(var.beta)))

  # ci.beta <- c(beta.hat - 1.96 * sqrt(diag(var.beta)), beta.hat +
  #                1.96 * sqrt(diag(var.beta)))

  # Calculate SSE
  sse <- sum(resid^2)

  # Calculate SST
  sst <- sum((response - mean(response))^2)

  # Calculate R2
  r2 <- 1 - sse/sst

  # Calculate Mallows' Cp
  cp <- sse + 2*p*sigma2.hat

  # Calculate f statisitc
  f <- {(sst-sse)/(p-1)} / {sse/(n-p)}

  # Calcualte p value
  pval <- pf(f, p-1, n-p)

  # Return all estimated values
  return(list(beta = beta.hat,
              ci = ci.beta,
              resid = resid,
              fitted = fitted,
              sigma = sigma2.hat,
              r2 = r2, cp = cp,
              f = f, pval = pval))
}


#' Plot Model diagnostics
#' @description Function that takes in linear model attributes in form of list and produces several plots:
#'
#' Residual Vs Fitted Values
#'
#' QQPlot of Residuals
#'
#' Histogram of Residuals
#'
#' @param model A \code{list} from \code{linear_model} output.
#'
#' @author Alex, Nan, and Khanh - Stat 6210 Auburn Univ
#'
#' @export
#' @examples
#' model_plots(model_out)

model_plots = function(model){

  # Residuals vs fitted-value
  plot(model$resid ~ model$fitted, ylab = "Residuals", xlab = "Fitted",
       main = "Residuals Vs Fitted Values")
  abline(h=0)

  # QQplot of Residuals
  qqnorm(model$resid, ylab = "Residual Quantiles")

  # histogram of residuals
  hist(model$resid, xlab = "Residuals", main = "Histogram of Residuals")
}




