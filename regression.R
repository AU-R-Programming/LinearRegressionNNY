#Citation https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function

my_lm = function(response, covariates, alpha = 0.05) {

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
}

