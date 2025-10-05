#Illustration of Poisson log likelihood
library(ggplot2) #For plots
library(numDeriv) #To compute derivatives

# Data
claims <- c(0, 1, 2, 3, 4)
policy_counts <- c(12962, 1369, 157, 14, 3)
exposures <- c(10545.94, 1187.13, 134.66, 11.08, 2.52)
total_claims <- sum(claims * policy_counts)
total_exposure <- sum(exposures)
lambda_hat <- total_claims / total_exposure

#Compute loglikelihood
loglik <- function(lambda) {
  sum(
    -lambda * exposures +
      policy_counts * claims * log(lambda) +
      policy_counts * claims * log(exposures / policy_counts) -
      policy_counts * lfactorial(claims)
  )
}

lambdas <- seq(0.05, 0.3, length.out = 500)
loglik_values <- sapply(lambdas, loglik)

#Observed Fisher information
obs_info <- -hessian(loglik, lambda_hat)

#Curve approximation of the loglikelihood using Fisher information
curve_approx <- function(l) {
  loglik(lambda_hat) - 0.5 * obs_info * (l - lambda_hat)^2
}
quad_values <- curve_approx(lambdas)

#Build dataframe
df <- data.frame(
  lambda = lambdas,
  loglik = loglik_values,
  quad = quad_values
)

# For single MLE point
df_mle <- data.frame(
  lambda = lambda_hat,
  loglik = loglik(lambda_hat)
)

ggplot(df, aes(x = lambda)) +
  geom_line(aes(y = loglik), linewidth = 1.2, color = "black") +
  geom_line(aes(y = quad), linetype = "dashed", color = "blue", linewidth = 1) +
  geom_vline(xintercept = lambda_hat, color = "red", linetype = "dotted", linewidth = 1) +
  geom_point(data = df_mle, aes(x = lambda, y = loglik), color = "red", size = 3) +
  annotate("text", x = lambda_hat, y = loglik(lambda_hat), 
           label = "hat(lambda)", color = "red", hjust = -0.1, vjust = -0.5, size = 5, parse = TRUE) +
  labs(
    title = "Log-likelihood of Poisson Claim Frequency",
    x = expression(lambda),
    y = "Log-likelihood"
  ) +
  theme_minimal(base_size = 15) +
  annotate("text", x = lambda_hat+0.02, y = loglik(lambda_hat)-20,
           label = paste0("Observed Fisher info: ", round(obs_info, 2)),
           color = "blue", hjust = 0)

cat("Estimated lambda (MLE):", lambda_hat, "\n")
cat("Observed Fisher information:", obs_info, "\n")

#How does the likelihood change when we vary exposures ? 

library(ggplot2)

# Original data
claims <- c(0, 1, 2, 3, 4)
policy_counts <- c(12962, 1369, 157, 14, 3)
exposures <- c(10545.94, 1187.13, 134.66, 11.08, 2.52)

# Compute original lambda_hat and likelihood
lambda_hat <- sum(claims * policy_counts) / sum(exposures)

loglik <- function(lambda, exposures, claims, policy_counts) {
  sum(
    -lambda * exposures +
      policy_counts * claims * log(lambda) +
      policy_counts * claims * log(exposures / policy_counts) -
      policy_counts * lfactorial(claims)
  )
}

# Scaling scenarios
scenarios <- list(
  "Original" = list(scalar = 1),
  "x0.25" = list(scalar = 0.25),
  "x10" = list(scalar = 10)
)

lambdas <- seq(0.05, 0.3, length.out = 500)
plot_data <- data.frame()

#Compute log likelihood and relative loglikelihood in each scenario
for (sc in names(scenarios)) {
  s <- scenarios[[sc]]$scalar
  exp_scaled <- exposures * s
  pol_scaled <- policy_counts * s
  
  # log-likelihood for scaled scenario
  loglik_vals <- sapply(lambdas, loglik, exposures = exp_scaled, claims = claims, policy_counts = pol_scaled)
  ll_at_hat <- loglik(lambda_hat, exp_scaled, claims, pol_scaled)
  rel_ll <- loglik_vals - ll_at_hat
  plot_data <- rbind(plot_data,
                     data.frame(lambda = lambdas, logR = rel_ll, scenario = sc))
}

ggplot(plot_data, aes(x = lambda, y = logR, color = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = lambda_hat, linetype = "dotted", color = "red", linewidth = 1) +
  labs(
    title = expression(paste("Relative log-likelihood with fixed ", hat(lambda), " and scaled exposure/policies")),
    x = expression(lambda),
    y = expression(ln~R(lambda)),
    color = "Scenario"
  ) +
  theme_minimal(base_size = 15) +
  annotate("text", x = lambda_hat, y = max(plot_data$logR), 
           label = "hat(lambda)", color = "red", hjust = -0.1, vjust = -0.5, size = 5, parse = TRUE)
