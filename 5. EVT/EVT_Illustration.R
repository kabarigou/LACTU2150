# Load libraries
library(ggplot2)
library(dplyr)

# Define the Generalized Extreme Value (GEV) Distribution function (vectorized)
gev_density <- function(x, xi) {
  ifelse(
    xi != 0,
    ifelse(1 + xi * x > 0, 
           (1 + xi * x)^(-1 / xi - 1) * exp(-(1 + xi * x)^(-1 / xi)), 
           0), 
    exp(-x) * exp(-exp(-x))
  )
}

# Generate data for the three cases
xi_values <- c(0.5, -0.5, 0) # Fréchet, Weibull, Gumbel
x <- seq(-5, 5, length.out = 500)

# Create a dataframe with densities for all distributions
df <- expand.grid(x = x, xi = xi_values) %>%
  rowwise() %>%  # Process rows one at a time for correct `xi` handling
  mutate(
    density = gev_density(x, xi),
    Distribution = case_when(
      xi > 0 ~ "Fréchet (Heavy-Tailed)",
      xi < 0 ~ "Weibull (Bounded Support)",
      xi == 0 ~ "Gumbel (Light-Tailed)"
    )
  ) %>%
  ungroup()  # Remove row-wise grouping

# Plot with ggplot2
ggplot(df, aes(x = x, y = density, color = Distribution, linetype = Distribution)) +
  geom_line(size = 1) +
  labs(
    title = "Comparison of Extreme Value Distributions",
    x = "x",
    y = "Density",
    color = "Distribution",
    linetype = "Distribution"
  ) +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

#GPD - Generalized Pareto Distribution


# Generalized Pareto density function
gpd_density <- function(y, xi, tau) {
  if (xi == 0) {
    return(dexp(y, rate = 1 / tau))  # Exponential distribution when xi = 0
  } else {
    support <- if (xi > 0) y >= 0 else y >= 0 & y < -tau / xi
    ifelse(support, (1 + xi * y / tau)^(-1 / xi - 1) / tau, 0)
  }
}

# Create a grid of y values and parameter combinations
y_values <- seq(0, 5, length.out = 100)
xi_values <- c(-0.5, 0, 0.5)  # Shape parameters
tau_values <- c(1, 2)         # Scale parameters

# Create a data frame with densities
df <- expand.grid(y = y_values, xi = xi_values, tau = tau_values) %>%
  mutate(
    density = mapply(gpd_density, y, xi, tau),
    xi_label = factor(xi, labels = paste("xi =", xi_values)),
    tau_label = factor(tau, labels = paste("tau =", tau_values))
  )

# Plot the densities
ggplot(df, aes(x = y, y = density, color = xi_label, linetype = tau_label)) +
  geom_line(size = 1) +
  labs(
    title = "Generalized Pareto Distributions",
    x = "y",
    y = "Density",
    color = "Shape (xi)",
    linetype = "Scale (tau)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme(legend.position = "top")

#Illustration Pareto vs. exponential tails 

library(ggplot2)

# Generate x values from 1 to 1000 on a log scale
x <- 10^(seq(0, 4, length.out = 500))

# Create data frame
df <- data.frame(
  x = rep(x, 2),
  tail = c(x^(-2), exp(-x / 200)),
  type = rep(c("Pareto tail ~ x^-2", "Exponential tail ~ exp(-x)"), each = length(x))
)

# Plot with ggplot2 (log–log scale)
ggplot(df, aes(x = x, y = tail, color = type)) +
  geom_line(size = 1.2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Log–log plot: Pareto vs Exponential Tails",
    x = "x",
    y = "Tail probability 1 - F(x)",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12)
  )



# Fit GPD to Bitcoin data - Example from the book "Quantitative Risk Management"

# libraries (install if necessary)
library(qrmdata)   # crypto dataset
library(qrmtools)  # POT helpers: mean_excess_plot, GPD_shape_plot, fit_GPD_MLE, qq_plot, qGPD
library(zoo)       # plot.zoo


## a) Data preparation and plots
data(crypto)
time <- c("2014-01-01", "2017-12-31")
time. <- paste0(time, collapse = "/")
BTC <- crypto[time., "BTC"]   # daily USD price of 1 BTC
X <- returns(BTC)             # log-returns
L <- -X                       # losses (negative log-returns)

# Visual checks
plot.zoo(BTC, xlab = "Time", ylab = "Price of 1 BTC in USD",
         main = "Bitcoin price (2014-01-01 to 2017-12-31)")
plot.zoo(L, main = "Bitcoin negative log-returns (losses)", xlab = "Time", ylab = "Loss")

# Mean excess plot of positive losses
mean_excess_plot(L[L > 0], xaxt = "n", yaxt = "n")
axis(1, at = seq(0, 0.25, by = 0.02))
axis(2, at = seq(0, 0.35, by = 0.02))

# Choose threshold u based on the plot:
u <- 0.064
abline(v = u, lty = 3, lwd = 1.6)

# Check stability of fitted shape xi across thresholds
GPD_shape_plot(L)
abline(v = u, lty = 4, lwd = 1.6, col = "royalblue3")
abline(h = 0.5, lty = 3, lwd = 1.6, col = "darkorange2") # xi >= 0.5 => infinite variance
abline(h = 1,   lty = 3, lwd = 1.6, col = "maroon3")    # xi >= 1 => infinite mean

## We see that our choice of u leads to a (roughly) stabilizing fitted GPD shape
## parameter (before confidence intervals get wider). Our choice of u leads to
## an infinite variance (but finite mean) model. Especially infinite variance
## models lie well between the pointwise asymptotic confidence intervals for
## thresholds such as u or larger.

## c) Fit GPD MLE on the excesses over threshold u
exceed <- L[L > u]         # exceedance values (original scale, > u)
excess <- exceed - u       # excesses (>= 0)
n_exc <- length(excess)
cat("Number of exceedances over u =", u, ":", n_exc, "\n")
cat("Fraction of data:", round(n_exc / length(L), 4), "\n")

fit <- fit_GPD_MLE(excess) # MLE on excesses
print(fit)
shape <- fit$par[["shape"]]
scale <- fit$par[["scale"]]
cat("Estimated shape (xi) =", shape, "\n")
cat("Estimated scale (beta) =", scale, "\n")

# Q-Q plot of excesses versus fitted GPD quantiles
# qq_plot from qrmtools expects the sample (here 'excess') and a function returning q for prob p
qq_plot(excess, FUN = function(p) qGPD(p, shape = shape, scale = scale),
        main = "Q-Q plot: empirical excesses vs fitted GPD")

## Plot empirical exceedance loss distribution function overlaid with the
## shifted fitted GPD
## Note: Replacing 'x-u' by 'x' would give the empirical excess distribution
##       function F[u](x) (= GPD(x))
res <- edf_plot(exceed, do.points = FALSE, ylab = "Exceedance loss distribution function")

# grid for overlay but only from u to max(exceed)
x_grid <- seq(u, max(exceed), length.out = 300)

# helper: GPD CDF for excess = x - u (works for shape ~ 0)
gpd_cdf <- function(x, shape, scale) {
  x <- pmax(0, x)
  if (abs(shape) < 1e-8) {
    1 - exp(-x / scale)
  } else {
    1 - (1 + shape * x / scale)^(-1 / shape)
  }
}

# compute GPD CDF values on x_grid (shifted: x - u)
y_gpd <- gpd_cdf(x_grid - u, shape = shape, scale = scale)

# overlay the fitted (shifted) GPD
lines(x_grid, y_gpd, col = "royalblue3", lwd = 2)
legend("bottomright", bty = "n", lty = c(1, 1), col = c("black", "royalblue3"),
       legend = c("empirical", expression(F[u](x-u)~"for"~x>=u~"and"~F[u]~"being the fitted GPD")))

# Secura POT analysis
#The dataset from the R package CASdatasets
#was collected by the reinsurer Secura Re Belgium 
#and comprises of 371 automobile claims from 1988 until 2001. 

# packages: install if needed
if(!requireNamespace("CASdatasets", quietly = TRUE)) install.packages("CASdatasets")
if(!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if(!requireNamespace("tea", quietly = TRUE)) install.packages("tea")  # you used tea::ggplot/tcplot

library(CASdatasets)
library(dplyr)
library(tea)
library(POT)


# data + quick plot
data(besecura)
plot(Loss ~ Year, data = besecura, log = "y", xlab = "Year",
     ylab = "Claim size", main = "Secura Re Belgian dataset")

# count by year
data_by_year <- besecura %>% group_by(Year) %>% summarise(count = n())
print(data_by_year)

# tail diagnostics on the sample
# Pareto index plot (tcplot from 'tea')
claim <- besecura$Loss
tcplot(claim)   # Pareto index / tail concentration plot

# Gertensgarbe-style plot (tea::ggplot returns threshold suggestions)
out <- tea::ggplot(claim)  # shows diagnostic plot and returns a list including thresholds
print(out$threshold)        # suggested thresholds (use first one or inspect visually)

# mean residual life plot (mrlplot from 'tea' / typical name in extreme packages)
mrlplot(claim, u.range = c(1, quantile(claim, probs = 0.995)), col = c("green","black","green"), nt = 200)

# Fit GPD on chosen threshold (use first suggested threshold from tea::ggplot)
u <- out$threshold[1]
cat("Using threshold u =", u, "\n")

# fitgpd: keep the same call you used but add method name explicitly
fit <- fitgpd(claim, threshold = u, "mle")
print(fit)

plot(fit,npy=1)
