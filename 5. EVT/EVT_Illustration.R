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
x <- seq(-2, 5, length.out = 500)

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

#Fit Pareto distribution to the tail of the dataset

library(CASdatasets)
library(POT)
library(dplyr)

#The dataset from the R package CASdatasets
#was collected by the reinsurer Secura Re Belgium 
#and comprises of 371 automobile claims from 1988 until 2001. 
data(besecura)

plot(Loss ~ Year, data= besecura, log="y", xlab="Year", 
     ylab="Claim size", main="Secura Re Belgian dataset")

# Count the number of data points by Year
data_by_year <- besecura %>%
  group_by(Year) %>%
  summarise(count = n())

# View the result
print(data_by_year)

# Filter the data for Year 1991
besecura_1991 <- besecura %>%
  filter(Year == 1991)

#Claim severities for the Year 1991
claim<-besecura_1991$Loss


#Pareto index plot
tcplot(claim)

#Gertensgarbe plot
tea::ggplot(claim)

# #Mean residual plot
# mrlplot(claim, u.range = c(1, quantile(claim, probs = 0.995)),
#         col = c("green", "black", "green"), nt = 200)


# Filter the data for Year 1991
besecura_1995 <- besecura %>%
  filter(Year == 1995)

#Claim severities for the Year 1991
claim<-besecura_1995$Loss


#Pareto index plot
tcplot(claim)

#Gertensgarbe plot
tea::ggplot(claim)

# #Mean residual plot
# mrlplot(claim, u.range = c(1, quantile(claim, probs = 0.995)),
#                  col = c("green", "black", "green"), nt = 200)

#Fit GPD distribution with all observations
claim<-besecura$Loss
output<-tea::ggplot(claim)

fitgpd(claim, threshold = output$threshold[1], 'mle')

