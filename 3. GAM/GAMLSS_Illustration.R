#GAMLSS Illustration

#GAMLSS for frequency modelling

# Install GAMLSS if not already installed
if (!requireNamespace("gamlss", quietly = TRUE)) {
  install.packages("gamlss")
}
library(gamlss)
library(dplyr)
library(tidyr)
library(ggplot2)


set.seed(123)

# Simulate policyholder features
n <- 10000
age <- rnorm(n, mean = 50, sd = 10)          # Policyholder age
age <- pmax(pmin(age, 70), 30) #bound between age 18 and 80

car_type <- factor(sample(c("sedan", "SUV", "truck"), n, replace = TRUE)) # Car type
region <- factor(sample(c("urban", "rural"), n, replace = TRUE)) # Region

# Simulate region-specific dispersion
sigma <- exp(ifelse(region == "urban", 0.3, 0.7))  # Dispersion varies by region

# Simulate claim counts using a Negative Binomial distribution
mu <- exp(1 + 0.02 * age + ifelse(car_type == "SUV", 0.3, 0) + ifelse(region == "urban", -0.5, 0))
claim_counts <- rnbinom(n, size = 1 / sigma, mu = mu)

# Combine into a data frame
insurance_data <- data.frame(age, car_type, region, claim_counts)


# Fit a GAMLSS model with Negative Binomial distribution
model <- gamlss(
  claim_counts ~ pb(age) + car_type + region,  # Mean model
  sigma.formula = ~ -1+region,                   # Dispersion model
  family = NBI(),                              # Negative Binomial family
  data = insurance_data
)

summary(model)



#Visualization

# # Predicted (fitted) claim counts
# fitted_claim_counts <- fitted(model, type = "response")

fitted_claim_counts <- fitted(model, type = "response")

# Create a data frame for plotting
plot_data <- data.frame(age = insurance_data$age, 
                        simulated = insurance_data$claim_counts,
                        fitted = fitted_claim_counts)



# Add the fitted claim counts to the data frame
insurance_data$fitted_claim_counts <- fitted_claim_counts

# Visualization: Scatter plot of observed vs. predicted claim counts
library(ggplot2)
ggplot(insurance_data, aes(x = claim_counts, y = fitted_claim_counts)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Observed vs. Predicted Claim Counts",
    x = "Observed Claim Counts",
    y = "Predicted Claim Counts"
  ) +
  theme_minimal()

# Create age groups for visualization
insurance_data <- insurance_data %>%
  mutate(age_group = cut(age, breaks = seq(30, 70, by = 5), include.lowest = TRUE))

# Aggregate observed and predicted values by age group
comparison_data <- insurance_data %>%
  group_by(age_group) %>%
  summarise(
    simulated = mean(claim_counts),
    predicted = mean(fitted_claim_counts)
  ) %>%
  pivot_longer(cols = c(simulated, predicted), names_to = "Type", values_to = "Counts")

# Side-by-side bar chart
p<-ggplot(comparison_data, aes(x = age_group, y = Counts, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
  scale_fill_manual(values = c("skyblue", "orange"), name = "Type") +
  labs(
    title = "Simulated vs. Predicted Claim Counts by Age Group",
    x = "Age Group",
    y = "Average Claim Counts"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p

# Save the plot as a PDF with 8.5 x 11 inches
ggsave("gamlss.pdf", plot = p, width = 11, height = 8.5, units = "in", device = "pdf")

