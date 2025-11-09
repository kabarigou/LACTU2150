#Illustration - Omitted variable bias

# Composition of the portfolio and observed claim frequencies
#------------------------------------------------------------
Expo   <- c(7000, 115000, 80000, 2000)
Cyl    <- c("Small", "Large", "Small", "Large")
Gender <- c("M", "M", "F", "F")
Nombre <- c(692, 11460, 7196, 194)

# Create data frame
df <- data.frame(
  Expo = Expo,
  Cyl = factor(Cyl, levels = c("Small", "Large")),
  Gender = factor(Gender, levels = c("F", "M")),
  Nombre = Nombre
)

# Poisson GLM analysis with Gender and Cylinder
GLMpoisson <- glm(
  Nombre ~ Gender + Cyl,
  offset = log(Expo),
  family = poisson(link = "log"),
  data = df
)

summary(GLMpoisson)

# Poisson GLM analysis without Gender
GLMpoissonb <- glm(
  Nombre ~ Cyl,
  offset = log(Expo),
  family = poisson(link = "log"),
  data = df
)

summary(GLMpoissonb)

# Check reference levels (base) for your factors
levels(df$Gender)  # First element is the base for Gender
levels(df$Cyl)     # First element is the base for Cylinder


# If we use the "base" function: 

# Composition of the portfolio and observed claim frequencies
#-------------------------------------------------------------
Expo   <- c(7000, 115000, 80000, 2000)
Cyl    <- c("Small", "Large", "Small", "Large")
Gender <- c("M", "M", "F", "F")
Nombre <- c(692, 11460, 7196, 194)

# Poisson GLM analysis with Gender and Cylinder
GLMpoisson <- glm(
  Nombre ~ C(as.factor(Gender), base=2) + C(as.factor(Cyl), base=1),
  offset = log(Expo),
  family = poisson(link = "log")
)

cat("Poisson GLM with Gender and Cylinder:\n")
summary(GLMpoisson)

# Poisson GLM analysis without Gender
GLMpoissonb <- glm(
  Nombre ~ C(as.factor(Cyl), base=1),
  offset = log(Expo),
  family = poisson(link = "log")
)

cat("\nPoisson GLM with Cylinder only:\n")
summary(GLMpoissonb)
