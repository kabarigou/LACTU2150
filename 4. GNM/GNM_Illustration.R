#Extract mortality data

library(StMoMo)
library("demography")


# Save multiple objects
#save(dataBE, dataJPN, file = "mortalitydata.RData")

# Load the file later
load("mortalitydata.RData")


#Create free account on https://www.mortality.org/
dataBE <- hmd.mx(country = "BEL", username = "karim.barigou@uclouvain.be",
                 password = "XXXX", label = "Belgium")

dataJPN <- hmd.mx(country = "JPN", username = "karim.barigou@uclouvain.be",
                 password = "XXXX", label = "Japan")

#Remplacer par votre adresse courriel et mot de passe de votre compte HMD

#Extract male mortality and create a matrix of deaths and exposures
data <- StMoMoData(dataBE, series = "male")

#Number of ages and years we want to fit
ages.fit<-0:100
years.fit<-1950:2022

#Define log Poisson GNM and fit the Lee-Carter model using the StMoMo package
LC <- lc(link = "log")
LCfit <- fit(LC, data = data, ages.fit = ages.fit,years.fit=years.fit)

#Plot the fitted parameters alpha, beta and kappa from the Lee-Carter model
plot(LCfit, nCol = 3)


# Fit the Cairns-Blake-Dowd model
data_initial<-central2initial(data)
CBD <- cbd()
CBDfit <- fit(CBD, data = data_initial, ages.fit = ages.fit,years.fit=years.fit)

#deaths
deaths<-data_initial$Dxt

#Plot the fitted parameters
plot(CBDfit, parametricbx = FALSE)



