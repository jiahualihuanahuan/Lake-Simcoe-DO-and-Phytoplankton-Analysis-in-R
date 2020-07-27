# Environmental vairbales analysis
# PCA, Cluster analysis, breakpoint analysis, Rodionov regime shift analysis (sequential algorithm)

# Laod Mann-Kendall trend test package
library("Kendall")
library("wq")

# 1 annual environmental variables
# Data
env.C1
env.C6
env.C9
env.E51
env.K39
env.K42
env.K45
env.S15

# Load environmental variables (each variable)
#Environmetal variables
# Set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/Environmetal data/csv files/individual env/by variable")


# Read files
Alk <- read.csv("Alkalinity.csv", header=T)
Chla <- read.csv("Chla.csv", header=T)
Cl <- read.csv("Cl.csv", header=T)
NH4 <- read.csv("NH4.csv", header=T)
NO3 <- read.csv("NO3.csv", header=T)
TKN <- read.csv("TKN.csv", header=T)
pH <- read.csv("pH.csv", header=T)
Si <- read.csv("Si.csv", header=T)
Ca <- read.csv("Ca.csv", header=T)
DO <- read.csv("DO.csv", header=T)
Secchi <- read.csv("Secchi.csv", header=T)
Temperature <- read.csv("Temperature.csv", header=T)
TP <- read.csv("TP.csv", header=T)
S <- read.csv("Stability.csv", header=T)
#------------------------------------
# modify date format
Alk$Date <- as.Date(Alk$Date, "%Y-%m-%d")
Chla$Date <- as.Date(Chla$Date, "%Y-%m-%d")
Cl$Date <- as.Date(Cl$Date, "%Y-%m-%d")
NH4$Date <- as.Date(NH4$Date, "%Y-%m-%d")
NO3$Date <- as.Date(NO3$Date, "%Y-%m-%d")
TKN$Date <- as.Date(TKN$Date, "%Y-%m-%d")
pH$Date <- as.Date(pH$Date, "%Y-%m-%d")
Si$Date <- as.Date(Si$Date, "%Y-%m-%d")
Ca$Date <- as.Date(Ca$Date, "%Y-%m-%d")
DO$Date <- as.Date(DO$Date, "%Y-%m-%d")
Secchi$Date <- as.Date(Secchi$Date, "%Y-%m-%d")
Temperature$Date <- as.Date(Temperature$Date, "%Y-%m-%d")
TP$Date <- as.Date(TP$Date, "%Y-%m-%d")
S$Date <- as.Date(S$Date, "%Y-%m-%d")

#----------------------------------------------
# Linear trend of each variables over years all sites (plot)|| same variable but different sites


# Alk
# Ca
# Chla
# Cl
# DO
# NH4
# NO3
# pH
# Secchi
# Si
# Stability
# Temperature
# TKN
# TN = TKN + NO3
# TON = TKN - NH4
# TP


# Plots and Mann-Kendall monotonic trend
env <- Stability
# can change this to other environmental variables
tau <- c()
p <- c()

par(mfrow = c(2, 4))
for (i in 1:8){
	plot(env[,c(1,i+3)], xlab="Year")
	env.lm <- lm(env[,i+3] ~ env[,1])
	summary(env.lm)
	abline(env.lm, col="red")
	tau[i] <- Kendall(env[,1], env[,i+3])[1]
	p[i] <- Kendall(env[,1], env[,i+3])[2]
	# MK <- mannKen(env[,c(i+3)], plot=T, type=c("slope", "pct","tau"), order =F)
}

tau
p

# ?mannKen


	# lines(lowess(na.omit(env[,c(1, i+3)])), pch=1)
	# plx<-predict(loess(env[,i+3] ~ Date, data= env), se=T)
	# lines(env$Date,plx$fit+2*plx$s, lty=2) #rough & ready CI
	# lines(env$Date,plx$fit-2*plx$s, lty=2)

	# Kendall(Date, env[,i+3])
	
#----------------------------------------------
# Column name
colnames(Alk)
colnames(Ca)
colnames(Cl)
colnames(Chla)
colnames(DO)
colnames(NH4)
colnames(NO3)
colnames(TKN)
colnames(pH)
colnames(Secchi)
colnames(Si)
colnames(S)
colnames(Temperature)
colnames(TP)

#---------------------------------------------
# 8 stations
C1 <- 4
C6 <- 5
C9 <- 6
E51 <- 7
K39 <- 8
K42 <- 9
K45 <- 10
S15 <- 11

site <-K42

#---------------------------------------------
# Divide into 12 plots in one graph
par(mfrow = c(4, 3))
par(mar=c(2,4,2,2))
#---------------------------------------------
# Alklinity
plot(Alk[,c(1,C1)], xlab="Year", ylab="CaCO3 (mg/L)", main="Alklinity")
lines(lowess(na.omit(Alk[,c(1, C1)])), pch=1)
points(Alk[,c(1,C6)], pch=2)
lines(lowess(na.omit(Alk[,c(1, C6)])))
points(Alk[,c(1,C9)], pch=3)
lines(lowess(na.omit(Alk[,c(1, C9)])))
points(Alk[,c(1,E51)], pch=4)
lines(lowess(na.omit(Alk[,c(1, E51)])))
points(Alk[,c(1,K39)], pch=5)
lines(lowess(na.omit(Alk[,c(1)])))
points(Alk[,c(1,K42)], pch=6)
lines(lowess(na.omit(Alk[,c(1, K42)])))
points(Alk[,c(1,K45)], pch=7)
lines(lowess(na.omit(Alk[,c(1, K45)])))
points(Alk[,c(1,S15)], pch=8)
lines(lowess(na.omit(Alk[,c(1, S15)])))
#---------------------------------------------
# Ca
plot(Ca[,c(1,C1)], xlab="Year", ylab="CaCO3 (mg/L)", main="Ca")
lines(lowess(na.omit(Ca[,c(1, C1)])), pch=1)
points(Ca[,c(1,C6)], pch=2)
lines(lowess(na.omit(Ca[,c(1, C6)])))
points(Ca[,c(1,C9)], pch=3)
lines(lowess(na.omit(Ca[,c(1, C9)])))
points(Ca[,c(1,E51)], pch=4)
lines(lowess(na.omit(Ca[,c(1, E51)])))
points(Ca[,c(1,K39)], pch=5)
lines(lowess(na.omit(Ca[,c(1)])))
points(Ca[,c(1,K42)], pch=6)
lines(lowess(na.omit(Ca[,c(1, K42)])))
points(Ca[,c(1,K45)], pch=7)
lines(lowess(na.omit(Ca[,c(1, K45)])))
points(Ca[,c(1,S15)], pch=8)
lines(lowess(na.omit(Ca[,c(1, S15)])))
#---------------------------------------------
plot(Cl[,c(1,C1)], xlab="Year", ylab="Cl (mg/L)", main="Cl")
lines(lowess(na.omit(Cl[,c(1, C1)])), pch=1)
points(Cl[,c(1,C6)], pch=2)
lines(lowess(na.omit(Cl[,c(1, C6)])))
points(Cl[,c(1,C9)], pch=3)
lines(lowess(na.omit(Cl[,c(1, C9)])))
points(Cl[,c(1,E51)], pch=4)
lines(lowess(na.omit(Cl[,c(1, E51)])))
points(Cl[,c(1,K39)], pch=5)
lines(lowess(na.omit(Cl[,c(1)])))
points(Cl[,c(1,K42)], pch=6)
lines(lowess(na.omit(Cl[,c(1, K42)])))
points(Cl[,c(1,K45)], pch=7)
lines(lowess(na.omit(Cl[,c(1, K45)])))
points(Cl[,c(1,S15)], pch=8)
lines(lowess(na.omit(Cl[,c(1, S15)])))
#---------------------------------------------
plot(Chla[,c(1,C1)], xlab="Year", ylab="Chla (mg/L)", main="Chla")
lines(lowess(na.omit(Chla[,c(1, C1)])), pch=1)
points(Chla[,c(1,C6)], pch=2)
lines(lowess(na.omit(Chla[,c(1, C6)])))
points(Chla[,c(1,C9)], pch=3)
lines(lowess(na.omit(Chla[,c(1, C9)])))
points(Chla[,c(1,E51)], pch=4)
lines(lowess(na.omit(Chla[,c(1, E51)])))
points(Chla[,c(1,K39)], pch=5)
lines(lowess(na.omit(Chla[,c(1)])))
points(Chla[,c(1,K42)], pch=6)
lines(lowess(na.omit(Chla[,c(1, K42)])))
points(Chla[,c(1,K45)], pch=7)
lines(lowess(na.omit(Chla[,c(1, K45)])))
points(Chla[,c(1,S15)], pch=8)
lines(lowess(na.omit(Chla[,c(1, S15)])))
#---------------------------------------------
plot(NH4[,c(1,C1)], xlab="Year", ylab="Total Ammonia (NH3+NH4, mg/L)", main="NH4")
lines(lowess(na.omit(NH4[,c(1, C1)])), pch=1)
points(NH4[,c(1,C6)], pch=2)
lines(lowess(na.omit(NH4[,c(1, C6)])))
points(NH4[,c(1,C9)], pch=3)
lines(lowess(na.omit(NH4[,c(1, C9)])))
points(NH4[,c(1,E51)], pch=4)
lines(lowess(na.omit(NH4[,c(1, E51)])))
points(NH4[,c(1,K39)], pch=5)
lines(lowess(na.omit(NH4[,c(1)])))
points(NH4[,c(1,K42)], pch=6)
lines(lowess(na.omit(NH4[,c(1, K42)])))
points(NH4[,c(1,K45)], pch=7)
lines(lowess(na.omit(NH4[,c(1, K45)])))
points(NH4[,c(1,S15)], pch=8)
lines(lowess(na.omit(NH4[,c(1, S15)])))
#---------------------------------------------
plot(NO3[,c(1,C1)], xlab="Year", ylab="Nitrite and Nitrate (NO2+NO3, mg/L)", main="NO3")
lines(lowess(na.omit(NO3[,c(1, C1)])), pch=1)
points(NO3[,c(1,C6)], pch=2)
lines(lowess(na.omit(NO3[,c(1, C6)])))
points(NO3[,c(1,C9)], pch=3)
lines(lowess(na.omit(NO3[,c(1, C9)])))
points(NO3[,c(1,E51)], pch=4)
lines(lowess(na.omit(NO3[,c(1, E51)])))
points(NO3[,c(1,K39)], pch=5)
lines(lowess(na.omit(NO3[,c(1)])))
points(NO3[,c(1,K42)], pch=6)
lines(lowess(na.omit(NO3[,c(1, K42)])))
points(NO3[,c(1,K45)], pch=7)
lines(lowess(na.omit(NO3[,c(1, K45)])))
points(NO3[,c(1,S15)], pch=8)
lines(lowess(na.omit(NO3[,c(1, S15)])))
#---------------------------------------------
plot(TKN[,c(1,C1)], xlab="Year", ylab="TKN (mg/L)", main="TKN")
lines(lowess(na.omit(TKN[,c(1, C1)])), pch=1)
points(TKN[,c(1,C6)], pch=2)
lines(lowess(na.omit(TKN[,c(1, C6)])))
points(TKN[,c(1,C9)], pch=3)
lines(lowess(na.omit(TKN[,c(1, C9)])))
points(TKN[,c(1,E51)], pch=4)
lines(lowess(na.omit(TKN[,c(1, E51)])))
points(TKN[,c(1,K39)], pch=5)
lines(lowess(na.omit(TKN[,c(1)])))
points(TKN[,c(1,K42)], pch=6)
lines(lowess(na.omit(TKN[,c(1, K42)])))
points(TKN[,c(1,K45)], pch=7)
lines(lowess(na.omit(TKN[,c(1, K45)])))
points(TKN[,c(1,S15)], pch=8)
lines(lowess(na.omit(TKN[,c(1, S15)])))
#---------------------------------------------
plot(pH[,c(1,C1)], xlab="Year", ylab="pH", main="pH")
lines(lowess(na.omit(pH[,c(1, C1)])), pch=1)
points(pH[,c(1,C6)], pch=2)
lines(lowess(na.omit(pH[,c(1, C6)])))
points(pH[,c(1,C9)], pch=3)
lines(lowess(na.omit(pH[,c(1, C9)])))
points(pH[,c(1,E51)], pch=4)
lines(lowess(na.omit(pH[,c(1, E51)])))
points(pH[,c(1,K39)], pch=5)
lines(lowess(na.omit(pH[,c(1)])))
points(pH[,c(1,K42)], pch=6)
lines(lowess(na.omit(pH[,c(1, K42)])))
points(pH[,c(1,K45)], pch=7)
lines(lowess(na.omit(pH[,c(1, K45)])))
points(pH[,c(1,S15)], pch=8)
lines(lowess(na.omit(pH[,c(1, S15)])))
#---------------------------------------------
plot(Secchi[,c(1,C1)], xlab="Year", ylab="Secchi depth (m)", ylim=c(0,15), main="Secchi")
lines(lowess(na.omit(Secchi[,c(1, C1)])), pch=1)
points(Secchi[,c(1,C6)], pch=2)
lines(lowess(na.omit(Secchi[,c(1, C6)])))
points(Secchi[,c(1,C9)], pch=3)
lines(lowess(na.omit(Secchi[,c(1, C9)])))
points(Secchi[,c(1,E51)], pch=4)
lines(lowess(na.omit(Secchi[,c(1, E51)])))
points(Secchi[,c(1,K39)], pch=5)
lines(lowess(na.omit(Secchi[,c(1)])))
points(Secchi[,c(1,K42)], pch=6)
lines(lowess(na.omit(Secchi[,c(1, K42)])))
points(Secchi[,c(1,K45)], pch=7)
lines(lowess(na.omit(Secchi[,c(1, K45)])))
points(Secchi[,c(1,S15)], pch=8)
lines(lowess(na.omit(Secchi[,c(1, S15)])))
#---------------------------------------------
plot(Si[,c(1,C1)], xlab="Year", ylab="Reactive silica (mg/L)", main="Si")
lines(lowess(na.omit(Si[,c(1, C1)])), pch=1)
points(Si[,c(1,C6)], pch=2)
lines(lowess(na.omit(Si[,c(1, C6)])))
points(Si[,c(1,C9)], pch=3)
lines(lowess(na.omit(Si[,c(1, C9)])))
points(Si[,c(1,E51)], pch=4)
lines(lowess(na.omit(Si[,c(1, E51)])))
points(Si[,c(1,K39)], pch=5)
lines(lowess(na.omit(Si[,c(1)])))
points(Si[,c(1,K42)], pch=6)
lines(lowess(na.omit(Si[,c(1, K42)])))
points(Si[,c(1,K45)], pch=7)
lines(lowess(na.omit(Si[,c(1, K45)])))
points(Si[,c(1,S15)], pch=8)
lines(lowess(na.omit(Si[,c(1, S15)])))
#---------------------------------------------
# plot(S[,c(1,C1)], xlab="Year", ylab="S (g cm/cm2)", main="S", ylim=c(0,3000), main="Stability")
# lines(lowess(na.omit(S[,c(1, C1)])), pch=1)
# points(S[,c(1,C6)], pch=2)
# lines(lowess(na.omit(S[,c(1, C6)])))
# points(S[,c(1,C9)], pch=3)
# lines(lowess(na.omit(S[,c(1, C9)])))
# points(S[,c(1,E51)], pch=4)
# lines(lowess(na.omit(S[,c(1, E51)])))
# points(S[,c(1,K39)], pch=5)
# lines(lowess(na.omit(S[,c(1)])))
# points(S[,c(1,K42)], pch=6)
# lines(lowess(na.omit(S[,c(1, K42)])))
# points(S[,c(1,K45)], pch=7)
# lines(lowess(na.omit(S[,c(1, K45)])))
# points(S[,c(1,S15)], pch=8)
# lines(lowess(na.omit(S[,c(1, S15)])))
#---------------------------------------------
plot(Temperature[,c(1,C1)], xlab="Year", ylab="Epilimnetic temperature (˚C)", ylim=c(0,28), main="Epi temperature")
lines(lowess(na.omit(Temperature[,c(1, C1)])), pch=1)
points(Temperature[,c(1,C6)], pch=2)
lines(lowess(na.omit(Temperature[,c(1, C6)])))
points(Temperature[,c(1,C9)], pch=3)
lines(lowess(na.omit(Temperature[,c(1, C9)])))
points(Temperature[,c(1,E51)], pch=4)
lines(lowess(na.omit(Temperature[,c(1, E51)])))
points(Temperature[,c(1,K39)], pch=5)
lines(lowess(na.omit(Temperature[,c(1)])))
points(Temperature[,c(1,K42)], pch=6)
lines(lowess(na.omit(Temperature[,c(1, K42)])))
points(Temperature[,c(1,K45)], pch=7)
lines(lowess(na.omit(Temperature[,c(1, K45)])))
points(Temperature[,c(1,S15)], pch=8)
lines(lowess(na.omit(Temperature[,c(1, S15)])))
#---------------------------------------------
plot(TP[,c(1,C1)], xlab="Year", ylab="Total Phosphorus (µg/L)", main="TP")
lines(lowess(na.omit(TP[,c(1, C1)])), pch=1)
points(TP[,c(1,C6)], pch=2)
lines(lowess(na.omit(TP[,c(1, C6)])))
points(TP[,c(1,C9)], pch=3)
lines(lowess(na.omit(TP[,c(1, C9)])))
points(TP[,c(1,E51)], pch=4)
lines(lowess(na.omit(TP[,c(1, E51)])))
points(TP[,c(1,K39)], pch=5)
lines(lowess(na.omit(TP[,c(1)])))
points(TP[,c(1,K42)], pch=6)
lines(lowess(na.omit(TP[,c(1, K42)])))
points(TP[,c(1,K45)], pch=7)
lines(lowess(na.omit(TP[,c(1, K45)])))
points(TP[,c(1,S15)], pch=8)
lines(lowess(na.omit(TP[,c(1, S15)])))

#----------------------------------------------
#----------------------------------------------
#----------------------------------------------
#----------------------------------------------
#----------------------------------------------
#---------------------------------------------#--------------------------------------------#---------------------------------------------#---------------------------------------------
# Plot all variables in one of the 8 sites in one graph

# Divide into 12 plots in one graph
par(mfrow = c(4, 3))
par(mar=c(2,4,1,2))

# 8 stations
C1 <- 4
C6 <- 5
C9 <- 6
E51 <- 7
K39 <- 8
K42 <- 9
K45 <- 10
S15 <- 11

site <- K42

plot(Alk[,c(1,site)], xlab="Year", ylab="Alklinity (mg/L)")
lines(lowess(na.omit(Alk[,c(1,site)])), col=1)

plot(Ca[,c(1,site)], xlab="Year", ylab="Ca (mg/L)")
lines(lowess(na.omit(Ca[,c(1,site)])), col=1)

plot(Cl[,c(1,site)], xlab="Year", ylab="Chloride (mg/L)")
lines(lowess(na.omit(Cl[,c(1,site)])), col=1)

plot(Chla[,c(1,site)], xlab="Year", ylab="Chlorophyll a (µg/L)")
lines(lowess(na.omit(Chla[,c(1,site)])), col=1)

plot(NH4[,c(1,site)], xlab="Year", ylab="Total Ammonia (NH3+NH4, mg/L)")
lines(lowess(na.omit(NH4[,c(1,site)])), col=1)

plot(NO3[,c(1,site)], xlab="Year", ylab="Nitrite and Nitrate (NO2+NO3, mg/L)")
lines(lowess(na.omit(NO3[,c(1,site)])), col=1)

plot(TKN[,c(1,site)], xlab="Year", ylab="TKN (mg/L)")
lines(lowess(na.omit(TKN[,c(1,site)])), col=1)

plot(pH[,c(1,site)], xlab="Year", ylab="pH")
lines(lowess(na.omit(pH[,c(1,site)])), col=1)

plot(Secchi[,c(1,site)], xlab="Year", ylab="Secchi depth (m)")
lines(lowess(na.omit(Secchi[,c(1,site)])), col=1)

plot(Si[,c(1,site)], xlab="Year", ylab="Reactive silica (mg/L)")
lines(lowess(na.omit(Si[,c(1,site)])), col=1)

plot(Temperature[,c(1,site)], xlab="Year", ylab="Epilimnetic temperature (˚C)")
lines(lowess(na.omit(Temperature[,c(1,site)])), col=1)

plot(TP[,c(1,site)], xlab="Year", ylab="Total Phosphorus (µg/L)")
lines(lowess(na.omit(TP[,c(1,site)])), col=1)

#------------------------------------
#------------------------------------

#------------------------------------

#------------------------------------
setwd("C:/Users/jiahu_000/Dropbox/R/Great textbook with R and data/Numerical Ecology with R/NEwR_functions_original/NumEcolR_functions")
source("evplot.R")
source("cleanplot.pca.R")

#------------------------------------
# PCA on env data
# C1
colnames(env.C1)
rownames(env.C1)

env.C1.pca <- rda(env.C1[,-2])
plot(env.C1.pca)
# Plot eigenvalues and % of variance for each axis
ev.C1 <- env.C1.pca$CA$eig
evplot(ev.C1)
# PCA biplots
cleanplot.pca(env.C1.pca, point=TRUE)

#------------------------------------
# C6
colnames(env.C6)
rownames(env.C6)

env.C6.pca <- rda(env.C6)
plot(env.C6.pca)
# Plot eigenvalues and % of variance for each axis
ev.C6 <- env.C6.pca$CA$eig
evplot(ev.C6)
# PCA biplots
cleanplot.pca(env.C6.pca, point=TRUE)

#------------------------------------
# C9
colnames(env.C9)
rownames(env.C9)

env.C9.pca <- rda(env.C9)
plot(env.C9.pca)
# Plot eigenvalues and % of variance for each axis
ev.C9 <- env.C9.pca$CA$eig
evplot(ev.C9)
# PCA biplots
cleanplot.pca(env.C9.pca, point=TRUE)

#------------------------------------
# E51
colnames(env.E51)
rownames(env.E51)

env.E51.pca <- rda(env.E51)
plot(env.E51.pca)
# Plot eigenvalues and % of variance for each axis
ev.E51 <- env.E51.pca$CA$eig
evplot(ev.E51)
# PCA biplots
cleanplot.pca(env.E51.pca, point=TRUE)

#------------------------------------
# K39
colnames(env.K39)
rownames(env.K39)

env.K39.pca <- rda(env.K39)
plot(env.K39.pca)
# Plot eigenvalues and % of variance for each axis
ev.K39 <- env.K39.pca$CA$eig
evplot(ev.K39)
# PCA biplots
cleanplot.pca(env.K39.pca, point=TRUE)

#------------------------------------
# K42
colnames(env.K42)
rownames(env.K42)

env.K42.pca <- rda(env.K42)
plot(env.K42.pca)
# Plot eigenvalues and % of variance for each axis
ev.K42 <- env.K42.pca$CA$eig
evplot(ev.K42)
# PCA biplots
cleanplot.pca(env.K42.pca, point=TRUE)

#------------------------------------
# K45
colnames(env.K45)
rownames(env.K45)

env.K45.pca <- rda(env.K45)
plot(env.K45.pca)
# Plot eigenvalues and % of variance for each axis
ev.K45 <- env.K45.pca$CA$eig
evplot(ev.K45)
# PCA biplots
cleanplot.pca(env.K45.pca, point=TRUE)

#------------------------------------
# S15
colnames(env.S15)
rownames(env.S15)

env.S15.pca <- rda(env.S15[-c(1:5),-c(2,4,9)])
plot(env.S15.pca)
# Plot eigenvalues and % of variance for each axis
ev.S15 <- env.S15.pca$CA$eig
evplot(ev.S15)
# PCA biplots
cleanplot.pca(env.S15.pca, point=TRUE)




#------------------------------------

# Cluster Analysis
env.w <- hclust(dist(scale(env.C1[,-2])), "ward")
plot(env.w)

# PCA correlation and cluster
gr <- cutree(env.w,k=2)
grl <- levels(factor(gr))

site.sc1 <- scores(env.pca, display="wa", scaling=1)

p <- plot(env.pca, display="wa", scaling=1, type="n", main="PCA correlation + clusters")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")

for (i in 1:length(grl)){
	points(site.sc1[gr==i,], pch=(14+i), cex=2, col=i+1)
}
text(site.sc1, row.names(env), cex=.7, pos=3)

ordicluster(p, env.w, col="dark grey")

#-----------------------------------
# Figure 8 in my Thesis
# PCA on K42 environmental variables
colnames(env.K42)
DM.f <- factor(env.K42.pca$DM)
DM.ff <- c("O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM","DM")
env.K42.pca <- data.frame(env.K42[,-c(14,17:46)],DM.ff)
colnames(env.K42.pca)
rownames(env.K42.pca)
summary(env.K42.pca)

# 
library(FactoMineR)
# ? FactoMineR
env.pca.f <- DMFA(env.K42.pca, num.fact=2)
# ?AFDM
# PCA on environmental variables
env.pca <- rda(env.K42.pca[,-17], scale=T)
summary(env.pca)
# Plot the PCA
plot(env.pca, main="PCA plot")
summary(env.pca)
env.scores <- read.csv("pca.env.scores.csv", header=T)
for(i in 1:16){
	text(env.scores[i,2], env.scores[i,3], labels=paste(env.scores[i,1]), col="blue")
}

arrows(0,0, env.scores[,2], env.scores[,3], length=0, lty=1, col="blue")

# Cluster Analysis
env.w <- hclust(dist(scale(env)), "ward")
plot(env.w)

# PCA correlation and cluster
gr <- cutree(env.w,k=2)
grl <- levels(factor(gr))

site.sc1 <- scores(env.pca, display="wa", scaling=1)

p <- plot(env.pca, display="wa", scaling=1, type="n", main="PCA correlation + clusters")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")

for (i in 1:length(grl)){
	points(site.sc1[gr==i,], pch=(14+i), cex=2, col=i+1)
}
text(site.sc1, row.names(env), cex=.7, pos=3)

ordicluster(p, env.w, col="dark grey")


# PCA on the environmental data set using PCA and biplot.PCA
setwd("/Users/jiahuali1991/Dropbox/R/Great textbook with R and data/Numerical Ecology with R/NEwR_functions_original/NumEcolR_functions")
source("PCA.R")

# PCA; scaling 1 is the default for biplots
env.PCA.PL1 <- PCA(env, stand=T)
biplot.PCA (env.PCA.PL1)
abline(h=0, lty=3)
abline(v=0, lty=3)
# The graphs may be mirro images of those obtained with vegan. This is unimportant since the choice of the sign of the pricipal components, made within the PCA functions, is arbitrary





