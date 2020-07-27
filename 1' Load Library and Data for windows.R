# Load library and data

# 1 Load library

# 1.1 Install library
#install.packages("ade4")
#install.packages("packfor") # this is not working
#install.packages("MASS")
#install.packages("ellipse")
#install.packages("SparseM")
#install.packages("car")

# 1.2 Load library
# Load required packages
library(vegan)
library(ade4)
library(packfor)
library (MASS)
library(ellipse)
library(FactoMineR)
library(car)

setwd("C:/Users/jiahu_000/Dropbox/R/Great textbook with R and data/Numerical Ecology with R/NEwR_functions_original/NumEcolR_functions")

source("evplot.R")
source("cleanplot.pca.R")
#---------------------
# 2 Load phytoplankton species composition data

# 2.1 phyto annual
# set working directory: phytoplankton annual
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/Phytoplankton data/csv files/annual ice-free")


# Load annual (ice-free period) phyto data 
# all annaul phyto
phyto.annual.all <- read.csv("phyto all sites 1980-2012.csv", header=T, row.names=1)
#colnames(phyto.C1)
phyto.annual.all.2p <- phyto.annual.all[,c(1:11)] 
# #11 2% up; #16 1% up
#colnames(phyto.annual.all.2p)

phyto.annual.all.1p <- phyto.annual.all[,c(1:16)] 
# #11 2% up; #16 1% up
#colnames(phyto.annual.all.1p)
#---------------------
# C1
phyto.C1 <- read.csv("C1 1980-2008.csv", header=T, row.names=1)
#colnames(phyto.C1)
phyto.C1.2p <- phyto.C1[,c(1:11)]
phyto.C1.1p <- phyto.C1[,c(1:19)]
#11 2% up; #19 1% up

# C6
phyto.C6 <- read.csv("C6 1980-2009.csv", header=T, row.names=1)
#colnames(phyto.C6)
phyto.C6.2p <- phyto.C6[,c(1:12)]
phyto.C6.1p <- phyto.C6[,c(1:19)]
#12 2% up; #19 1% up

# C9
phyto.C9 <- read.csv("C9 1980-2008.csv", header=T, row.names=1)
#colnames(phyto.C9)
phyto.C9.2p <- phyto.C9[,c(1:10)]
phyto.C9.1p <- phyto.C9[,c(1:14)]
#10 2% up; #14 1% up

# E51
phyto.E51 <- read.csv("E51 1980-2008.csv", header=T, row.names=1)
#colnames(phyto.E51)
phyto.E51.2p <- phyto.E51[,c(1:11)]
phyto.E51.1p <- phyto.E51[,c(1:15)]
#11 2% up; #15 1% up

# K39
phyto.K39 <- read.csv("K39 1980-2008.csv", header=T, row.names=1)
#colnames(phyto.K39)
phyto.K39.2p <- phyto.K39[,c(1:9)]
phyto.K39.1p <- phyto.K39[,c(1:14)]
# 9 2% up; # 14 1% up

# K42: 1980-2008
phyto.K42 <- read.csv("K42 1980-2008.csv", header=T, row.names=1)
#colnames(phyto.K42)
phyto.K42.2p <- phyto.K42[,c(1:10)]
phyto.K42.1p <- phyto.K42[,c(1:16)]
# 10 2% up; # 16 1% up

# K42: 1980-2012
phyto.K42.2012 <- read.csv("K42 1980-2012.csv", header=T, row.names=1)
#colnames(phyto.K42)
phyto.K42.2012.2p <- phyto.K42.2012[,c(1:10)]
phyto.K42.2012.1p <- phyto.K42.2012[,c(1:16)]
# 10 2% up; # 16 1% up

# K45
phyto.K45 <- read.csv("K45 1980-2008.csv", header=T, row.names=1)
#colnames(phyto.K45)
phyto.K45.2p <- phyto.K45[,c(1:10)]
phyto.K45.1p <- phyto.K45[,c(1:15)]
#10 2% up; #15 1% up

# S15
phyto.S15 <- read.csv("S15 1984-2008.csv", header=T, row.names=1)
#colnames(phyto.S15)
phyto.S15.2p <- phyto.S15[,c(1:11)]
phyto.S15.1p <- phyto.S15[,c(1:15)]
#11 2% up; #15 1% up

#------------------------------------------------------
# 2.2 phyto seasonal
# set working directory: phytoplankton seasonal (summer and fall)
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/Phytoplankton data/csv files/seasonal")

# Load phyto data
phyto.C1.S <- read.csv("C1.S.csv", header=T, row.names=1)
#colnames(phyto.C1.S)
#rownames(phyto.C1.S)
phyto.C1.S <- phyto.C1.S[,c(1:14)]

phyto.E51.S <- read.csv("E51.S.csv", header=T, row.names=1)
#colnames(phyto.E51.S)
#rownames(phyto.E51.S)
phyto.E51.S <- phyto.E51.S[,c(1:15)]

phyto.K42.S <- read.csv("K42.S.csv", header=T, row.names=1)
#colnames(phyto.K42.S)
#rownames(phyto.K42.S)
phyto.K42.S <- phyto.K42.S[,c(1:13)]

phyto.C1.F <- read.csv("C1.F.csv", header=T, row.names=1)
#colnames(phyto.C1.F)
#rownames(phyto.C1.F)
phyto.C1.F <- phyto.C1.F[,c(1:10)]

phyto.E51.F <- read.csv("E51.F.csv", header=T, row.names=1)
#colnames(phyto.E51.F)
#rownames(phyto.E51.F)
phyto.E51.F <- phyto.E51.F[,c(1:10)]

phyto.K42.F <- read.csv("K42.F.csv", header=T, row.names=1)
#colnames(phyto.K42.F)
#rownames(phyto.K42.F)
phyto.K42.F <- phyto.K42.F[,c(1:9)]

phyto.all <- read.csv("C1 E51 K42 S F.csv", header=T, row.names=1)
#colnames(phyto.all)
#rownames(phyto.all)
phyto.seasonal.all.2p <- phyto.all[,c(1:14)]
phyto.seasonal.all.1p <- phyto.all[,c(1:24)]

phyto.C1.S.F <- read.csv("C1 S F.csv", header=T, row.names=1)
#colnames(phyto.C1.S.F)
#rownames(phyto.C1.S.F)
phyto.C1.S.F.2p <- phyto.C1.S.F[,c(1:12)]
phyto.C1.S.F.1p <- phyto.C1.S.F[,c(1:14)]

phyto.E51.S.F <- read.csv("E51 S F.csv", header=T, row.names=1)
#colnames(phyto.E51.S.F)
#rownames(phyto.E51.S.F)
phyto.E51.S.F.2p <- phyto.E51.S.F[,c(1:12)]
phyto.E51.S.F.1p <- phyto.E51.S.F[,c(1:20)]

phyto.K42.S.F <- read.csv("K42 S F.csv", header=T, row.names=1)
#colnames(phyto.K42.S.F)
#rownames(phyto.K42.S.F)
phyto.K42.S.F.2p <- phyto.K42.S.F[,c(1:10)]
phyto.K42.S.F.1p <- phyto.K42.S.F[,c(1:20)]

#---------------------
#---------------------
#---------------------
#---------------------
#---------------------
#---------------------
#---------------------
#---------------------

#---------------------
# 3 Load environmental variables data
# 3.1 env annual 
# set working directory: env annual (ice-free period) 
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/Environmetal data/csv files/Annual")

# Load annual (ice-free period) env data
# all annual env
env.annual.all <- read.csv("env all sites 1980-2012.csv", header=T, row.names=1)
#colnames(env.annual.all)
#rownames(env.annual.all)


# Load annual (ice-free period) env data
# C1
env.C1 <- read.csv("C1 1980-2012.csv", header=T, row.names=1)
#colnames(env.C1)

# C6
env.C6 <- read.csv("C6 1980-2012.csv", header=T, row.names=1)
#colnames(env.C6)

# C9
env.C9 <- read.csv("C9 1980-2012.csv", header=T, row.names=1)
#colnames(env.C9)

# E51
env.E51 <- read.csv("E51 1980-2012.csv", header=T, row.names=1)
#colnames(env.E51)

# K39
env.K39 <- read.csv("K39 1980-2012.csv", header=T, row.names=1)
#colnames(env.K39)

# K42
env.K42 <- read.csv("K42 1980-2012.csv", header=T, row.names=1)
#colnames(env.K42)

# K45
env.K45 <- read.csv("K45 1980-2012.csv", header=T, row.names=1)
#colnames(env.K45)

# S15
env.S15 <- read.csv("S15 1985-2012.csv", header=T, row.names=1)
#colnames(env.S15)

#---------------------
# 3.2 env seasonal
# set working directory: env variables seasonal (summer and fall)
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/Environmetal data/csv files/Seasonal")

# Load env data
env.C1.S <- read.csv("C1.S.csv", header=T, row.names=1)
#colnames(env.C1.S)
#rownames(env.C1.S)

env.E51.S <- read.csv("E51.S.csv", header=T, row.names=1)
#colnames(env.E51.S)
#rownames(env.E51.S)

env.K42.S <- read.csv("K42.S.csv", header=T, row.names=1)
#colnames(env.K42.S)
#rownames(env.K42.S)

env.C1.F <- read.csv("C1.F.csv", header=T, row.names=1)
#colnames(env.C1.F)
#rownames(env.C1.F)

env.E51.F <- read.csv("E51.F.csv", header=T, row.names=1)
#colnames(env.E51.F)
#rownames(env.E51.F)

env.K42.F <- read.csv("K42.F.csv", header=T, row.names=1)
#colnames(env.K42.F)
#rownames(env.K42.F)

env.seasonal.all.S.F <- read.csv("C1 E51 K42 S F.csv", header=T, row.names=1)
#colnames(env.seasonal.all.S.F)
#colnames(env.seasonal.all.S.F)

env.C1.S.F <- read.csv("C1 S F.csv", header=T, row.names=1)
#colnames(env.C1.S.F)
#colnames(env.C1.S.F)

env.E51.S.F <- read.csv("E51 S F.csv", header=T, row.names=1)
#colnames(env.E51.S.F)
#colnames(env.E51.S.F)

env.K42.S.F <- read.csv("K42 S F.csv", header=T, row.names=1)
#colnames(env.K42.S.F)
#colnames(env.K42.S.F)


#---------------------

#---------------------
# 4 Climate Variables
# 4.1 sunspot cycle and large climatic oscillation indices
# set working directory: Large-climatic cycle indices
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Oscillation and Fluctuation of Climate")

# Load Large-climatic cycle indices data
lcci <- read.csv("All Oscillation index.csv", header=T, row.names=1)
#colnames(lcci)
#rownames(lcci)

# 4.1.2 seasonal sunspot cycle and large climatic oscillation indices
# set working directory: Large-climatic cycle indices
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/seasonal data/csv")

# Load Large-climatic cycle indices data
lcci.S.F <- read.csv("seasonal data.csv", header=T, row.names=1)
#colnames(lcci.S.F)
#rownames(lcci.S.F)

#---------------------
# 4.2 air temperature (Shanty Bay), Wind speed (Moskoka) and percipitation (unknown, data not ready)
# Load temperature data
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Temperature profile and Stability/csv files")

temp <-read.csv("air and lake water temperature.csv",header=T, row.names=1)
head(temp) 
#colnames(temp)

# note that water temperatures are for K42 only, so, only use air in subsequent analysis
temp.air <- temp[,c(1,5:22)]
#colnames(temp.air)
#rownames(temp.air)

#---------------------
# 4.3 Ice-phenology data
# Set working directory
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/Ice phenology data")

# load ice data
ice <- read.csv("Lake Simcoe Ice Phenology Data 1853-2013.csv", header=T, row.names=1)
#colnames(ice)
#rownames(ice)

# study period 1980-2012
ice.s <- ice[c(129:161),]
#colnames(ice.s)
#rownames(ice.s)

#---------------------
# 5 Moran's Eigenvector maps of Time series analysis
# MEM variables data
# Set working directory
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 2 Phytoplankton analysis/Data/MEM")

# load ice data
MEM <- read.csv("MEM variables.csv", header=T, row.names=1)
#colnames(MEM)
#rownames(MEM)


# PCNM analysis
# Generate transect points
tr33 <- 1980:2012
# Euclidean distance matrix
tr33.d1 <- dist(tr33)
# truncation distance set to 1
thresh <- 1
# Truncation to threshold 1
tr33.d1[tr33.d1 > thresh] <- 4*thresh
# PCoA of truncated matrix 
tr33.PCoA <- cmdscale(tr33.d1, eig=TRUE, k=length(tr33)-1)
# Count the positive eigenvalues
(nb.ev <- length(which(tr33.PCoA$eig > 0.0000001)))
# Matrix of PCNM variables
tr33.PCNM <- tr33.PCoA$points[,1:nb.ev]

#rownames(tr33.PCNM) <- 1980:2012
#colnames(tr33.PCNM) <- c("")

tr33.PCNM.2003 <- tr33.PCNM[-24,]
tr33.PCNM.1981.2003 <- tr33.PCNM[-c(2,24),]
#colnames(tr33.PCNM) <- 

#rownames(tr33.PCNM) 

# Plot some PCNM variables modelling positive spatial correlation (Fig. 7.3)
#quartz(title="PCNM variables (transect)")
#par(mfrow=c(2,2))
#somePCNM <- c(1,2,3,4)
#for(i in 1:length(somePCNM)){
#	plot(tr33.PCNM[,somePCNM[i]], type="l", ylab=c("PCNM", somePCNM[i]))
#}

# write.csv(tr33.PCNM, "my.file.csv")

# Combine temporal patterns and environmental variables
# tr33.PCNM.std <- decostand(tr33.PCNM[-24,], "standardize")
# env.temp <- cbind(tr33.PCNM.std, env.std)