# This file updated only up to C1 station
# Due to genus with one division has the dominant genus change we can tell enough interpretive information
#-------------------------------------
# 6.1 Phyto annual composite vs Env (all 8 sites individually)
# RDA: annual phyto ~ env
# 6.1.1: C1
### env + log
head(env.C1)
env.C1[14,12] <- 0.000000001
env.C1.sel <- env.C1[,-c(1,2,4,7,9)]
env.C1.log <- log(env.C1.sel[,-10])
envpluslog.C1 <- cbind(env.C1.sel, env.C1.log)
colnames(envpluslog.C1)[11:19] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.C1)
rownames(envpluslog.C1)
head(envpluslog.C1)

### Transformation
# Phytoplankton species composition
phyto.hel.C1 <- decostand(phyto.C1.division, "hellinger")
colnames(phyto.hel.C1)
rownames(phyto.hel.C1)

# combine variables
env.all.C1 <- cbind(envpluslog.C1, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.C1 <-decostand(env.all.C1[,], "standardize")
colnames(env.std.C1)
rownames(env.std.C1)

### forward selection
# a) env 
forward.sel(phyto.hel.C1, envpluslog.C1[-c(24,30:33),-c(17,16)], alpha=0.3, nperm=9999) 
#DM, S, TP, logNH4, Si
# R2adj=30%

# b) indices
forward.sel(phyto.hel.C1, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa
# R2adj=15.7%


# c) air temp
forward.sel(phyto.hel.C1, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AirTemp6
# R2adj=9.4%

# d) MEM
forward.sel(phyto.hel.C1, MEM[-c(24,30:33),-1], alpha=0.1, nperm=9999) 
# 8.67, 24, 17 year cycle
# R2adj=17.3%

# e) all data (without MEM)
forward.sel(phyto.hel.C1, env.std.C1[-c(24,30:33),], alpha=0.06, nperm=9999) 
#DM, S, 8.6 yc, SSn, 3.56 and 11 yc
# R2adj=42.78%

# Select only significant env variables
colnames(env.std.C1)
env.std.sel.C1 <- env.std.C1[-c(24,30:33),c(10,7,72,20,83,70)]
colnames(env.std.sel.C1)
# RDA analysis with only selected
phyto.rda.sel.C1 <- rda(phyto.hel.C1 ~ ., data= env.std.sel.C1)
summary(phyto.rda.sel.C1)

vif(phyto.rda.sel.C1)

coef(phyto.rda.sel.C1)
(R2 <- RsquareAdj(phyto.rda.sel.C1)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C1)$adj.r.squared)

plot(phyto.rda.sel.C1, scaling=2)
phyto.sc <- scores(phyto.rda.sel.C1, choices=1:2, scaling=2, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C1, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C1, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.C1$CA$eig[phyto.rda.sel.C1$CA$eig > mean(phyto.rda.sel.C1$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.std.C1)
colnames(env.std.sel.C1)
env.std.sel.C1 <- env.std.C1[-c(24,30:33),c(10,14,24,45,16,7)]

env.w <- env.std.sel.C1[,c(2,4,5,6)]
temp <- env.std.sel.C1[,c(3)]
DM <- env.std.sel.C1[,c(1)]
phyto.part.all <- varpart(phyto.hel.C1, env.w,temp, DM)
plot(phyto.part.all)
phyto.part.all


#---------------------
# 5.1.2: C6
### env + log
head(env.C6)
env.C6.sel <- env.C6[,-c(1,2,4,7,9)]
env.C6.log <- log(env.C6.sel[,-10])
envpluslog.C6 <- cbind(env.C6.sel, env.C6.log)
colnames(envpluslog.C6)[11:19] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.C6)
rownames(envpluslog.C6)
head(envpluslog.C6)

### Transformation
# Phytoplankton species composition
phyto.hel.C6 <- decostand(phyto.C6s, "hellinger")
colnames(phyto.hel.C6)
rownames(phyto.hel.C6)

# combine variables
env.all.C6 <- cbind(envpluslog.C6, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.C6 <-decostand(env.all.C6[,], "standardize")
colnames(env.std.C6)
rownames(env.std.C6)

### forward selection

# a) env 
forward.sel(phyto.hel.C6, envpluslog.C6[-c(24,31:33),], alpha=0.13, nperm=9999) 
# DM, logSi, Stability, logNTK, TP

# b) indices
forward.sel(phyto.hel.C6, lcci[-c(24,31:33),-c(17,10)], alpha=0.1, nperm=9999) 
# AMOa, SSN., PDOi

# c) air temp
forward.sel(phyto.hel.C6, temp.air[-c(24,31:33),], alpha=0.05, nperm=9999) 
# AirTemp9

# d) MEM
forward.sel(phyto.hel.C6, MEM[-c(24,31:33),], alpha=0.05, nperm=9999) 
# 34,11,10,5,14,4.57 years cycle

# e) all data
forward.sel(phyto.hel.C6, env.std.C6[-c(24,31:33),], alpha=0.05, nperm=9999) 
# DM, SSN, logSi, AirTemp2, Stability, QBOa, 7-year-cycle
# R2adj=51.8%

# Select only significant env variables
colnames(env.std.C6)
colnames(env.std.sel.C6)
env.std.sel.C6 <- env.std.C6[-c(24,31:33),c(10,20,16,50,7,22,74)]

colnames(env.std.sel.C6)[c(4,7)] <- c("AirTempFeb","7-year-cycle")
colnames(env.std.sel.C6)

# RDA analysis with only selected variables
phyto.rda.sel.C6 <- rda(phyto.hel.C6 ~ ., data=env.std.sel.C6)
summary(phyto.rda.sel.C6)

vif(phyto.rda.sel.C6)

coef(phyto.rda.sel.C6)
(R2 <- RsquareAdj(phyto.rda.sel.C6)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C6)$adj.r.squared)

plot(phyto.rda.sel.C6, scaling=1)
phyto.sc <- scores(phyto.rda.sel.C6, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C6, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C6, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.C6$CA$eig[phyto.rda.sel.C6$CA$eig > mean(phyto.rda.sel.C6$CA$eig)]


### Variation partitioning with two sets of explanatory variables
# split variables first
env.w <- env.std.sel.C6[,c(3,4,5)]
temp <- env.std.sel.C6[,c(2,6,7)]
DM <- env.std.sel.C6[,1]
phyto.part.all <- varpart(phyto.hel.C6, env.w, temp,DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.3: C9
### env + log
head(env.C9)
env.C9.sel <- env.C9[,-c(1,2,4,7,9)]
env.C9.log <- log(env.C9.sel[,-10])
envpluslog.C9 <- cbind(env.C9.sel, env.C9.log)
colnames(envpluslog.C9)[11:19] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.C9)
rownames(envpluslog.C9)
head(envpluslog.C9)

### Transformation
# Phytoplankton species composition
phyto.hel.C9 <- decostand(phyto.C9s, "hellinger")
colnames(phyto.hel.C9)
rownames(phyto.hel.C9)
phyto.hel.C9[,c(1:10)]

# combine variables
env.all.C9 <- cbind(envpluslog.C9, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.C9 <-decostand(env.all.C9[,], "standardize")
rownames(env.std.C9)
colnames(env.std.C9)

### forward selection
# a) env 
forward.sel(phyto.hel.C9, envpluslog.C9[-c(24,30:33),], alpha=0.17, nperm=9999) 
#DM, logSi, logTP, Secchi

# b) indices
forward.sel(phyto.hel.C9, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, SSN, PDOi

# c) air temp
forward.sel(phyto.hel.C9, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AirTemp9 and AirTemp6

# d) MEM
forward.sel(phyto.hel.C9, MEM[-c(24,30:33),], alpha=0.05, nperm=9999) 
# 34, 11, 10, xxx year cycle

# e) all data
forward.sel(phyto.hel.C9, env.std.C9[-c(24,30:33),-c(66:87)], alpha=0.07, nperm=9999) 
# DM, SSN, AT4, logTP
# logSi, Annual AirTemp
# ice-out date

# Select only significant env variables (env only)
colnames(env.std.C9)

env.std.sel.C9 <- env.std.C9[-c(24,30:33),c(10,20,52,19,16,43)]

colnames(env.std.sel.C9)
colnames(env.std.sel.C9)[c(3)] <- c("AirTempApril")
colnames(env.std.sel.C9)

# RDA analysis with only selected
phyto.rda.sel.C9 <- rda(phyto.hel.C9 ~ ., data= env.std.sel.C9)
summary(phyto.rda.sel.C9)

vif(phyto.rda.sel.C9)

coef(phyto.rda.sel.C9)
(R2 <- RsquareAdj(phyto.rda.sel.C9)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C9)$adj.r.squared)

plot(phyto.rda.sel.C9, scaling=1)
phyto.sc <- scores(phyto.rda.sel.C9, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C9, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C9, by="axis", step=1000)


### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.std.C9)

env.w <- env.std.sel.C9[,c(3,4,5,6)]
temp <- env.std.sel.C9[,2]
DM <-env.std.sel.C9[,1]
phyto.part.all <- varpart(phyto.hel.C9, env.w, temp, DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.4: E51
### env + log
head(env.E51)
env.E51.sel <- env.E51[,-c(1,2,4,7,9)]
env.E51.log <- log(env.E51.sel[,-10])
envpluslog.E51 <- cbind(env.E51.sel, env.E51.log)
colnames(envpluslog.E51)[11:19] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.E51)
rownames(envpluslog.E51)
head(envpluslog.E51)

### Transformation
# Phytoplankton species composition
phyto.hel.E51 <- decostand(phyto.E51s, "hellinger")
colnames(phyto.hel.E51)
rownames(phyto.hel.E51)

# combine variables
env.all.E51 <- cbind(envpluslog.E51, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.E51 <-decostand(env.all.E51[,], "standardize")
colnames(env.std.E51)
rownames(env.std.E51)

### forward selection

# a) env 
forward.sel(phyto.hel.E51, envpluslog.E51[-c(24,30:33),], alpha=0.05, nperm=9999) 
# DM, Chl.a and Si

# b) indices
forward.sel(phyto.hel.E51, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa

# c) air temp
forward.sel(phyto.hel.E51, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AirTemp9 and AirTemp6

# d) MEM
forward.sel(phyto.hel.E51, MEM[-c(24,30:33),-1], alpha=0.05, nperm=9999) 
# 11 years cycle

# e) all data
forward.sel(phyto.hel.E51, env.std.E51[-c(24,30:33),], alpha=0.05, nperm=9999) 
# DM, 11,4.57 years cycle, SSN, AT9, 24-y-c, logChla, QBOa, MEIf, AT11

# f) all data without MEM
forward.sel(phyto.hel.E51, env.std.E51[-c(24,30:33),-c(66:87)], alpha=0.05, nperm=9999) 
# DM, 11,4.57 years cycle, SSN, AT9, 24-y-c, logChla, QBOa, MEIf, AT11

# Select only significant env variables
colnames(env.std.E51)
env.std.sel.E51 <- env.std.E51[-c(24,30:33),c(10,1,16)]
# RDA analysis with only selected
phyto.rda.sel.E51 <- rda(phyto.hel.E51 ~ ., data= env.std.sel.E51)
summary(phyto.rda.sel.E51)

vif(phyto.rda.sel.E51)

coef(phyto.rda.sel.E51)
(R2 <- RsquareAdj(phyto.rda.sel.E51)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.E51)$adj.r.squared)

plot(phyto.rda.sel.E51, scaling=2)
phyto.sc <- scores(phyto.rda.sel.E51, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.E51, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.E51, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.E51$CA$eig[phyto.rda.sel.E51$CA$eig > mean(phyto.rda.sel.E51$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.std.sel.E51)

env.w <- env.std.sel.E51[,c(2,3)]
DM <-env.std.sel.E51[,1]
phyto.part.all <- varpart(phyto.hel.E51, env.w, DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.5: K39
### env + log
head(env.K39)
env.K39.sel <- env.K39[,-c(1,2,4,7,9)]
env.K39.log <- log(env.K39.sel[,-10])
envpluslog.K39 <- cbind(env.K39.sel, env.K39.log)
colnames(envpluslog.K39)[11:19] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.K39)
rownames(envpluslog.K39)
head(envpluslog.K39)

### Transformation
# Phytoplankton species composition
phyto.hel.K39 <- decostand(phyto.K39s, "hellinger")
colnames(phyto.hel.K39)
rownames(phyto.hel.K39)

# combine variables
env.all.K39 <- cbind(envpluslog.K39, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.K39 <-decostand(env.all.K39, "standardize")
colnames(env.std.K39)
rownames(env.std.K39)

### forward selection
# a) env
forward.sel(phyto.hel.K39, envpluslog.K39[-c(24,30:33),-11], alpha=0.05, nperm=9999) 
# DM, Chla, T

# b) indices
forward.sel(phyto.hel.K39, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, PDOi, AMOf, SSN

# c) air temp
forward.sel(phyto.hel.K39, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AT9

# d) MEM
forward.sel(phyto.hel.K39, MEM[-c(24,30:33),-1], alpha=0.06, nperm=9999) 
# 5, 17 years cycle

# e) all data without MEM
forward.sel(phyto.hel.K39, env.std.K39[-c(24,30:33),-c(11,66:87,12,2)], alpha=0.05, nperm=9999) 
# 


# Select only significant env variables
colnames(env.std.K39)
env.std.sel.K39 <- env.std.K39[-c(24,30:33),c(10,1,22,19,29,20)] 
colnames(env.std.sel.K39)

# RDA analysis with only selected
phyto.rda.sel.K39 <- rda(phyto.hel.K39 ~ ., data= env.std.sel.K39)
summary(phyto.rda.sel.K39)

vif(phyto.rda.sel.K39)

coef(phyto.rda.sel.K39)
(R2 <- RsquareAdj(phyto.rda.sel.K39)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K39)$adj.r.squared)

plot(phyto.rda.sel.K39, scaling=2)
phyto.sc <- scores(phyto.rda.sel.K39, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K39, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K39, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K39$CA$eig[phyto.rda.sel.K39$CA$eig > mean(phyto.rda.sel.K39$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.std.sel.K39)

env.w <- env.std.sel.K39[,c(2,4)]
temp <- env.std.sel.K39[,c(3,5,6)]
DM <-env.std.sel.K39[,1]
phyto.part.all <- varpart(phyto.hel.K39, env.w, temp,DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.6: K42 1980-2008
### env + log
head(env.K42)
env.K42.sel <- env.K42[,-c(4)]
#env.K42.log <- log(env.K42.sel[,-10])
envpluslog.K42 <- cbind(env.K42.sel)
colnames(envpluslog.K42)
rownames(envpluslog.K42)
head(envpluslog.K42)


### Transformation
# Phytoplankton species composition
phyto.hel.K42 <- decostand(phyto.K42.2p, "hellinger")
colnames(phyto.hel.K42)
rownames(phyto.hel.K42)

# combine variables
env.all.K42 <- cbind(envpluslog.K42, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.K42 <-decostand(env.all.K42[,], "standardize")
colnames(env.std.K42)
rownames(env.std.K42)

### forward selection
# a) env data
forward.sel(phyto.hel.K42, envpluslog.K42[-c(30:33),-c(10,11,26,45)], alpha=0.26, nperm=9999) # DM, Si, T, S, TP, NO3

# b) indices
forward.sel(phyto.hel.K42, lcci[-c(30:33),-17], alpha=0.05, nperm=9999) # AMOa

# c) air temp
forward.sel(phyto.hel.K42, temp.air[-c(30:33),], alpha=0.05, nperm=9999) # AirTemp9

# d) MEM
forward.sel(phyto.hel.K42, MEM[-c(30:33),], alpha=0.05, nperm=9999) # 34,11,5,3.67 years cycle

# e) all data without MEM
forward.sel(phyto.hel.K42, env.std.K42[-c(30:33),-c(92:113)], alpha=0.14, nperm=9999)
# 

# Select only significant variables
colnames(env.std.K42)
env.std.sel.K42 <- envpluslog.K42[,c(13,9,11,22,12)]
colnames(env.std.sel.K42)
# RDA analysis with only selected
phyto.rda.sel.K42 <- rda(phyto.hel.K42 ~ ., data=env.std.sel.K42[-c(30:33),])
#summary(phyto.rda.sel.K42)

vif(phyto.rda.sel.K42)

coef(phyto.rda.sel.K42)
(R2 <- RsquareAdj(phyto.rda.sel.K42)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K42)$adj.r.squared)

plot(phyto.rda.sel.K42, scaling=1)
phyto.sc <- scores(phyto.rda.sel.K42, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K42, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K42, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K42$CA$eig[phyto.rda.sel.K42$CA$eig > mean(phyto.rda.sel.K42$CA$eig)]

#---------------------
# 5.1.6.2: K42 1980-2012
### env + log
head(env.K42)
env.K42.sel <- env.K42[,-c(4)]
#env.K42.log <- log(env.K42.sel[,-10])
envpluslog.K42 <- cbind(env.K42.sel)
colnames(envpluslog.K42)
rownames(envpluslog.K42)
head(envpluslog.K42)

### Transformation
# Phytoplankton species composition
phyto.hel.K42 <- decostand(phyto.K42.2012, "hellinger")
colnames(phyto.hel.K42)
rownames(phyto.hel.K42)

# combine variables
env.all.K42 <- cbind(envpluslog.K42, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.K42 <-decostand(env.all.K42[,], "standardize")
colnames(env.std.K42)
rownames(env.std.K42)

### forward selection
# a) env data
forward.sel(phyto.hel.K42, envpluslog.K42[,], alpha=0.07, nperm=9999) # DM Si

# b) indices
forward.sel(phyto.hel.K42, lcci[,], alpha=0.05, nperm=9999) # AMOa, PDOi

# c) air temp
forward.sel(phyto.hel.K42, temp.air[,], alpha=0.05, nperm=9999) # Annual Air Temp

# d) MEM
forward.sel(phyto.hel.K42, MEM[,-c(1,2)], alpha=0.05, nperm=9999) # 11 year cycle

# e) all data
forward.sel(phyto.hel.K42, env.std.K42[,-93], alpha=0.05, nperm=9999)

# customerized data
forward.sel(phyto.hel.K42, cbind(envpluslog.K42, temp.air, ice.s), alpha=0.05, nperm=9999)

# Select only significant env variables
colnames(env.std.K42)
env.std.sel.K42 <- env.std.K42[,c(13,9)]# DM and Si
# RDA analysis with only selected
phyto.rda.sel.K42 <- rda(phyto.hel.K42 ~ ., data= env.std.sel.K42)
summary(phyto.rda.sel.K42)

vif(phyto.rda.sel.K42)

coef(phyto.rda.sel.K42)
(R2 <- RsquareAdj(phyto.rda.sel.K42)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K42)$adj.r.squared)

plot(phyto.rda.sel.K42, scaling=1)
phyto.sc <- scores(phyto.rda.sel.K42, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K42, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K42, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K42$CA$eig[phyto.rda.sel.K42$CA$eig > mean(phyto.rda.sel.K42$CA$eig)]
#---------------------
# 5.1.7: K45
### env + log
head(env.K45)
env.K45.sel <- env.K45[,-c(2,4,7,9)]
env.K45.log <- log(env.K45.sel[,-11])
envpluslog.K45 <- cbind(env.K45.sel, env.K45.log)
colnames(envpluslog.K45)[12:21] <- c("logAlk","logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.K45)
rownames(envpluslog.K45)
head(envpluslog.K45)

### Transformation
# Phytoplankton species composition
phyto.hel.K45 <- decostand(phyto.K45s, "hellinger")
colnames(phyto.hel.K45)
rownames(phyto.hel.K45)

# combine variables
env.all.K45 <- cbind(envpluslog.K45, lcci, temp.air, ice.s, MEM)

# Standardized environmental variables
env.std.K45 <-decostand(env.all.K45[,], "standardize")
colnames(env.std.K45)		
rownames(env.std.K45)

### forward selection
# a) env data
forward.sel(phyto.hel.K45, envpluslog.K45[-c(2,24,30:33),-c(3,14)], alpha=0.15, nperm=9999) # DM, logSi, Stability

# b) indices
# forward.sel(phyto.hel.K45, lcci[-c(2,24,30:33),], alpha=0.05, nperm=9999) # AMOa, PDOa

# c) air temp
# forward.sel(phyto.hel.K45, temp.air[-c(2,24,30:33),-c(17)], alpha=0.05, nperm=9999) # summer air temperature

# d) MEM
# forward.sel(phyto.hel.K45, MEM[-c(2,24,30:33),-1], alpha=0.05, nperm=9999) # 5 years cycle

# e) all without MEM
forward.sel(phyto.hel.K45, env.std.K45[-c(2,24,30:33),-c(68:89,3,14)], alpha=0.14, nperm=9999) # DM, logSi, SSN, AT9

# f) all
forward.sel(phyto.hel.K45, env.std.K45[-c(2,24,30:33),], alpha=0.05, nperm=9999) # DM, 4.25 and 5 year cycle, SSN, MEIa, and logSi


# Select only significant env variables
colnames(env.std.K45)
env.std.sel.K45 <- env.std.K45[-c(2,24,30:33),c(11, 82, 80, 22, 33,18)] # DM, logSi, SSN, AT9
# env.std.sel.K45 <- env.std.K45[-c(2,24,30:33),c(11,18,22,59)] # DM, logSi, SSN, AT9
# RDA analysis with only selected
colnames(env.std.sel.K45)[c(2,3)] <-c("4.25 year cycle", "5 year cycle")
phyto.rda.sel.K45 <- rda(phyto.hel.K45 ~ ., data= env.std.sel.K45)
summary(phyto.rda.sel.K45)

vif(phyto.rda.sel.K45)

coef(phyto.rda.sel.K45)
(R2 <- RsquareAdj(phyto.rda.sel.K45)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K45)$adj.r.squared)

plot(phyto.rda.sel.K45, scaling=2)
phyto.sc <- scores(phyto.rda.sel.K45, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K45, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K45, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K45$CA$eig[phyto.rda.sel.K45$CA$eig > mean(phyto.rda.sel.K45$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.std.sel.K45)

env.w <- env.std.sel.K45[,c(6)]
DM <-env.std.sel.K45[,1]
temp <- env.std.sel.K45[,2:5]
phyto.part.all <- varpart(phyto.hel.K45, env.w, temp, DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.8: S15
### env + log
head(env.S15)
env.S15.sel <- env.S15[,-c(2,4,9)]
env.S15.log <- log(env.S15.sel[,-12])
envpluslog.S15 <- cbind(env.S15.sel, env.S15.log)
colnames(envpluslog.S15)[13:23] <- c("logAlk","logChla","logDO","logNH4","logNO3","logNTK","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.S15)
rownames(envpluslog.S15)
head(envpluslog.S15)

### Transformation
# Phytoplankton species composition
phyto.hel.S15 <- decostand(phyto.S15s[-1,], "hellinger")
colnames(phyto.hel.S15)
rownames(phyto.hel.S15)

# combine variables
env.all.S15 <- cbind(envpluslog.S15, lcci, temp.air, ice.s)

# Standardized environmental variables
env.std.S15 <-decostand(env.all.S15, "standardize")
colnames(env.std.S15)
rownames(env.std.S15)

### forward selection
# a) env 
forward.sel(phyto.hel.S15, envpluslog.S15[-c(1:5,24,30:33),-c(3,15)], alpha=0.08, nperm=9999) 
# DM, T, Si

# b) indices
# forward.sel(phyto.hel.S15, lcci[-c(1:5,24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, NP, MEIa

# c) air temp
# forward.sel(phyto.hel.S15, temp.air[-c(1:5,24,30:33),], alpha=0.05, nperm=9999) # AirTemp9

# d) MEM
# forward.sel(phyto.hel.S15, MEM[-c(1:5,24,30:33),-c(1,2)], alpha=0.05, nperm=9999) # 17 amd 8.67 year cycles

# e) all data
forward.sel(phyto.hel.S15, env.std.S15[-c(1:5,24,30:33),-c(3,15)], alpha=0.05, nperm=9999) 
# DM, NP, ENSO, SSN, NAO

# Select only significant env variables
colnames(env.std.S15)
env.std.sel.S15 <- env.std.S15[,c(12,34,43,24,25)]# 
colnames(env.std.sel.S15)
# RDA analysis with only selected
phyto.rda.sel.S15 <- rda(phyto.hel.S15 ~ ., data= env.std.sel.S15[-c(1:5,24,30:33),])
summary(phyto.rda.sel.S15)

vif(phyto.rda.sel.S15)

coef(phyto.rda.sel.S15)
(R2 <- RsquareAdj(phyto.rda.sel.S15)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.S15)$adj.r.squared)

plot(phyto.rda.sel.S15, scaling=1)
phyto.sc <- scores(phyto.rda.sel.S15, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.S15, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.S15, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.S15$CA$eig[phyto.rda.sel.S15$CA$eig > mean(phyto.rda.sel.S15$CA$eig)]

########xxx
temp <- env.std.S15[-c(1:5,24,30:33),c(34,43,24,25)]
DM <-env.std.S15[-c(1:5,24,30:33),c(12)]
phyto.part.all <- varpart(phyto.hel.S15[,], temp, DM)
plot(phyto.part.all)
phyto.part.all
#---------------------
# Plot 8 RDA biplots in one graph

par(mfrow = c(4, 2))
par(mar=c(1,2,1.5,2))
plot(phyto.rda.sel.C1, scaling=1)
plot(phyto.rda.sel.C6, scaling=1)
plot(phyto.rda.sel.C9, scaling=1)
plot(phyto.rda.sel.E51, scaling=1)
plot(phyto.rda.sel.K39, scaling=1)
plot(phyto.rda.sel.K42, scaling=1)
plot(phyto.rda.sel.K45, scaling=1)
plot(phyto.rda.sel.S15, scaling=1)