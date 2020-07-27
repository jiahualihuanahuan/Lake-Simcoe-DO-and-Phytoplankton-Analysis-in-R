# 5.0 Phyto annual composite vs Env (all 8 sites)
# 5.0.1: C1 C6 C9 E51 K39 K42 K45 S15: All annual

### forward selection
colnames(envpluslog.annual.all)[42:53] <- c("AT1","AT2","AT3","AT4","AT5","AT6","AT7","AT8","AT9","AT10","AT11","AT12")
# rownames(envpluslog.annual.all)
colnames(envpluslog.annual.all)
# # a) all environmental variables and DM
# rownames(phyto.annual.all.2p.hel)
# colnames(phyto.annual.all.2p.hel)

DM.f <- factor(envpluslog.annual.all$DM)
envpluslog.annual.all <- data.frame(envpluslog.annual.all[,],DM.f)
colnames(envpluslog.annual.all)
rownames(envpluslog.annual.all)
summary(envpluslog.annual.all)


forward.sel(phyto.annual.all.2p.hel[-c(171:174,202),], envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),-c(17:59, 12,5)], alpha=0.05, nperm=9999) 
# DM, logSi, Secchi, logTP, Temperature, loggNH4
# R2adj= 0.3609722

# b) only indices
# # forward.sel(phyto.annual.all.2p.hel[-c(171:174,202),], envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:231,232:236,255,261:264),c(10:32)], alpha=0.005, nperm=9999) 
# 21 variables are significant in p<0.005 level: interesting!!
# R2adj=56.70856%

# c) air temperatures
# # forward.sel(phyto.annual.all.2p.hel[-c(171:174,202),], envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:231,232:236,255,261:264),c(33:53)], alpha=0.005, nperm=9999) 
# 13 variables are significant in p<0.005 level: interesting!!
# R2adj=44.47053%

# e) all variables
## forward.sel(phyto.hel.annual.all[-c(171:174,202),], envpluslog.annual.all.std[,-c(59,45,47,16,18,19,34,13:15)], alpha=0.05, nperm=9999) 
#DM, logSi,SSN,Secchi,
# R2adj=%



# Select only significant env variables
# colnames(envpluslog.annual.all)

envpluslog.annual.all.sel <- envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),c(60,13,4,16,7,10)]

# colnames(envpluslog.annual.all.sel)
# # RDA analysis with only selected
# colnames(phyto.annual.all.2p.hel)
# rownames(phyto.annual.all.2p.hel)
# colnames(envpluslog.annual.all.sel)
# rownames(envpluslog.annual.all.sel)

phyto.rda.sel.annual.all <- rda(phyto.annual.all.2p.hel[-c(171:174,202),] ~ ., data=envpluslog.annual.all.sel)
summary(phyto.rda.sel.annual.all)

vif(phyto.rda.sel.annual.all)

coef(phyto.rda.sel.annual.all)
(R2 <- RsquareAdj(phyto.rda.sel.annual.all)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.annual.all)$adj.r.squared)

plot(phyto.rda.sel.annual.all, scaling=1)
phyto.sc <- scores(phyto.rda.sel.annual.all, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.annual.all, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.annual.all, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.annual.all$CA$eig[phyto.rda.sel.annual.all$CA$eig > mean(phyto.rda.sel.annual.all$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.annual.all.sel)

env.w <- envpluslog.annual.all.sel[,c(2:6)]
DM <- envpluslog.annual.all.sel[,c(1)]
phyto.part.all <- varpart(phyto.annual.all.2p.hel[-c(171:174,202),], env.w, DM)
plot(phyto.part.all)
phyto.part.all

#-------------------------------------
# customerization
phyto.hel.sel.annual.all <- phyto.annual.all.2p.hel[-c(171:174,202),]
envpluslog.annual.all
phyto.rda.sel.annual.all

colnames(env.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),])
site <- env.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),60]
dm <- env.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),61]
with(envpluslog.annual.all, levels(site))
with(envpluslog.annual.all, levels(dm))

scl <- 2
colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")
colvec8 <- c("green1", "green2","green3","purple", "blue1","blue2","blue3", "red" )

# par(mfrow = c(1,2))
# par(mar=c(2,2,1.5,2))
#-------------------------------------
###### Divide by sites
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.annual.all, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),], points(phyto.rda.sel.annual.all, display="sites", col=colvec8[site], scaling = scl, pch=21, bg=colvec8[site]))

# add species scores with species names
text(phyto.rda.sel.annual.all, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.annual.all, legend("left", legend = levels(site), bty = "n", col = colvec8, pch=21, pt.bg=colvec8))

fit <- envfit(phyto.rda.sel.annual.all, envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:236,255,261:264),c("DM","logSi","Secchi","logTP","Temperature","logNH4")], perm=999)

plot(fit, add=T, col="black")
#-------------------------------------
###### Divide by DM
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.annual.all, type="n", scaling=scl) 
# add dm scores by colouring each site according to its land-use
with(envpluslog.annual.all, points(phyto.rda.sel.annual.all, display="sites", col=colvec2[dm], scaling = scl, pch=21, bg=colvec2[dm]))

head(with(env.all, colvec2[dm]))

# add species scores with species names
text(phyto.rda.sel.annual.all, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.annual.all, legend("topleft", legend = levels(dm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.annual.all, envpluslog.annual.all[-c(24,30:33,57,64:66,90,96:99,123,129:132,156,162:165,195:198,200,222,228:231,232:236,255,261:264),c("DM","logSi","Secchi","logTP","Temperature","logNH4")], perm=999)

plot(fit, add=T, col="black")
#-------------------------------------
# 5.1 Phyto annual composite vs Env (all 8 sites individually)
# RDA: annual phyto ~ env
# 5.1.1: C1

# combine variables
env.all.C1 <- cbind(envpluslog.C1, lcci, temp.air, ice.s, MEM)


### forward selection
# a) env 
# # forward.sel(phyto.C1.2p.hel, envpluslog.C1[-c(24,30:33),], alpha=0.2, nperm=9999) 
#DM, logNTK, S, and logSi
# R2adj=37.91%

colnames(envpluslog.C1)

# b) indices
# # forward.sel(phyto.C1.2p.hel, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa and PDOi
# R2adj=22.56%


# c) air temp
# # forward.sel(phyto.C1.2p.hel, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# annual and AirTemp9
# R2adj=16.32%

# d) MEM
# # forward.sel(phyto.C1.2p.hel, MEM[-c(24,30:33),-1], alpha=0.1, nperm=9999) 
# 11
# R2adj=3.86%

# e) all data (without MEM)
forward.sel(phyto.C1.2p.hel, env.all.C1[-c(24,30:33),-c(17,49:51,66:87)], alpha=0.09, nperm=99999) 
#DM, logTKK, QBOs, Winter air temperature1-3, logSi, Stability
# R2adj=0.4515465

# Select only significant env variables
colnames(env.all.C1)
colnames(env.all.C1)[20] <- "logTKN"

env.all.C1.sel <- env.all.C1[-c(24,30:33),c("DM", "logTKN", "QBOs", "Winter.Air.1.3", "logSi", "Stability")]

colnames(env.all.C1.sel)[4] <- "Winter Air Temperature"
# RDA analysis with only selected variables
phyto.rda.sel.C1 <- rda(phyto.C1.2p.hel ~ ., data= env.all.C1.sel)
summary(phyto.rda.sel.C1)

vif(phyto.rda.sel.C1)

coef(phyto.rda.sel.C1)
(R2 <- RsquareAdj(phyto.rda.sel.C1)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C1)$adj.r.squared)

plot(phyto.rda.sel.C1, scaling=1)
phyto.sc <- scores(phyto.rda.sel.C1, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C1, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C1, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.C1$CA$eig[phyto.rda.sel.C1$CA$eig > mean(phyto.rda.sel.C1$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.all.C1.sel)

env.w <- env.all.C1.sel[,c(1,2,4,5,6)]
temp <- env.all.C1.sel[,c(3)]

phyto.part.all <- varpart(phyto.C1.2p.hel, env.w, temp)
plot(phyto.part.all)
phyto.part.all


#---------------------
# 5.1.2: C6

# combine variables
envpluslog.C6.all <- cbind(envpluslog.C6, lcci, temp.air, ice.s, MEM)

### forward selection

# a) env 
# forward.sel(phyto.C6.2p.hel, envpluslog.C6[-c(24,31:33),], alpha=0.13, nperm=9999) 
# DM, logSi, Stability, logNTK, TP

# b) indices
# forward.sel(phyto.C6.2p.hel, lcci[-c(24,31:33),-c(17,10)], alpha=0.1, nperm=9999) 
# AMOa, SSN., PDOi

# c) air temp
# forward.sel(phyto.C6.2p.hel, temp.air[-c(24,31:33),], alpha=0.05, nperm=9999) 
# AirTemp9

# d) MEM
# forward.sel(phyto.C6.2p.hel, MEM[-c(24,31:33),], alpha=0.05, nperm=9999) 
# 34,11,10,5,14,4.57 years cycle

# e) all data
# forward.sel(phyto.C6.2p.hel, envpluslog.C6.all[-c(24,31:33),], alpha=0.05, nperm=9999) 
# DM, SSN, logSi, AirTemp2, Stability, QBOa, 24-year-cycle
# R2adj=51.8%

# Select only significant env variables
colnames(envpluslog.C6.all)
envpluslog.C6.all.sel <- envpluslog.C6.all[-c(24,31:33),c(10,20,16,50,7,22,67)]
colnames(envpluslog.C6.all.sel)

colnames(envpluslog.C6.all.sel)[c(4,7)] <- c("AirTempFeb","24-year-cycle")
colnames(envpluslog.C6.all.sel)

# RDA analysis with only selected variables
phyto.rda.sel.C6 <- rda(phyto.C6.2p.hel ~ ., data=envpluslog.C6.all.sel)
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
env.w <- envpluslog.C6.all.sel[,c(3,4,5)]
temp <- envpluslog.C6.all.sel[,c(2,6,7)]
DM <- envpluslog.C6.all.sel[,1]
phyto.part.all <- varpart(phyto.C6.2p.hel, env.w, temp,DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.3: C9

# combine variables
envpluslog.C9.all <- cbind(envpluslog.C9, lcci, temp.air, ice.s, MEM)

### forward selection
# a) env 
# forward.sel(phyto.C9.2p.hel, envpluslog.C9[-c(24,30:33),], alpha=0.17, nperm=9999) 
#DM, logSi, logTP, Secchi

# b) indices
# forward.sel(phyto.C9.2p.hel, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, SSN, PDOi

# c) air temp
# forward.sel(phyto.C9.2p.hel, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AirTemp9 and AirTemp6

# d) MEM
# forward.sel(phyto.C9.2p.hel, MEM[-c(24,30:33),], alpha=0.05, nperm=9999) 
# 34, 11, 10, xxx year cycle

# e) all data
# forward.sel(phyto.C9.2p.hel, envpluslog.C9.all[-c(24,30:33),-c(66:87)], alpha=0.07, nperm=9999) 
# DM, SSN, AT4, logTP, logSi
# Annual AirTemp, ice-out date

# Select only significant env variables (env only)
colnames(envpluslog.C9.all)

envpluslog.C9.all.sel <- envpluslog.C9.all[-c(24,30:33),c(10,20,52,19,16,62,43)]

colnames(envpluslog.C9.all.sel)
colnames(env.std.sel.C9)[c(3)] <- c("AirTempApril")
colnames(env.std.sel.C9)

# RDA analysis with only selected
phyto.rda.sel.C9 <- rda(phyto.C9.2p.hel ~ ., data= envpluslog.C9.all.sel)
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

# combine variables
envpluslog.E51.all <- cbind(envpluslog.E51, lcci, temp.air, ice.s, MEM)

### forward selection

# a) env 
# forward.sel(phyto.E51.2p.hel, envpluslog.E51[-c(24,30:33),], alpha=0.05, nperm=9999) 
# DM, Chl.a and Si
# R2adj=0.3467735

# b) indices
# forward.sel(phyto.E51.2p.hel, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa

# c) air temp
# forward.sel(phyto.E51.2p.hel, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AirTemp9 and AirTemp6

# d) MEM
# forward.sel(phyto.E51.2p.hel, MEM[-c(24,30:33),-1], alpha=0.05, nperm=9999) 
# 11 and 5 years cycle

# e) all data
# forward.sel(phyto.E51.2p.hel, envpluslog.E51.all[-c(24,30:33),], alpha=0.07, nperm=9999) 
# DM, 11,4.57 years cycle, SSN, AT9
# R2adj=0.4316731 

# f) all data without MEM
# forward.sel(phyto.E51.2p.hel, envpluslog.E51.all[-c(24,30:33),-c(66:87)], alpha=0.05, nperm=9999) 
# DM, Chla, logSi
# R2adj=0.3467735

rownames(envpluslog.E51.all)
colnames(envpluslog.E51.all)

# Select only significant env variables
colnames(envpluslog.E51)
envpluslog.E51.sel <- envpluslog.E51[-c(24,30:33),c(19,1,15)]
# RDA analysis with only selected
phyto.rda.sel.E51 <- rda(phyto.E51.2p.hel ~ ., data= envpluslog.E51.sel)
summary(phyto.rda.sel.E51)

vif(phyto.rda.sel.E51)

coef(phyto.rda.sel.E51)
(R2 <- RsquareAdj(phyto.rda.sel.E51)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.E51)$adj.r.squared)

plot(phyto.rda.sel.E51, scaling=1)
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
colnames(envpluslog.E51.sel)

env.w <- envpluslog.E51.sel[,c(2,3)]
DM <-envpluslog.E51.sel[,1]
phyto.part.all <- varpart(phyto.E51.2p.hel, env.w, DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.5: K39

# combine variables
envpluslog.K39.all <- cbind(envpluslog.K39, lcci, temp.air, ice.s, MEM)

### forward selection
# a) env
# forward.sel(phyto.K39.2p.hel, envpluslog.K39[-c(24,30:33),-11], alpha=0.05, nperm=9999) 
# DM, T, Chla
# R2adj=0.3366849

# b) indices
# forward.sel(phyto.K39.2p.hel, lcci[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, PDOi, AMOf, SSN
# R2adj=0.2954251

# c) air temp
# forward.sel(phyto.K39.2p.hel, temp.air[-c(24,30:33),], alpha=0.05, nperm=9999) 
# AT9
# R2adj=0.1238578

# d) MEM
# forward.sel(phyto.K39.2p.hel, MEM[-c(24,30:33),-1], alpha=0.06, nperm=9999) 
# 10 years cycle
# R2adj=0.04919166

# e) all data without MEM
# forward.sel(phyto.K39.2p.hel, envpluslog.K39.all[-c(24,30:33),-c(11,66:87,2,10)], alpha=0.05, nperm=9999) 
# DM, Temperature, Chla
# R2adj=0.3366849

colnames(envpluslog.K39.all)


# Select only significant env variables
colnames(envpluslog.K39)
envpluslog.K39.sel <- envpluslog.K39[-c(24,30:33),c(19,8,1)] 
colnames(envpluslog.K39)

# RDA analysis with only selected
phyto.rda.sel.K39 <- rda(phyto.K39.2p.hel ~ ., data= envpluslog.K39.sel)
summary(phyto.rda.sel.K39)

vif(phyto.rda.sel.K39)

coef(phyto.rda.sel.K39)
(R2 <- RsquareAdj(phyto.rda.sel.K39)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K39)$adj.r.squared)

plot(phyto.rda.sel.K39, scaling=1)
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
colnames(envpluslog.K39.sel)

env.w <- envpluslog.K39.sel[,c(2,3)]
DM <-envpluslog.K39.sel[,1]
phyto.part.all <- varpart(phyto.K39.2p.hel, env.w,DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.6: K42 1980-2008

# combine variables
envpluslog.K42.all <- cbind(env.K42, lcci, temp.air, ice.s, MEM)

### forward selection
# a) env data
forward.sel(phyto.K42.2008.2p.hel, env.K42[-c(30:33),-c(4,25,10,11,26,45)], alpha=0.05, nperm=9999) 
# p<0.05
# DM
# R2adj=0.296475

# p<0.26
# DM, Si, VHET, S, TP, NO3
# R2adj=0.3705311

# b) indices
forward.sel(phyto.K42.2008.2p.hel, lcci[-c(30:33),-17], alpha=0.05, nperm=9999) 
# AMOa

# c) air temp
forward.sel(phyto.K42.2008.2p.hel, temp.air[-c(30:33),], alpha=0.05, nperm=9999) 
# AirTemp9

# d) MEM
forward.sel(phyto.K42.2008.2p.hel, MEM[-c(30:33),], alpha=0.05, nperm=9999) 
# 34,11,5,3.67 years cycle

# e) all data
forward.sel(phyto.K42.2008.2p.hel, envpluslog.K42.all[-c(30:33),-c(4,25)], alpha=0.05, nperm=9999)
# DM, 7 and 3.56 year cycle
# R2adj= 0.3608969

# e) all data without MEM
colnames(envpluslog.K42.all)
forward.sel(phyto.K42.2008.2p.hel, envpluslog.K42.all[-c(30:33),-c(4,25,93:114)], alpha=0.05, nperm=9999)
# DM
# R2adj= 0.296475


# # Select only significant variables
# colnames(envpluslog.K42)
# envpluslog.K42.sel <- envpluslog.K42[,c(13,9,37,22,33,5)]
# colnames(envpluslog.K42.sel)
# # RDA analysis with only selected
# phyto.rda.sel.K42 <- rda(phyto.K42.2008.2p.hel ~ ., data=envpluslog.K42.sel[-c(30:33),])
# #summary(phyto.rda.sel.K42)

# vif(phyto.rda.sel.K42)

# coef(phyto.rda.sel.K42)
# (R2 <- RsquareAdj(phyto.rda.sel.K42)$r.squared)
# (R2adj <- RsquareAdj(phyto.rda.sel.K42)$adj.r.squared)

# plot(phyto.rda.sel.K42, scaling=1)
# phyto.sc <- scores(phyto.rda.sel.K42, choices=1:2, scaling=1, display="sp")
# arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# # Global test of the RDA result
# anova.cca(phyto.rda.sel.K42, step=1000)
# # Test of all canonical axes
# anova.cca(phyto.rda.sel.K42, by="axis", step=1000)

# # Apply Kaiser-Guttan criterion to residual axes
# phyto.rda.sel.K42$CA$eig[phyto.rda.sel.K42$CA$eig > mean(phyto.rda.sel.K42$CA$eig)]

#---------------------
# 5.1.6.2: K42 1980-2012

# combine variables
envpluslog.K42.all <- cbind(env.K42, lcci, temp.air, ice.s, MEM)

### forward selection
# a) env data
forward.sel(phyto.K42.2012.2p.hel, env.K42[,-c(4,11)], alpha=0.05, nperm=9999) 
# DM Si, TP, Secchi, NO3, Temperature, Average stability, NTK, Air_log, vWT
# R2adj=0.4377202
# colnames(envpluslog.K42)

# b) indices
# forward.sel(phyto.K42.2012.2p.hel, lcci[,], alpha=0.05, nperm=9999) 
# AMOa, PDOi
# R2adj=0.2876828

# c) air temp
# forward.sel(phyto.K42.2012.2p.hel, temp.air[,], alpha=0.05, nperm=9999) 
# Annual Air Temp

# d) MEM
# forward.sel(phyto.K42.2012.2p.hel, MEM[,-c(1,2)], alpha=0.05, nperm=9999) 
# 11 year cycle

# e) all data
# forward.sel(phyto.K42.2012.2p.hel, envpluslog.K42.all[,], alpha=0.05, nperm=9999)
# DM, 7 year cycle, AMOf, 11 year cycle
# R2adj=0.4088788

# Select only significant env variables
colnames(env.K42)
env.K42.sel <- envpluslog.K42[,c(13,9,12,8,5,11,22,6,35,15)]# DM and Si
# RDA analysis with only selected
phyto.rda.sel.K42.2012 <- rda(phyto.K42.2012.2p.hel ~ ., data= env.K42.sel)
summary(phyto.rda.sel.K42.2012)

vif(phyto.rda.sel.K42.2012)

coef(phyto.rda.sel.K42.2012)
(R2 <- RsquareAdj(phyto.rda.sel.K42.2012)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K42.2012)$adj.r.squared)

plot(phyto.rda.sel.K42.2012, scaling=1)
phyto.sc <- scores(phyto.rda.sel.K42.2012, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K42, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K42, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K42$CA$eig[phyto.rda.sel.K42$CA$eig > mean(phyto.rda.sel.K42$CA$eig)]
#---------------------
# 5.1.7: K45

# combine variables
envpluslog.K45.all <- cbind(envpluslog.K45, lcci, temp.air, ice.s, MEM)


### forward selection
# a) env data
# forward.sel(phyto.K45.2p.hel, envpluslog.K45[-c(2,24,30:33),-c(3,13)], alpha=0.15, nperm=9999) 
# DM, logSi, Stability
# R2adj=0.3195528
colnames(envpluslog.K45)

# b) indices
# forward.sel(phyto.K45.2p.hel, lcci[-c(2,24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, PDOa

# c) air temp
# forward.sel(phyto.K45.2p.hel, temp.air[-c(2,24,30:33),-c(17)], alpha=0.05, nperm=9999) 
# summer air temperature

# d) MEM
# forward.sel(phyto.K45.2p.hel, MEM[-c(2,24,30:33),-1], alpha=0.05, nperm=9999) 
# 5 years cycle

# e) all without MEM
# forward.sel(phyto.K45.2p.hel, envpluslog.K45.all[-c(2,24,30:33),-c(68:89,3,14)], alpha=0.14, nperm=9999) 
# DM, logSi, SSN, AT9

# f) all
# forward.sel(phyto.K45.2p.hel, envpluslog.K45.all[-c(2,24,30:33),], alpha=0.042, nperm=9999) 
# DM, 4.25 and 5 year cycle, SSN, MEIa, and logSi
# R2adj=0.4600908


# Select only significant env variables
colnames(envpluslog.K45.all)
envpluslog.K45.all.sel <- envpluslog.K45.all[-c(2,24,30:33),c(21,82,80,22,33,17)]

# RDA analysis with only selected
colnames(envpluslog.K45.all.sel)[c(2,3)] <-c("4.25 year cycle", "5 year cycle")
phyto.rda.sel.K45 <- rda(phyto.K45.2p.hel ~ ., data= envpluslog.K45.all.sel)
summary(phyto.rda.sel.K45)

vif(phyto.rda.sel.K45)

coef(phyto.rda.sel.K45)
(R2 <- RsquareAdj(phyto.rda.sel.K45)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K45)$adj.r.squared)

plot(phyto.rda.sel.K45, scaling=1)
phyto.sc <- scores(phyto.rda.sel.K45, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K45, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K45, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K45$CA$eig[phyto.rda.sel.K45$CA$eig > mean(phyto.rda.sel.K45$CA$eig)]

### Variation partitioning with three sets of explanatory variables
# split variables first
colnames(envpluslog.K45.all.sel)

env.w <- envpluslog.K45.all.sel[,c(6)]
DM <-envpluslog.K45.all.sel[,1]
temp <- envpluslog.K45.all.sel[,2:5]
phyto.part.all <- varpart(phyto.K45.2p.hel, env.w, temp, DM)
plot(phyto.part.all)
phyto.part.all

#---------------------
# 5.1.8: S15

# combine variables
envpluslog.S15.all <- cbind(envpluslog.S15, lcci, temp.air, ice.s)

### forward selection
# a) env 
# forward.sel(phyto.S15.2p.hel[-1,], envpluslog.S15[-c(1:5,24,30:33),-c(3,14)], alpha=0.08, nperm=9999) 
# DM, T, Si
# R2adj=0.3182513
colnames(envpluslog.S15)


# b) indices
# forward.sel(phyto.S15.2p.hel[-1,], lcci[-c(1:5,24,30:33),], alpha=0.05, nperm=9999) 
# AMOa, NP, MEIa

# c) air temp
# forward.sel(phyto.S15.2p.hel[-1,], temp.air[-c(1:5,24,30:33),], alpha=0.05, nperm=9999) 
# AirTemp9

# d) MEM
# forward.sel(phyto.S15.2p.hel[-1,], MEM[-c(1:5,24,30:33),-c(1,2)], alpha=0.05, nperm=9999) 
# 17 amd 8.67 year cycles

# e) all data
# forward.sel(phyto.S15.2p.hel[-1,], envpluslog.S15.all[-c(1:5,24,30:33),-c(3,14)], alpha=0.05, nperm=9999) 
# DM, NP, ENSOa, NAO, SSN

# Select only significant env variables
colnames(envpluslog.S15.all)
env.std.sel.S15 <- envpluslog.S15.all[,c(23,34,43,25,24)]# 
colnames(env.std.sel.S15)
# RDA analysis with only selected
phyto.rda.sel.S15 <- rda(phyto.S15.2p.hel[-1,] ~ ., data= env.std.sel.S15[-c(1:5,24,30:33),])
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
temp <- env.std.sel.S15[-c(1:5,24,30:33),2:5]
DM <-env.std.sel.S15[-c(1:5,24,30:33),1]
phyto.part.all <- varpart(phyto.S15.2p.hel[-1,], temp, DM)
plot(phyto.part.all)
phyto.part.all
#---------------------
# Plot 8 RDA biplots in one graph

par(mfrow = c(3,3))
par(mar=c(2,2,1.5,2))
plot(phyto.rda.sel.C1, scaling=1)
plot(phyto.rda.sel.C6, scaling=1)
plot(phyto.rda.sel.C9, scaling=1)
plot(phyto.rda.sel.E51, scaling=1)
plot(phyto.rda.sel.K39, scaling=1)
plot(phyto.rda.sel.K42, scaling=1)
plot(phyto.rda.sel.K42.2012, scaling=1)
plot(phyto.rda.sel.K45, scaling=1)
plot(phyto.rda.sel.S15, scaling=1)


