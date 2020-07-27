
# 1 site and 1 season
# RDA analysis C1.S

# 6.2.1: C1 X S

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
# colnames(phyto.C1.S.hel)
# rownames(phyto.C1.S.hel)
# colnames(envpluslog.C1.S)
# rownames(envpluslog.C1.S[-c(1:10),])
forward.sel(phyto.C1.S.hel, envpluslog.C1.S[-c(1:10),], alpha=0.05, nperm=9999) 
# logSi, DM, Cl, TP
R2adj= 0.2849340

# b) indices
# forward.sel(phyto.C1.S.hel, lcci[-c(1:10),], alpha=0.05, nperm=9999) # AMOa

# c) air temp
# forward.sel(phyto.C1.S.hel, temp.air[-c(1:10),], alpha=0.07, nperm=9999) # AT9

# d) MEM
# forward.sel(phyto.C1.S.hel, MEM[-c(1:10),-1], alpha=0.05, nperm=9999) # 11 years cycle

# e) all data
# combine variables
# env.all.C1.S <- cbind(envpluslog.C1.S, lcci, temp.air, MEM)
# colnames(env.all.C1.S)
# rownames(env.all.C1.S)

# all env+oscoilations+temperatures
# forward.sel(phyto.C1.S.hel, env.all.C1.S[-c(1:10),], alpha=0.05, nperm=9999) 
# Si, 17, 8.67, 5.3, NO3, AirTemp4-6
# R2adj= 0.4029324

# all env except MEM
# forward.sel(phyto.C1.S.hel, env.all.C1.S[-c(1:10),-c(76:97)], alpha=0.05, nperm=9999)
# Si, DM, Cl, AirTemp4-6
# R2adj= 0.2868516


# Select only significant env variables
colnames(env.all.C1.S)
####env.sel.C1.S <- env.all.C1.S[-c(1:10),c("logSi","X17y.c","X8.2.3y.c","X5.1.3y.c","NO3","Spring.Air.4.6","AMOs")]
env.sel.C1.S <- env.all.C1.S[-c(1:10),c("logSi","DM","Cl","TP")]
colnames(env.sel.C1.S)

# RDA analysis with only selected
phyto.rda.sel.C1.S <- rda(phyto.C1.S.hel ~ ., data=env.sel.C1.S)
summary(phyto.rda.sel.C1.S)

vif(phyto.rda.sel.C1.S)

coef(phyto.rda.sel.C1.S)
(R2 <- RsquareAdj(phyto.rda.sel.C1.S)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C1.S)$adj.r.squared)

plot(phyto.rda.sel.C1.S, scaling=1)
phyto.sc <- scores(phyto.rda.sel.C1.S, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C1.S, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C1.S, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.C1.S$CA$eig[phyto.rda.sel.C1.S$CA$eig > mean(phyto.rda.sel.C1.S$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.C1.S)

# Select only significant env variables
colnames(env.sel.C1.S)
rownames(env.sel.C1.S)
env.w <- env.sel.C1.S[,c(1,3,4)]
DM <- env.sel.C1.S[,2]
phyto.part.all <- varpart(phyto.C1.S.hel, env.w, DM)
plot(phyto.part.all)
phyto.part.all
#-------------------------------------
# customerization
dm <- env.C1.S[-c(1:10),16]
with(env.sel.C1.S, levels(dm))

scl <- 1
colvec2 <- c("red2", "green4")

#-------------------------------------
###### Divide by DM
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.C1.S, type="n", scaling=scl) 
# add dm scores by colouring each site according to its land-use
with(env.sel.C1.S, points(phyto.rda.sel.C1.S, display="sites", col=colvec2[dm], scaling = scl, pch=21, bg=colvec2[dm]))

head(with(env.C1.S[-c(1:10),], colvec2[dm]))

# add species scores with species names
text(phyto.rda.sel.C1.S, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.C1.S, legend("topleft", legend = levels(dm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.C1.S, env.sel.C1.S, perm=999)

plot(fit, add=T, col="black")

#------------------------------------
# 6.2.2: C1 X F

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
colnames(phyto.C1.F.hel)
rownames(phyto.C1.F.hel)
colnames(envpluslog.C1.F)
rownames(envpluslog.C1.F[-c(1:10,33),])
forward.sel(phyto.C1.F.hel, envpluslog.C1.F[-c(1:10,33),], alpha=0.05, nperm=9999) 
# logChla, logS
# R2adj= 0.1905470

# b) indices
# forward.sel(phyto.C1.F.hel, lcci[-c(1:10,33),], alpha=0.05, nperm=9999) # NP, ENSOi, AMOa

# c) air temp
# forward.sel(phyto.C1.F.hel, temp.air[-c(1:10,33),], alpha=0.05, nperm=9999)

# d) MEM
# forward.sel(phyto.C1.F.hel, MEM[-c(1:10,33),-c(1)], alpha=0.05, nperm=9999) # 17 years cycle

# e) all data
# combine variables
env.all.C1.F <- cbind(envpluslog.C1.F, lcci, temp.air, MEM)
colnames(env.all.C1.F)
rownames(env.all.C1.F)

# all variables
forward.sel(phyto.C1.F.hel, env.all.C1.F[-c(1:10,33),], alpha=0.05, nperm=9999) 


# all variables except MEM
forward.sel(phyto.C1.F.hel, env.all.C1.F[-c(1:10,33),-c(72:93)], alpha=0.05, nperm=9999)
# logChla, logS, NP, logNO3
# R2adj= 0.3063541

# Select only significant env variables
colnames(env.all.C1.F)
env.sel.C1.F <- env.all.C1.F[-c(1:10,33),c("logChla","logS","NP","logNO3")]
colnames(env.sel.C1.F)

# RDA analysis with only selected
phyto.rda.sel.C1.F <- rda(phyto.C1.F.hel ~ ., data=env.sel.C1.F)
summary(phyto.rda.sel.C1.F)

vif(phyto.rda.sel.C1.F)

coef(phyto.rda.sel.C1.F)
(R2 <- RsquareAdj(phyto.rda.sel.C1.F)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C1.F)$adj.r.squared)

plot(phyto.rda.sel.C1.F, scaling=1)
phyto.sc <- scores(phyto.rda.sel.C1.F, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C1.F, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C1.F, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.C1.F$CA$eig[phyto.rda.sel.C1.F$CA$eig > mean(phyto.rda.sel.C1.F$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
env.w <- env.sel.C1.F[,c(1,2,4)]
temp <- env.sel.C1.F[,3]
phyto.part.all <- varpart(phyto.C1.F.hel, env.w, temp)
plot(phyto.part.all)
phyto.part.all
#-------------------------------------
# customerization
dm <- env.C1.F[-c(1:10),16]
with(env.sel.C1.F, levels(dm))

scl <- 2
colvec2 <- c("red2", "green4")

#-------------------------------------
###### Divide by DM
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.C1.F, type="n", scaling=scl) 
# add dm scores by colouring each site according to its land-use
with(env.sel.C1.F, points(phyto.rda.sel.C1.F, display="sites", col=colvec2[dm], scaling = scl, pch=21, bg=colvec2[dm]))

head(with(env.C1.F[-c(1:10),], colvec2[dm]))

# add species scores with species names
text(phyto.rda.sel.C1.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.C1.F, legend("topleft", legend = levels(dm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.C1.F, env.sel.C1.F, perm=999)

plot(fit, add=T, col="black")
#------------------------------------

# 6.2.1: K42 X S
# 6.2.1: K42

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
# colnames(phyto.K42.F.hel)
# rownames(phyto.K42.F.hel)
# colnames(envpluslog.K42.F)
# rownames(envpluslog.K42.F[-c(2:10,20,24,25),])
# rownames(envpluslog.K42.F[,])

# include 1980
# forward.sel(phyto.K42.F.hel, envpluslog.K42.F[-c(2:10,20,24,25),-c(1,2,4,7,9,15,16,18,21,23, 5,19)], alpha=0.05, nperm=9999) 
# logSi
R2adj= 0.2792042

# exclude 1980, p < 0.05
# forward.sel(phyto.K42.F.hel[-1,], envpluslog.K42.F[-c(1:10,20,24,25),-c(1,2,4,7,9,15,16,18,21,23, 5,19)], alpha=0.05, nperm=9999) 
# logSi
R2adj= 0.282352

# exclude 1980, p < 0.08
# forward.sel(phyto.K42.F.hel[-1,], envpluslog.K42.F[-c(1:10,20,24,25),-c(1,2,4,7,9,15,16,18,21,23, 5,19)], alpha=0.08, nperm=9999) 
# logSi, logNH4, DM
# R2adj= 0.3693679

# b) indices
# forward.sel(phyto.K42.F.hel, lcci[-c(2:10,20,24,25),], alpha=0.05, nperm=9999) 
# AMOa, PDOi, SSN


# c) air temp
# forward.sel(phyto.K42.F.hel, temp.air[-c(2:10,20,24,25),], alpha=0.05, nperm=9999) 
# AT9

# d) MEM
# forward.sel(phyto.K42.F.hel, MEM[-c(2:10,20,24,25),-1], alpha=0.05, nperm=9999) # 24, 17, 14, 7 years cycle

# e) all data
# combine variables
# env.all.K42.F <- cbind(envpluslog.K42.F, lcci, temp.air, MEM)
# colnames(env.all.K42.F)
# rownames(env.all.K42.F)

# all env+oscoilations+temperatures
# forward.sel(phyto.K42.F.hel, env.all.K42.F[-c(2:10,20,24,25),-c(1,2,4,7,9,15,16,18,21,23)], alpha=0.05, nperm=9999) 
# logSi, 17y.c, air temp 12, 10, PDOi, 8.67 year cycle
# R2adj= 0.6093600

# all env except MEM
colnames(env.all.K42.F)
forward.sel(phyto.K42.F.hel, env.all.K42.F[-c(2:10,20,24,25),-c(72:93, 1,2,4,7,9,15,16,18,21,23, 5,19, 70)], alpha=0.05, nperm=9999)
# logSi, SSN, PDOa, air temp 10
# R2adj= 0.4677258


# Select only significant env variables
colnames(env.all.K42.F)
env.sel.K42.F <- env.all.K42.F[-c(2:10,20,24,25),c("logSi","SSN","PDOa","X10")]
colnames(env.sel.K42.F)
colnames(env.sel.K42.F)[4] <- "Oct Air Temperature"

# RDA analysis with only selected
phyto.rda.sel.K42.F <- rda(phyto.K42.F.hel ~ ., data=env.sel.K42.F)
summary(phyto.rda.sel.K42.F)

vif(phyto.rda.sel.K42.F)

coef(phyto.rda.sel.K42.F)
(R2 <- RsquareAdj(phyto.rda.sel.K42.F)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K42.F)$adj.r.squared)

plot(phyto.rda.sel.K42.F, scaling=2)
phyto.sc <- scores(phyto.rda.sel.K42.F, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K42.F, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K42.F, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K42.F$CA$eig[phyto.rda.sel.K42.F$CA$eig > mean(phyto.rda.sel.K42.F$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.K42.F)

# Select only significant env variables
colnames(env.sel.K42.F)
rownames(env.sel.K42.F)
env.w <- env.sel.K42.F[,c(1,4)]
temp <- env.sel.K42.F[,c(2,3)]
phyto.part.all <- varpart(phyto.K42.F.hel, env.w, temp)
plot(phyto.part.all)
phyto.part.all
#-------------------------------------
# customerization
dm <- env.K42.F[-c(2:10,20,24,25),16]
with(env.sel.K42.F, levels(dm))

scl <- 1
colvec2 <- c("red2", "green4")

#-------------------------------------
###### Divide by DM
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.K42.F, type="n", scaling=scl) 
# add dm scores by colouring each site according to its land-use
with(env.sel.K42.F, points(phyto.rda.sel.K42.F, display="sites", col=colvec2[dm], scaling = scl, pch=21, bg=colvec2[dm]))

head(with(env.K42.F[-c(2:10,20,24,25),], colvec2[dm]))

# add species scores with species names
text(phyto.rda.sel.K42.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.K42.F, legend("topleft", legend = levels(dm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.K42.F, env.sel.K42.F, perm=999)

plot(fit, add=T, col="black")
#------------------------------------
# 6.2.4: K42 X F
# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
colnames(phyto.K42.S.hel)
rownames(phyto.K42.S.hel)
colnames(envpluslog.K42.S)
rownames(envpluslog.K42.S[-c(2:10,20,22,24,25),])
rownames(envpluslog.K42.S[,])

# include 1980
forward.sel(phyto.K42.S.hel, envpluslog.K42.S[-c(2:10,20,22,24,25),-c(7,21,9,4)], alpha=0.05, nperm=9999) 
# DM, logT, logpH, logCl
R2adj= 0.4218258

# exclude 1980, p < 0.05
forward.sel(phyto.K42.S.hel[-1,], envpluslog.K42.S[-c(1:10,20,22,24,25),-c(7,21,19,5,4,18)], alpha=0.05, nperm=9999) 
# DM, logT, NH4
R2adj= 0.3603546

# b) indices
forward.sel(phyto.K42.S.hel, lcci[-c(2:10,20,22,24,25),], alpha=0.05, nperm=9999) 
# AMOa, PDOi, SSN


# c) air temp
forward.sel(phyto.K42.S.hel, temp.air[-c(2:10,20,22,24,25),], alpha=0.05, nperm=9999) 
# AT9

# d) MEM
forward.sel(phyto.K42.S.hel, MEM[-c(2:10,20,22,24,25),-1], alpha=0.05, nperm=9999) # 11,24,10 years cycle

# e) all data
# combine variables
env.all.K42.S <- cbind(envpluslog.K42.S, lcci, temp.air, MEM)
colnames(env.all.K42.S)
# rownames(env.all.K42.S)

# all env+oscoilations+temperatures
forward.sel(phyto.K42.S.hel, env.all.K42.S[-c(2:10,20,22,24,25),-c(7,21,4)], alpha=0.05, nperm=9999) 
# DM, logT, 
# R2adj=

# all env except MEM
colnames(env.all.K42.S)
forward.sel(phyto.K42.S.hel, env.all.K42.S[-c(2:10,20,22,24,25),-c(72:93, 7,21,4,39)], alpha=0.05, nperm=9999)
# DM, logT, PDOa, logpH
# R2adj= 0.4299874

# DM, logT, logpH, logCl

# Select only significant env variables
colnames(env.all.K42.S)
env.sel.K42.S <- env.all.K42.S[-c(2:10,20,22,24,25),c("DM","logT","logpH","logCl")]
colnames(env.sel.K42.S)

# RDA analysis with only selected
phyto.rda.sel.K42.S <- rda(phyto.K42.S.hel ~ ., data=env.sel.K42.S)
summary(phyto.rda.sel.K42.S)

vif(phyto.rda.sel.K42.S)

coef(phyto.rda.sel.K42.S)
(R2 <- RsquareAdj(phyto.rda.sel.K42.S)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K42.S)$adj.r.squared)

plot(phyto.rda.sel.K42.S, scaling=2)
phyto.sc <- scores(phyto.rda.sel.K42.S, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K42.S, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K42.S, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K42.S$CA$eig[phyto.rda.sel.K42.S$CA$eig > mean(phyto.rda.sel.K42.S$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.K42.S)

# Select only significant env variables
colnames(env.sel.K42.S)
rownames(env.sel.K42.S)
env.w <- env.sel.K42.S[,c(2:4)]
DM <- env.sel.K42.S[,c(1)]
phyto.part.all <- varpart(phyto.K42.S.hel, env.w, DM)
plot(phyto.part.all)
phyto.part.all
#-------------------------------------
# customerization
dm <- env.K42.S[-c(2:10,20,22,24,25),16]
with(env.sel.K42.S, levels(dm))

scl <- 2
colvec2 <- c("red2", "green4")

#-------------------------------------
###### Divide by DM
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.K42.S, type="n", scaling=scl) 
# add dm scores by colouring each site according to its land-use
with(env.sel.K42.S, points(phyto.rda.sel.K42.S, display="sites", col=colvec2[dm], scaling = scl, pch=21, bg=colvec2[dm]))

head(with(env.K42.S[-c(2:10,20,22,24,25),], colvec2[dm]))

# add species scores with species names
text(phyto.rda.sel.K42.S, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.K42.S, legend("topleft", legend = levels(dm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.K42.S, env.sel.K42.S, perm=999)

plot(fit, add=T, col="black")
#------------------------------------
# 6.2.1: E51 X S

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
colnames(phyto.E51.S.hel)
rownames(phyto.E51.S.hel)
colnames(envpluslog.E51.S)
rownames(envpluslog.E51.S[-c(1:19),])
rownames(envpluslog.E51.S[,])

# p < 0.05
forward.sel(phyto.E51.S.hel, envpluslog.E51.S[-c(1:19),-c(15)], alpha=0.05, nperm=9999) 
# Stability
R2adj= 0.1308484

# p < 0.08
forward.sel(phyto.E51.S.hel[,], envpluslog.E51.S[-c(1:19),-c(15)], alpha=0.08, nperm=9999) 
# Stability, logNH4
R2adj= 0.1811940

# b) indices
forward.sel(phyto.E51.S.hel, lcci[-c(1:19),], alpha=0.05, nperm=9999) 


# c) air temp
forward.sel(phyto.E51.S.hel, temp.air[-c(1:19),], alpha=0.05, nperm=9999) 
# AT10

# d) MEM
forward.sel(phyto.E51.S.hel, MEM[-c(1:19),-1], alpha=0.05, nperm=9999) 

# e) all data
# combine variables
env.all.E51.S <- cbind(envpluslog.E51.S, lcci, temp.air, MEM)
colnames(env.all.E51.S)
# rownames(env.all.E51.S)

# all env+oscoilations+temperatures
forward.sel(phyto.E51.S.hel, env.all.E51.S[-c(1:19),-c(15)], alpha=0.05, nperm=9999) 
# Stability, 3.4 year cycle, AMOs, logNH4, airtemp4
# R2adj= 0.4982486

# all env except MEM
colnames(env.all.E51.S)
forward.sel(phyto.E51.S.hel, env.all.E51.S[-c(1:19),-c(15, 72:93)], alpha=0.05, nperm=9999) 


# Select only significant env variables
colnames(env.all.E51.S)
env.sel.E51.S <- env.all.E51.S[-c(1:19),c("Stability","X3.4y.c","AMOs","logNH4", "X4")]
colnames(env.sel.E51.S)
colnames(env.sel.E51.S)[2] <- "3.4-year-cycle"
colnames(env.sel.E51.S)[5] <- "April air temperature"
# RDA analysis with only selected
phyto.rda.sel.E51.S <- rda(phyto.E51.S.hel ~ ., data=env.sel.E51.S)
summary(phyto.rda.sel.E51.S)

vif(phyto.rda.sel.E51.S)

coef(phyto.rda.sel.E51.S)
(R2 <- RsquareAdj(phyto.rda.sel.E51.S)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.E51.S)$adj.r.squared)

plot(phyto.rda.sel.E51.S, scaling=2)
phyto.sc <- scores(phyto.rda.sel.E51.S, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.E51.S, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.E51.S, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.E51.S$CA$eig[phyto.rda.sel.E51.S$CA$eig > mean(phyto.rda.sel.E51.S$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.E51.S)

# Select only significant env variables
colnames(env.sel.E51.S)
rownames(env.sel.E51.S)
env.w <- env.sel.E51.S[,c(1,4,5)]
temp <- env.sel.E51.S[,c(2,3)]
phyto.part.all <- varpart(phyto.E51.S.hel, env.w, temp)
plot(phyto.part.all)
phyto.part.all
#------------------------------------
# 6.2.4: E51 X F
# 6.2.1: K42

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
colnames(phyto.E51.F.hel)
rownames(phyto.E51.F.hel)
colnames(envpluslog.E51.F)
rownames(envpluslog.E51.F[-c(1:19,27),])
rownames(envpluslog.E51.F[,])

# p < 0.05
forward.sel(phyto.E51.F.hel, envpluslog.E51.F[-c(1:19,27),-c(4,18)], alpha=0.05, nperm=9999) 
# logpH, logSecchi
R2adj= 0.2335554

# p < 0.1
forward.sel(phyto.E51.F.hel[,], envpluslog.E51.F[-c(1:19,27),-c(4,18,11)], alpha=0.1, nperm=9999) 
# logpH, logSecchi, logSi, NH4
R2adj= 0.3648257

# b) indices
forward.sel(phyto.E51.F.hel, lcci[-c(1:19,27),], alpha=0.05, nperm=9999) 
# PDOs, SSN

# c) air temp
forward.sel(phyto.E51.F.hel, temp.air[-c(1:19,27),], alpha=0.05, nperm=9999) 
# AT2

# d) MEM
forward.sel(phyto.E51.F.hel, MEM[-c(1:19,27),-1], alpha=0.05, nperm=9999) 
# 24, 11 year cycle

# e) all data
# combine variables
env.all.E51.F <- cbind(envpluslog.E51.F, lcci, temp.air, MEM)
colnames(env.all.E51.F)
# rownames(env.all.E51.F)

# all env+oscoilations+temperatures
forward.sel(phyto.E51.F.hel, env.all.E51.F[-c(1:19,27),], alpha=0.05, nperm=9999) 
# 24 year cycle, logpH
# R2adj= 0.2721714

# all env except MEM, air temperature
colnames(env.all.E51.F)
forward.sel(phyto.E51.F.hel, env.all.E51.F[-c(1:19,27),-c(58:92)], alpha=0.05, nperm=9999) 

# logpH, logSecchi, logSi, NH4
R2adj= 0.3648257

# Select only significant env variables
colnames(env.all.E51.F)
env.sel.E51.F <- env.all.E51.F[-c(1:19,27),c("logpH","logSecchi","logSi","NH4")]
colnames(env.sel.E51.F)

# RDA analysis with only selected
phyto.rda.sel.E51.F <- rda(phyto.E51.F.hel ~ ., data=env.sel.E51.F)
summary(phyto.rda.sel.E51.F)

vif(phyto.rda.sel.E51.F)

coef(phyto.rda.sel.E51.F)
(R2 <- RsquareAdj(phyto.rda.sel.E51.F)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.E51.F)$adj.r.squared)

plot(phyto.rda.sel.E51.F, scaling=2)
phyto.sc <- scores(phyto.rda.sel.E51.F, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.E51.F, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.E51.F, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.E51.F$CA$eig[phyto.rda.sel.E51.F$CA$eig > mean(phyto.rda.sel.E51.F$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.E51.F)

# Select only significant env variables
# colnames(env.sel.E51.F)
# rownames(env.sel.E51.F)
# env.w <- env.sel.E51.F[,c(1,4,5)]
# temp <- env.sel.E51.F[,c(2,3)]
# phyto.part.all <- varpart(phyto.E51.F.hel, env.w, temp)
# plot(phyto.part.all)
# phyto.part.all


#------------------------------------

# Plot all RDA triplot together
par(mfrow = c(2, 3))
plot(phyto.rda.sel.C1.S, main="C1 Summer")
plot(phyto.rda.sel.E51.S, main="E51 Summer")
plot(phyto.rda.sel.K42.S, main="K42 Summer")
plot(phyto.rda.sel.C1.F, main="C1 Fall")
plot(phyto.rda.sel.E51.F, main="E51 Fall")
plot(phyto.rda.sel.K42.F, main="K42 Fall")