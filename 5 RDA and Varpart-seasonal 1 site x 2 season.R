# RDA analysis and variation partitioning


# 6. seasonal phyto analysis
# 6.1: 1 site 2 seasons
# 6.1.1: C1 X S F
### env + log
env.C1.S.F
# colnames(env.C1.S.F)
# rownames(env.C1.S.F)
env.C1.S.F.sel <- env.C1.S.F[,]
env.C1.S.F.log <- log(env.C1.S.F.sel[,-c(15,16,17)])
envpluslog.C1.S.F <- cbind(env.C1.S.F.sel, env.C1.S.F.log)
colnames(envpluslog.C1.S.F)[18:31] <- c("logAlk","logCa","logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.C1.S.F)
# rownames(envpluslog.C1.S.F)
# head(envpluslog.C1.S.F)

# combine variables
#env.all.C1.S.F <- cbind(envpluslog.C1.S.F, lcci, temp.air, ice.s, MEM)
#colnames(env.all.C1.S.F)
#rownames(env.all.C1.S.F)

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
# colnames(envpluslog.C1.S.F)
# forward.sel(phyto.C1.S.F.2p.hel, envpluslog.C1.S.F[-c(1:20,65),-c(16,17, 29,21)], alpha=0.05, nperm=9999)
# DM, Cl, Temperature, logTP
# R2adj=0.24353127

# b) indices
# forward.sel(phyto.C1.S.F.2p.hel, lcci.S.F[-c(1:20,65),], alpha=0.05, nperm=9999) 
# rowames(lcci.S.F[-c(1:20,65),])
# rownames(phyto.hel.C1.S.F)
# AMO

# c) all data
# combine variables
envpluslog.lcci.C1.S.F <- cbind(envpluslog.C1.S.F, lcci.S.F)
# colnames(envpluslog.lcci.C1.S.F)
# rownames(envpluslog.lcci.C1.S.F)

# forward.sel(phyto.C1.S.F.2p.hel, envpluslog.lcci.C1.S.F[-c(1:20,65),-c(16,17,29,21)], alpha=0.05, nperm=9999) # 
# DM, Cl, Temperature, logTP

# Select only significant env variables
colnames(envpluslog.C1.S.F)
env.sel.C1.S.F <- envpluslog.C1.S.F[-c(1:20,65),c("DM", "logCl", "Temperature", "logTP")]

# RDA analysis with only selected
phyto.rda.sel.C1.S.F <- rda(phyto.C1.S.F.2p.hel ~ ., data=env.sel.C1.S.F)
summary(phyto.rda.sel.C1.S.F)

vif(phyto.rda.sel.C1.S.F)

coef(phyto.rda.sel.C1.S.F)
(R2 <- RsquareAdj(phyto.rda.sel.C1.S.F)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.C1.S.F)$adj.r.squared)

plot(phyto.rda.sel.C1.S.F, scaling=1)
phyto.sc <- scores(phyto.rda.sel.C1.S.F, choices=1:2, scaling=2, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.C1.S.F, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.C1.S.F, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.C1.S.F$CA$eig[phyto.rda.sel.C1.S.F$CA$eig > mean(phyto.rda.sel.C1.S.F$CA$eig)]

#---------------------
# customerization

season <- envpluslog.C1.S.F[-c(1:20,65),16]
zm <- envpluslog.C1.S.F[-c(1:20,65),17]
with(envpluslog.C1.S.F[-c(1:20,65),], levels(season))
with(envpluslog.C1.S.F[-c(1:20,65),], levels(zm))

scl <- 2
colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")

# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.C1.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.C1.S.F, points(phyto.rda.sel.C1.S.F, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

head(with(envpluslog.C1.S.F, colvec2[season]))

# add species scores with species names
text(phyto.rda.sel.C1.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.C1.S.F, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.C1.S.F, envpluslog.C1.S.F[-c(1:20,65),c("DM", "logCl", "Temperature", "logTP")], perm=999)
plot(fit, add=T, col="black")

#---------------------
scl <- 2
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.C1.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.C1.S.F, points(phyto.rda.sel.C1.S.F, display="sites", col=colvec2[zm], scaling = scl, pch=21, bg=colvec2[zm]))

head(with(envpluslog.C1.S.F, colvec2[zm]))

# add species scores with species names
text(phyto.rda.sel.C1.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.C1.S.F, legend("topright", legend = levels(zm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.C1.S.F, envpluslog.C1.S.F[-c(1:20,65),c("DM", "logCl", "Temperature", "logTP")], perm=999)
plot(fit, add=T, col="black")

#---------------------
# 6.1.2: K42 X S F
### env + log
env.K42.S.F
# colnames(env.K42.S.F)
# rownames(env.K42.S.F)
env.K42.S.F.sel <- env.K42.S.F[,]
env.K42.S.F.log <- log(env.K42.S.F.sel[,-c(15,16,17)])
envpluslog.K42.S.F <- cbind(env.K42.S.F.sel, env.K42.S.F.log)
colnames(envpluslog.K42.S.F)[18:31] <- c("logAlk","logCa","logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.K42.S.F)
# rownames(envpluslog.K42.S.F)
# head(envpluslog.K42.S.F)

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
# colnames(envpluslog.K42.S.F)
# rownames(envpluslog.K42.S.F)
# colnames(phyto.K42.S.F.2p.hel)
# rownames(phyto.K42.S.F.2p.hel)

# forward.sel(phyto.K42.S.F.2p.hel[-c(1:2),], envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),-c(16,17)], alpha=0.06, nperm=9999) 
# logCl, T, Si
# R2 adj= 0.3651942

# b) indices
# forward.sel(phyto.K42.S.F.2p.hel[-c(1:2),], lcci.S.F[-c(1:20,39,40,44,47:50),], alpha=0.05, nperm=9999) 
# Wind speed and ENSO

# c) all data
# combine variables
# envpluslog.lcci.K42.S.F <- cbind(envpluslog.K42.S.F, lcci.S.F)
# colnames(envpluslog.lcci.K42.S.F)
# rownames(envpluslog.lcci.K42.S.F)

# forward.sel(phyto.K42.S.F.2p.hel[-c(1:2),], envpluslog.lcci.K42.S.F[-c(1:20,39,40,44,47:50),-c(4,16,17)], alpha=0.05, nperm=9999) 


# Select only significant env variables
colnames(envpluslog.K42.S.F)
env.sel.K42.S.F <- envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),c("logCl", "Temperature", "Si")]

# RDA analysis with only selected
phyto.rda.sel.K42.S.F <- rda(phyto.K42.S.F.2p.hel[-c(1:2),] ~ ., data=env.sel.K42.S.F)
summary(phyto.rda.sel.K42.S.F)

vif(phyto.rda.sel.K42.S.F)

coef(phyto.rda.sel.K42.S.F)
(R2 <- RsquareAdj(phyto.rda.sel.K42.S.F)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.K42.S.F)$adj.r.squared)

plot(phyto.rda.sel.K42.S.F, scaling=1)
phyto.sc <- scores(phyto.rda.sel.K42.S.F, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.K42.S.F, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.K42.S.F, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.K42.S.F$CA$eig[phyto.rda.sel.K42.S.F$CA$eig > mean(phyto.rda.sel.K42.S.F$CA$eig)]

# ### Variation partitioning with two sets of explanatory variables
# # split variables first
# colnames(envpluslog.K42.S.F)

# forward.sel(phyto.hel.K42.S.F[-c(1:2),], env.all.K42.S.F[-c(1:20,39,40,44,47:50),-c(4,16,17)], alpha=0.05, nperm=9999) # 

# env.w <- env.all.K42.S.F[-c(1:20,39,40,44,47:50),-c(4,16,17)][,c(18,12,10)]
# DM <-env.all.K42.S.F[-c(1:20,39,40,44,47:50),-c(4,16,17)][,c(14)]
# phyto.part.all <- varpart(phyto.hel.K42.S.F[-c(1:2),], env.w, DM)
# plot(phyto.part.all)
# phyto.part.all

#---------------------
# customerization

season <- env.K42.S.F[-c(1:20,39,40,44,47:50),16]
zm <- env.K42.S.F[-c(1:20,39,40,44,47:50),17]
with(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),], levels(season))
with(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),], levels(zm))


colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")

scl <- 2
# 1 colour-coded each year+season according to its season
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.K42.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its season
with(env.sel.K42.S.F, points(phyto.rda.sel.K42.S.F, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

head(with(env.sel.K42.S.F, colvec2[season]))

# add species scores with species names
text(phyto.rda.sel.K42.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.K42.S.F, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.K42.S.F, env.sel.K42.S.F, perm=999)
plot(fit, add=T, col="black")

#---------------------
scl <- 2
# 2 colour-coded each year+season according to invasive dressinid mussels
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.K42.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.K42.S.F, points(phyto.rda.sel.K42.S.F, display="sites", col=colvec2[zm], scaling = scl, pch=21, bg=colvec2[zm]))
summary(phyto.rda.sel.K42.S.F)

head(with(envpluslog.K42.S.F, colvec2[zm]))

# add species scores with species names
text(phyto.rda.sel.K42.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.K42.S.F, legend("topright", legend = levels(zm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.K42.S.F, env.sel.K42.S.F, perm=999)
plot(fit, add=T, col="black")

colnames(phyto.K42.S.F.2p.hel[-c(1:2),])
rownames(phyto.K42.S.F.2p.hel[-c(1:2),])
colnames(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),])
rownames(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),])
#---------------------

# 6.1.3: E51 X S F
### env + log
# env.E51.S.F
# colnames(env.E51.S.F)
# rownames(env.E51.S.F)
env.E51.S.F.sel <- env.E51.S.F[,]
env.E51.S.F.log <- log(env.E51.S.F.sel[,-c(15,16,17)])
envpluslog.E51.S.F <- cbind(env.E51.S.F.sel, env.E51.S.F.log)
colnames(envpluslog.E51.S.F)[18:31] <- c("logAlk","logCa","logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.E51.S.F)
# rownames(envpluslog.E51.S.F)
# head(envpluslog.E51.S.F)

# combine variables
#env.all.E51.S.F <- cbind(envpluslog.E51.S.F, lcci, temp.air, ice.s, MEM)
#colnames(env.all.E51.S.F)
#rownames(env.all.E51.S.F)

# Standardized environmental variables
# A: forward.sel() function automaticaly run standardization on all variables

### forward selection
# a) env
# colnames(envpluslog.E51.S.F)
# rownames(envpluslog.E51.S.F)
# colnames(phyto.E51.S.F.2p.hel)
# rownames(phyto.E51.S.F.2p.hel)

# forward.sel(phyto.E51.S.F.2p.hel[,], envpluslog.E51.S.F[-c(1:38,53),-c(15,16,24)], alpha=0.05, nperm=9999) 
# Si, T, NTK
# R2adj= 0.2480209

# b) indices
# forward.sel(phyto.E51.S.F.2p.hel[,], lcci.S.F[-c(1:38,53),], alpha=0.05, nperm=9999) 
# Wind speed

# e) all data
# combine variables
envpluslog.lcci.E51.S.F <- cbind(envpluslog.E51.S.F, lcci.S.F)
# colnames(envpluslog.lcci.E51.S.F)
# rownames(envpluslog.lcci.E51.S.F)

# forward.sel(phyto.E51.S.F.2p.hel[,], envpluslog.lcci.E51.S.F[-c(1:38,53),-c(15,16,23)], alpha=0.05, nperm=9999) 
# Si, T, NTK, AMO
# R2adj= 0.2838024

# Select only significant env variables
colnames(envpluslog.E51.S.F)
env.sel.E51.S.F <- envpluslog.lcci.E51.S.F[-c(1:38,53),c("Si", "Temperature", "NTK", "AMO")]
colnames(env.sel.E51.S.F)[3] <- "TKN"

# RDA analysis with only selected
phyto.rda.sel.E51.S.F <- rda(phyto.E51.S.F.2p.hel[,] ~ ., data=env.sel.E51.S.F)
summary(phyto.rda.sel.E51.S.F)

vif(phyto.rda.sel.E51.S.F)

coef(phyto.rda.sel.E51.S.F)
(R2 <- RsquareAdj(phyto.rda.sel.E51.S.F)$r.squared)
(R2adj <- RsquareAdj(phyto.rda.sel.E51.S.F)$adj.r.squared)

plot(phyto.rda.sel.E51.S.F, scaling=2)
phyto.sc <- scores(phyto.rda.sel.E51.S.F, choices=1:2, scaling=1, display="sp")
arrows(0,0, phyto.sc[,1], phyto.sc[,2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(phyto.rda.sel.E51.S.F, step=1000)
# Test of all canonical axes
anova.cca(phyto.rda.sel.E51.S.F, by="axis", step=1000)

# Apply Kaiser-Guttan criterion to residual axes
phyto.rda.sel.E51.S.F$CA$eig[phyto.rda.sel.E51.S.F$CA$eig > mean(phyto.rda.sel.E51.S.F$CA$eig)]

### Variation partitioning with two sets of explanatory variables
# split variables first
colnames(envpluslog.E51.S.F)

forward.sel(phyto.hel.E51.S.F[,], env.all.E51.S.F[-c(1:38),-c(15,16,24)], alpha=0.05, nperm=9999) # 

# this result is not avaiable to do the variation partitioning
########xxx
#env.w <- env.all.E51.S.F[-c(1:20,39,40,44,47:50),-c(4,16,17)][,c(18,12,10)]
#DM <-env.all.E51.S.F[-c(1:20,39,40,44,47:50),-c(4,16,17)][,c(14)]
#phyto.part.all <- varpart(phyto.hel.E51.S.F[-c(1:2),], env.w, DM)
#plot(phyto.part.all)
#phyto.part.all

#---------------------
# customerization
phyto.hel.E51.S.F
envpluslog.E51.S.F
phyto.rda.sel.E51.S.F

season <- envpluslog.lcci.E51.S.F[-c(1:38,53),16]
zm <- envpluslog.lcci.E51.S.F[-c(1:38,53),17]
with(envpluslog.lcci.E51.S.F[-c(1:38,53),], levels(season))
with(envpluslog.lcci.E51.S.F[-c(1:38,53),], levels(zm))

scl <- 1
colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")

# 1 colour-coded each year+season according to its season
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.E51.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its season
with(env.sel.E51.S.F, points(phyto.rda.sel.E51.S.F, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

head(with(env.sel.E51.S.F, colvec2[season]))

# add species scores with species names
text(phyto.rda.sel.E51.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.E51.S.F, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.E51.S.F, env.sel.E51.S.F, perm=999)
plot(fit, add=T, col="black")
#---------------------#---------------------
# 2 colour-coded each year+season according to its DM
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.E51.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its season
with(env.E51.S.F[-c(1:38,53),], points(phyto.rda.sel.E51.S.F, display="sites", col=colvec2[zm], scaling = scl, pch=21, bg=colvec2[zm]))

head(with(env.E51.S.F[-c(1:38,53),], colvec2[zm]))

# add species scores with species names
text(phyto.rda.sel.E51.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.E51.S.F, legend("topright", legend = levels(zm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.E51.S.F, env.E51.S.F[-c(1:38,53),c("Si", "Temperature", "NTK", "AMO")], perm=999)
plot(fit, add=T, col="black")

#---------------------#---------------------
#---------------------#---------------------
#---------------------#---------------------
#---------------------#---------------------








# Adjust figure margins to fit all graphs in one figure

par(mfrow = c(3, 2))
par(mar=c(0,0,0,0)) # adjust margins (1,2,3,4)
# 1 bottom, 2 left, 3 top, 4 right

# 1.1 C1 S F season
season <- envpluslog.C1.S.F[-c(1:20,65),16]
zm <- envpluslog.C1.S.F[-c(1:20,65),17]
with(envpluslog.C1.S.F[-c(1:20,65),], levels(season))
with(envpluslog.C1.S.F[-c(1:20,65),], levels(zm))

scl <- 2
colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")

# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.C1.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.C1.S.F, points(phyto.rda.sel.C1.S.F, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

head(with(envpluslog.C1.S.F, colvec2[season]))

# add species scores with species names
text(phyto.rda.sel.C1.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.C1.S.F, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.C1.S.F, envpluslog.C1.S.F[-c(1:20,65),c("DM", "logCl", "Temperature", "logTP")], perm=999)
plot(fit, add=T, col="black")

#---------------------
# 1.2 C1 S F DM
scl <- 2
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.C1.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.C1.S.F, points(phyto.rda.sel.C1.S.F, display="sites", col=colvec2[zm], scaling = scl, pch=21, bg=colvec2[zm]))

head(with(envpluslog.C1.S.F, colvec2[zm]))

# add species scores with species names
text(phyto.rda.sel.C1.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.C1.S.F, legend("topright", legend = levels(zm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.C1.S.F, envpluslog.C1.S.F[-c(1:20,65),c("DM", "logCl", "Temperature", "logTP")], perm=999)
plot(fit, add=T, col="black")

#---------------------
#---------------------
# # 2.1 K42 S F season

season <- env.K42.S.F[-c(1:20,39,40,44,47:50),16]
zm <- env.K42.S.F[-c(1:20,39,40,44,47:50),17]
with(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),], levels(season))
with(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),], levels(zm))


colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")

scl <- 2
# 1 colour-coded each year+season according to its season
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.K42.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its season
with(env.sel.K42.S.F, points(phyto.rda.sel.K42.S.F, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

head(with(env.sel.K42.S.F, colvec2[season]))

# add species scores with species names
text(phyto.rda.sel.K42.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.K42.S.F, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.K42.S.F, env.sel.K42.S.F, perm=999)
plot(fit, add=T, col="black")

#---------------------
# 2.2 K42 S F DM
# 2 colour-coded each year+season according to invasive dressinid mussels
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.K42.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(envpluslog.K42.S.F, points(phyto.rda.sel.K42.S.F, display="sites", col=colvec2[zm], scaling = scl, pch=21, bg=colvec2[zm]))
summary(phyto.rda.sel.K42.S.F)

head(with(envpluslog.K42.S.F, colvec2[zm]))

# add species scores with species names
text(phyto.rda.sel.K42.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.K42.S.F, legend("topright", legend = levels(zm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.K42.S.F, env.sel.K42.S.F, perm=999)
plot(fit, add=T, col="black")

colnames(phyto.K42.S.F.2p.hel[-c(1:2),])
rownames(phyto.K42.S.F.2p.hel[-c(1:2),])
colnames(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),])
rownames(envpluslog.K42.S.F[-c(1:20,39,40,44,47:50),])

#---------------------
# 3 E51 S F season
season <- env.E51.S.F[-c(1:38,53),16]
zm <- env.E51.S.F[-c(1:38,53),17]
with(env.E51.S.F[-c(1:38,53),], levels(season))
with(env.E51.S.F[-c(1:38,53),], levels(zm))

scl <- 2
colvec2 <- c("red2", "green4")
colvec3 <- c("red2", "green4", "mediumblue")

# 1 colour-coded each year+season according to its season
# set up the basic plot axes by using type="n"
plot(phyto.rda.sel.E51.S.F, type="n", scaling=scl)
# add site scores by colouring each site according to its season
with(envpluslog.lcci.E51.S.F[-c(1:38,53),], points(phyto.rda.sel.E51.S.F, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

head(with(envpluslog.lcci.E51.S.F[-c(1:38,53),], colvec2[season]))

# add species scores with species names
text(phyto.rda.sel.E51.S.F, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.rda.sel.E51.S.F, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.rda.sel.E51.S.F, envpluslog.lcci.E51.S.F[-c(1:38,53),c("Si", "Temperature", "NTK", "AMO")], perm=999)
plot(fit, add=T, col="black")
