# Seasonal phyto C1 E51 K42 Summer and Fall
#---------------------

env.seasonal.all.S.F.sel

phyto.seasonal.all.2p.hel
colnames(phyto.seasonal.all.2p.hel)
rownames(phyto.seasonal.all.2p.hel)
colnames(env.seasonal.all.S.F.sel)
rownames(env.seasonal.all.S.F.sel)


# C1 E51 K42
# Forward selection
#A. RDA with all explanatory variables
phyto.all.rda.all <- rda(phyto.seasonal.all.2p.hel[-c(73,74),] ~ ., data=env.seasonal.all.S.F.sel)

phyto.all.rda.all
#B. Global adjusted R2 and R2
R2a.all=RsquareAdj(phyto.all.rda.all)$adj.r.squared
R2a.all
R2.all= RsquareAdj(phyto.all.rda.all)$r.squared
R2.all
#C. Forward selection
forward.sel(phyto.seasonal.all.2p.hel[-c(73,74),], env.seasonal.all.S.F.sel, adjR2thresh=R2a.all)
forward.sel(phyto.seasonal.all.2p.hel[-c(73,74),], env.seasonal.all.S.F.sel[,-c(4,28)], alpha=0.05, nperm=9999)
# Secchi.log, Cl.log, Chla.log, Temperature, ZM, Stability, NO3.log, Alkalinity, pH, Ca
# R2adj= 0.3969756

colnames(env.all.std)
colnames(phyto.all.hel)
# Redundancy Analysis
#A. RDA with only significant variables
phyto.all.rda.sel=rda(phyto.seasonal.all.2p.hel[-c(73,74),] ~ Secchi.log + Cl.log + Chla.log + Temperature + ZM + Stability + NO3.log + Alkalinity + pH + Ca, data=env.seasonal.all.S.F.sel) 
summary(phyto.all.rda.sel)
R2.adj=RsquareAdj(phyto.all.rda.sel)$adj.r.squared
R2.adj
plot(phyto.all.rda.sel)
vif.cca(phyto.all.rda.sel)
#ii) Tests of all canonical axes
anova.cca(phyto.all.rda.sel, by = "axis", step=9999)

#---------------------
# Variation partitioning with two sets of explanatory variables
# split variables first
colnames(env.seasonal.all.S.F.sel)
env.w <- env.seasonal.all.S.F.sel[,c(25,19,18,13,12,22,1,9,2)]
ZM <- env.seasonal.all.S.F.sel[,15]
phyto.part.all <- varpart(phyto.seasonal.all.2p.hel[-c(73,74),], env.w, ZM)
plot(phyto.part.all)

#---------------------
# customerization

site <- env.seasonal.all.S.F[,30]
season <- env.seasonal.all.S.F[,31]
dm <- env.seasonal.all.S.F[,32]
site+season+dm <- env.seasonal.all.S.F[,33]
with(env.all, levels(site))
with(env.all, levels(season))
with(env.all, levels(zm))
with(env.all, levels(site+season+dm))
scl <- 3 
colvec3 <- c("red2", "green4", "mediumblue")
colvec2 <- c("red2", "green4")

colvec10 <- c("red1", "red2","green1", "green2","blue1","blue2","yellow1","yellow2","orange1","orange2")

#---------------------
# Figure 7 based on sites
# set up the basic plot axes by using type="n"
plot(phyto.all.rda.sel, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(env.all, points(phyto.all.rda.sel, display="sites", col=colvec3[site], scaling = scl, pch=21, bg=colvec3[site]))

# add species scores with species names
text(phyto.all.rda.sel, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.all.rda.sel, legend("topright", legend = levels(site), bty = "n", col = colvec3, pch=21, pt.bg=colvec3))

fit <- envfit(phyto.all.rda.sel, env.seasonal.all.S.F.sel[,c(25,19,18,13,12,22,1,9,2,15)], perm=999)
plot(fit, add=T, col="black")

#---------------------
# Figure 8 based on DM
# set up the basic plot axes by using type="n"
plot(phyto.all.rda.sel, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(env.all, points(phyto.all.rda.sel, display="sites", col=colvec2[zm], scaling = scl, pch=21, bg=colvec2[zm]))

# add species scores with species names
text(phyto.all.rda.sel, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.all.rda.sel, legend("topright", legend = levels(zm), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.all.rda.sel, env.seasonal.all.S.F.sel[,c(25,19,18,13,12,22,1,9,2,15)], perm=999)
plot(fit, add=T, col="black")
#---------------------
# Figure 9 based on season
# set up the basic plot axes by using type="n"
plot(phyto.all.rda.sel, type="n", scaling=scl)
# add site scores by colouring each site according to its land-use
with(env.all, points(phyto.all.rda.sel, display="sites", col=colvec2[season], scaling = scl, pch=21, bg=colvec2[season]))

# add species scores with species names
text(phyto.all.rda.sel, display="species", scaling=scl, cex=0.8, col="darkcyan")

# add legend
with(phyto.all.rda.sel, legend("topright", legend = levels(season), bty = "n", col = colvec2, pch=21, pt.bg=colvec2))

fit <- envfit(phyto.all.rda.sel, env.seasonal.all.S.F.sel[,c(25,19,18,13,12,22,1,9,2,15)], perm=999)
plot(fit, add=T, col="black")