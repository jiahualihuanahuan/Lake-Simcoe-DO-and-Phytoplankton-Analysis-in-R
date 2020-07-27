# phytoplankton analysis
# PCA, Cluster analysis, breakpoint analysis, Rodionov regime shift analysis (sequential algorithm)

#------------------------------------
# PCA on phyto data
# 2%
# C1
colnames(phyto.C1.2p.hel)
rownames(phyto.C1.2p.hel)

phyto.C1.2p.pca <- rda(phyto.C1.2p.hel)
plot(phyto.C1.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.C1.2p <- phyto.C1.2p.pca$CA$eig
evplot(ev.C1.2p)
# PCA biplots
cleanplot.pca(phyto.C1.2p.pca, point=TRUE)

#------------------------------------
# C6
colnames(phyto.C6.2p.hel)
rownames(phyto.C6.2p.hel)

phyto.C6.2p.pca <- rda(phyto.C6.2p.hel)
plot(phyto.C6.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.C6.2p <- phyto.C6.2p.pca$CA$eig
evplot(ev.C6.2p)
# PCA biplots
cleanplot.pca(phyto.C6.2p.pca, point=TRUE)

#------------------------------------
# C9
colnames(phyto.C9.2p.hel)
rownames(phyto.C9.2p.hel)

phyto.C9.2p.pca <- rda(phyto.C9.2p.hel)
plot(phyto.C9.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.C9.2p <- phyto.C9.2p.pca$CA$eig
evplot(ev.C9.2p)
# PCA biplots
cleanplot.pca(phyto.C9.2p.pca, point=TRUE)

#------------------------------------
# E51
colnames(phyto.E51.2p.hel)
rownames(phyto.E51.2p.hel)

phyto.E51.2p.pca <- rda(phyto.E51.2p.hel)
plot(phyto.E51.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.E51.2p <- phyto.E51.2p.pca$CA$eig
evplot(ev.E51.2p)
# PCA biplots
cleanplot.pca(phyto.E51.2p.pca, point=TRUE)

#------------------------------------
# K39
colnames(phyto.K39.2p.hel)
rownames(phyto.K39.2p.hel)

phyto.K39.2p.pca <- rda(phyto.K39.2p.hel)
plot(phyto.K39.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.K39.2p <- phyto.K39.2p.pca$CA$eig
evplot(ev.K39.2p)
# PCA biplots
cleanplot.pca(phyto.K39.2p.pca, point=TRUE)

#------------------------------------
# K42
colnames(phyto.K42.2p.hel)
rownames(phyto.K42.2p.hel)

phyto.K42.2p.pca <- rda(phyto.K42.2p.hel)
plot(phyto.K42.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.K42.2p <- phyto.K42.2p.pca$CA$eig
evplot(ev.K42.2p)
# PCA biplots
cleanplot.pca(phyto.K42.2p.pca, point=TRUE)

#------------------------------------
# K45
colnames(phyto.K45.2p.hel)
rownames(phyto.K45.2p.hel)

phyto.K45.2p.pca <- rda(phyto.K45.2p.hel)
plot(phyto.K45.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.K45.2p <- phyto.K45.2p.pca$CA$eig
evplot(ev.K45.2p)
# PCA biplots
cleanplot.pca(phyto.K45.2p.pca, point=TRUE)

#------------------------------------
# S15
colnames(phyto.S15.2p.hel)
rownames(phyto.S15.2p.hel)

phyto.S15.2p.pca <- rda(phyto.S15.2p.hel)
plot(phyto.S15.2p.pca)
# Plot eigenvalues and % of variance for each axis
ev.S15.2p <- phyto.S15.2p.pca$CA$eig
evplot(ev.S15.2p)
# PCA biplots
cleanplot.pca(phyto.S15.2p.pca, point=TRUE)


#------------------------------------
# 1%
# C1
colnames(phyto.C1.1p.hel)
rownames(phyto.C1.1p.hel)

phyto.C1.1p.pca <- rda(phyto.C1.1p.hel)
plot(phyto.C1.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.C1.1p <- phyto.C1.1p.pca$CA$eig
evplot(ev.C1.1p)
# PCA biplots
cleanplot.pca(phyto.C1.1p.pca, point=TRUE)

#------------------------------------
# C6
colnames(phyto.C6.1p.hel)
rownames(phyto.C6.1p.hel)

phyto.C6.1p.pca <- rda(phyto.C6.1p.hel)
plot(phyto.C6.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.C6.1p <- phyto.C6.1p.pca$CA$eig
evplot(ev.C6.1p)
# PCA biplots
cleanplot.pca(phyto.C6.1p.pca, point=TRUE)

#------------------------------------
# C9
colnames(phyto.C9.1p.hel)
rownames(phyto.C9.1p.hel)

phyto.C9.1p.pca <- rda(phyto.C9.1p.hel)
plot(phyto.C9.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.C9.1p <- phyto.C9.1p.pca$CA$eig
evplot(ev.C9.1p)
# PCA biplots
cleanplot.pca(phyto.C9.1p.pca, point=TRUE)

#------------------------------------
# E51
colnames(phyto.E51.1p.hel)
rownames(phyto.E51.1p.hel)

phyto.E51.1p.pca <- rda(phyto.E51.1p.hel)
plot(phyto.E51.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.E51.1p <- phyto.E51.1p.pca$CA$eig
evplot(ev.E51.1p)
# PCA biplots
cleanplot.pca(phyto.E51.1p.pca, point=TRUE)

#------------------------------------
# K39
colnames(phyto.K39.1p.hel)
rownames(phyto.K39.1p.hel)

phyto.K39.1p.pca <- rda(phyto.K39.1p.hel)
plot(phyto.K39.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.K39.1p <- phyto.K39.1p.pca$CA$eig
evplot(ev.K39.1p)
# PCA biplots
cleanplot.pca(phyto.K39.1p.pca, point=TRUE)

#------------------------------------
# K42
colnames(phyto.K42.1p.hel)
rownames(phyto.K42.1p.hel)

phyto.K42.1p.pca <- rda(phyto.K42.1p.hel)
plot(phyto.K42.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.K42.1p <- phyto.K42.1p.pca$CA$eig
evplot(ev.K42.1p)
# PCA biplots
cleanplot.pca(phyto.K42.1p.pca, point=TRUE)

#------------------------------------
# K45
colnames(phyto.K45.1p.hel)
rownames(phyto.K45.1p.hel)

phyto.K45.1p.pca <- rda(phyto.K45.1p.hel)
plot(phyto.K45.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.K45.1p <- phyto.K45.1p.pca$CA$eig
evplot(ev.K45.1p)
# PCA biplots
cleanplot.pca(phyto.K45.1p.pca, point=TRUE)

#------------------------------------
# S15
colnames(phyto.S15.1p.hel)
rownames(phyto.S15.1p.hel)

phyto.S15.1p.pca <- rda(phyto.S15.1p.hel)
plot(phyto.S15.1p.pca)
# Plot eigenvalues and % of variance for each axis
ev.S15.1p <- phyto.S15.1p.pca$CA$eig
evplot(ev.S15.1p)
# PCA biplots
cleanplot.pca(phyto.S15.1p.pca, point=TRUE)






