### Transformation and Data Cleaning
# the main purpose here is 
# 1) check env data if follow normal distribution, if not, log transform them
# 2) Hellinger transformation on phyto data


#---------------------
# Check env variables for normal distribution
# methods used: 1)qq-plot and 2)Shapiro-Wilk test
# annual env
#--------------------
# all
# colnames(env.annual.all)
env.annual.all.sel <- env.annual.all[,-c(15,60,61)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:158){
  # qqnorm(env.annual.all.sel[,i], main=colnames(env.annual.all.sel[i]))
  # qqline(env.annual.all.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.annual.all.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.annual.all.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# except DO
#--------------------
# C1
env.C1.sel <- env.C1[,-c(2,15)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.C1.sel[,i], main=colnames(env.C1.sel[i]))
  # qqline(env.C1.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.C1.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.C1.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# Cl, Chl.a, NO3, Si, Stability, TP
#---------------------
# C6
# colnames(env.C6)
# rownames(env.C6)
env.C6.sel <- env.C6[,-c(1,2,4,7,9,15)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.C6.sel[,i], main=colnames(env.C6.sel[i]))
  # qqline(env.C6.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.C6.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.C6.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# NH4, NTK, Secchi, Si, TP

#---------------------
# C9
# colnames(env.C9)
# rownames(env.C9)
env.C9.sel <- env.C9[,-c(1,2,4,7,9,15)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.C9.sel[,i], main=colnames(env.C9.sel[i]))
  # qqline(env.C9.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.C9.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.C9.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# NH4, NTK, Secchi, Si, TP

#---------------------
# E51
# colnames(env.E51)
# rownames(env.E51)
env.E51.sel <- env.E51[,-c(1,2,4,7,9,15)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.E51.sel[,i], main=colnames(env.E51.sel[i]))
  # qqline(env.E51.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.E51.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.E51.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# NH4, NTK, Secchi, Si, Stability (0.0626), TP
#---------------------
# K39
# colnames(env.K39)
# rownames(env.K39)
env.K39.sel <- env.K39[,-c(1,2,4,7,9,15)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.K39.sel[,i], main=colnames(env.K39.sel[i]))
  # qqline(env.K39.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.K39.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.K39.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# Chla, NH4, NTK, Secchi (0.1033795), Si (0.065), TP
#---------------------
# K42
# colnames(env.K42)
# rownames(env.K42)
env.K42.sel <- env.K42[,-4]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(5, 5))
# for (i in 1:22){
  # qqnorm(env.K42.sel[,i], main=colnames(env.K42.sel[i]))
  # qqline(env.K42.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.K42.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.K42.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# Alka (0.0611646), NH4, NO3, NTK, pH, Si, Temperature, TP,   VWHT, overturn, S.in, S.de
#---------------------
# K45
# colnames(env.K45)
# rownames(env.K45)
env.K45.sel <- env.K45[,-c(2,4,7,9,15)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.K45.sel[,i], main=colnames(env.K45.sel[i]))
  # qqline(env.K45.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.K45.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.K45.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# Alka (0.04511405), NH4, NTK, Si, Temperature, TP
#---------------------
# S15
# colnames(env.S15)
# rownames(env.S15)
env.S15.sel <- env.S15[,-c(2,4,9,15)]
env.S15.sel.s <- env.S15.sel[-c(1:5),]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.S15.sel.s[,i], main=colnames(env.S15.sel.s[i]))
  # qqline(env.S15.sel.s[,i]) 
  # shapiro.W[i] <- shapiro.test(env.S15.sel.s[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.S15.sel.s[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# Alka (0.04511405), NH4, NTK, Si, Temperature, TP

# Seasonal phyto data normality check
#---------------------
# C1.S.F
# colnames(env.C1.S.F)
env.C1.S.F.sel <- env.C1.S.F[,-c(1,2,7,15:17)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:11){
  # qqnorm(env.C1.S.F.sel[,i], main=colnames(env.C1.S.F.sel[i]))
  # qqline(env.C1.S.F.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.C1.S.F.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.C1.S.F.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:Chla, NH4, NTK,pH, Si, S, T, TP
# except: Cl DO, Secchi

#---------------------
# E51.S.F
# colnames(env.E51.S.F)
env.E51.S.F.sel <- env.E51.S.F[,-c(15,16,17)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.E51.S.F.sel[,i], main=colnames(env.E51.S.F.sel[i]))
  # qqline(env.E51.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.E51.S.F.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.E51.S.F.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# except Cl and Temperature
#---------------------
# K42.S.F
env.K42.S.F.sel <- env.K42.S.F[,-c(1,2,7,9,15,16,17)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:15){
  # qqnorm(env.K42.S.F.sel[,i], main=colnames(env.K42.S.F.sel[i]))
  # qqline(env.K42.S.F.sel[,i]) 
  # shapiro.W[i] <- shapiro.test(env.K42.S.F.sel[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.K42.S.F.sel[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:
# except: DO, T, S
#---------------------
# env.seasonal.all
# colnames(env.seasonal.all.S.F)
env.seasonal.all.S.F.select <- env.seasonal.all.S.F[,c(1:14)]
# shapiro.W <- c()
# shapiro.p <- c()
# par(mfrow = c(3, 5))
# for (i in 1:30){
  # qqnorm(env.seasonal.all.S.F.select[,i], main=colnames(env.K42.S.F.select[i]))
  # qqline(env.K42.S.F.select[,i]) 
  # shapiro.W[i] <- shapiro.test(env.seasonal.all.S.F.select[,i])[1]
  # shapiro.p[i] <- shapiro.test(env.seasonal.all.S.F.select[,i])[2]
# }
# shapiro.W
# shapiro.p
# NOT normally distributed:


#---------------------
#---------------------
#---------------------
# log transformation on environmental variables
#---------------------
# annual all
DM <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# colnames(env.annual.all.sel)
# rownames(env.annual.all.sel)
colnames(env.annual.all)

env.annual.all[14,12] <- 0.00000001
env.annual.all.sel <- env.annual.all[,c(3,6,8,10:14)]
env.annual.all.sel.log <- log(env.annual.all.sel)
envpluslog.annual.all <- cbind(env.annual.all.sel, env.annual.all.sel.log, env.annual.all[,c(17:59)], DM)
# colnames(envpluslog.annual.all)
# rownames(envpluslog.annual.all)
colnames(envpluslog.annual.all)[9:16] <- c("logChla","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.annual.all)
# rownames(envpluslog.annual.all)
#---------------------
# C1
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.C1.log <- log(env.C1.sel)
# colnames(env.C1.sel)
# rownames(env.C1.sel)
envpluslog.C1 <- cbind(env.C1.sel, env.C1.log,DM)
# colnames(envpluslog.C1)
# rownames(envpluslog.C1)
colnames(envpluslog.C1)[14:26] <- c("logAlka","logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.C1)
# rownames(envpluslog.C1)
envpluslog.C1[14,24] <- 0
#---------------------
# C6
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.C6.log <- log(env.C6.sel)
# colnames(env.C6.sel)
# rownames(env.C6.sel)
envpluslog.C6 <- cbind(env.C6.sel, env.C6.log,DM)
# colnames(envpluslog.C6)
# rownames(envpluslog.C6)
colnames(envpluslog.C6)[10:18] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.C6)
# rownames(envpluslog.C6)
#---------------------
# C9
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.C9.log <- log(env.C9.sel)
# colnames(env.C9.sel)
# rownames(env.C9.sel)
envpluslog.C9 <- cbind(env.C9.sel, env.C9.log,DM)
# colnames(envpluslog.C9)
# rownames(envpluslog.C9)
colnames(envpluslog.C9)[10:18] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.C9)
# rownames(envpluslog.C9)
#---------------------
# E51
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.E51.log <- log(env.E51.sel)
# colnames(env.E51.sel)
# rownames(env.E51.sel)
envpluslog.E51 <- cbind(env.E51.sel, env.E51.log,DM)
# colnames(envpluslog.E51)
# rownames(envpluslog.E51)
colnames(envpluslog.E51)[10:18] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.E51)
# rownames(envpluslog.E51)
#---------------------
# K39
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.K39.log <- log(env.K39.sel)
# colnames(env.K39.sel)
# rownames(env.K39.sel)
envpluslog.K39 <- cbind(env.K39.sel, env.K39.log,DM)
# colnames(envpluslog.K39)
# rownames(envpluslog.K39)
colnames(envpluslog.K39)[10:18] <- c("logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.K39)
# rownames(envpluslog.K39)
#---------------------
# K42 1980-2008
# no need to transform data

#---------------------
# K42 1980-2012
# no need to transform data

#---------------------
# K45
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.K45.log <- log(env.K45.sel)
# colnames(env.K45.sel)
# rownames(env.K45.sel)
envpluslog.K45 <- cbind(env.K45.sel, env.K45.log,DM)
# colnames(envpluslog.K45)
# rownames(envpluslog.K45)
colnames(envpluslog.K45)[11:20] <- c("logAlka","logChla","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.K45)
# rownames(envpluslog.K45)
#---------------------
# S15
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
env.S15.log <- log(env.S15.sel)
# colnames(env.S15.sel)
# rownames(env.S15.sel)
envpluslog.S15 <- cbind(env.S15.sel, env.S15.log,DM)
# colnames(envpluslog.S15)
# rownames(envpluslog.S15)
colnames(envpluslog.S15)[12:22] <- c("logAlka","logChla","logDO","logNO3","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.S15)
# rownames(envpluslog.S15)
#---------------------
# Seasonal env
# Fall and Summer all three sites
env.seasonal.all.S.F.sel <- env.seasonal.all.S.F[,-c(30:32)]
# colnames(env.seasonal.all.S.F.sel)
# colnames(env.seasonal.all.S.F)
#---------------------
# C1 S and F
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# head(env.C1.S.F)
# colnames(env.C1.S.F)
env.C1.S.F.sel[c(1,27,32,42,65),9] <- 0.000000001
env.C1.S.F.log <- log(env.C1.S.F.sel)
# colnames(env.C1.S.F.sel)
# rownames(env.C1.S.F.sel)
envpluslog.C1.S.F <- cbind(env.C1.S.F.sel, env.C1.S.F.log,DM)
# colnames(envpluslog.C1.S.F)
# rownames(envpluslog.C1.S.F)
colnames(envpluslog.C1.S.F)[12:22] <- c("log Chla","logCl","logDO","logNH4","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.C1.S.F)
# rownames(envpluslog.C1.S.F)
#---------------------
# K42 S and F
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# head(env.K42.S.F)
# colnames(env.K42.S.F)
env.K42.S.F.log <- log(env.K42.S.F.sel)
# colnames(env.K42.S.F.sel)
# rownames(env.K42.S.F.sel)
envpluslog.K42.S.F <- cbind(env.K42.S.F.sel, env.K42.S.F.log,DM)
# colnames(envpluslog.K42.S.F)
# rownames(envpluslog.K42.S.F)
colnames(envpluslog.K42.S.F)[11:20] <- c("log Chla","logCl","logDO","logNH4","logNTK","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.K42.S.F)
# rownames(envpluslog.K42.S.F)
#---------------------
# E51 S F
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# head(env.E51.S.F)
# colnames(env.E51.S.F)
# colnames(env.E51.S.F.sel)
env.E51.S.F.sel[17,12] <- 0.0000000001
env.E51.S.F.log <- log(env.E51.S.F.sel)
# colnames(env.E51.S.F.sel)
# rownames(env.E51.S.F.sel)
envpluslog.E51.S.F <- cbind(env.E51.S.F.sel, env.E51.S.F.log,DM)
# colnames(envpluslog.E51.S.F)
# rownames(envpluslog.E51.S.F)
colnames(envpluslog.E51.S.F)[15:28] <- c("logAlkalinity", "logCa", "log Chla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
# colnames(envpluslog.E51.S.F)
# rownames(envpluslog.E51.S.F)

#---------------------
# Seasonal env individual season: S and F

# C1 F
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
head(env.C1.F)
colnames(env.C1.F)
env.C1.F[c(1,14,33),12] <- 0.00000000000001
env.C1.F.sel <- env.C1.F[,-c(15,16)]
colnames(env.C1.F.sel)
env.C1.F.log <- log(env.C1.F.sel)
colnames(env.C1.F.log)
rownames(env.C1.F.log)
envpluslog.C1.F <- cbind(env.C1.F.sel, env.C1.F.log,DM)
colnames(envpluslog.C1.F)
rownames(envpluslog.C1.F)
colnames(envpluslog.C1.F)[15:28] <- c("logAlkalinity", "logCa", "logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.C1.F)
rownames(envpluslog.C1.F)


#---------------------
# C1 S
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
head(env.C1.S)
colnames(env.C1.S)
env.C1.S[c(16,21),12] <- 0.00000000000001
env.C1.S.sel <- env.C1.S[,-c(15,16)]
colnames(env.C1.S.sel)
env.C1.S.log <- log(env.C1.S.sel)
colnames(env.C1.S.log)
rownames(env.C1.S.log)
envpluslog.C1.S <- cbind(env.C1.S.sel, env.C1.S.log,DM)
colnames(envpluslog.C1.S)
rownames(envpluslog.C1.S)
colnames(envpluslog.C1.S)[15:28] <- c("logAlkalinity", "logCa", "logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.C1.S)
rownames(envpluslog.C1.S)

#---------------------
# E51 S
head(env.E51.S)
colnames(env.E51.S)
env.E51.S.sel <- env.E51.S
colnames(env.E51.S.sel)
env.E51.S.log <- log(env.E51.S.sel)
colnames(env.E51.S.sel)
rownames(env.E51.S.sel)
envpluslog.E51.S <- cbind(env.E51.S.sel, env.E51.S.log)[,-30]
colnames(envpluslog.E51.S)
rownames(envpluslog.E51.S)
colnames(envpluslog.E51.S)[16:29] <- c("logAlkalinity", "logCa", "logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.E51.S)
rownames(envpluslog.E51.S)


#---------------------
# E51 F
head(env.E51.F)
colnames(env.E51.F)
env.E51.F.sel <- env.E51.F
colnames(env.E51.F.sel)
env.E51.F.log <- log(env.E51.F.sel)
colnames(env.E51.F.sel)
rownames(env.E51.F.sel)
envpluslog.E51.F <- cbind(env.E51.F.sel, env.E51.F.log)[,-30]
colnames(envpluslog.E51.F)
rownames(envpluslog.E51.F)
colnames(envpluslog.E51.F)[15:28] <- c("logAlkalinity", "logCa", "logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.E51.F)
rownames(envpluslog.E51.F)

#---------------------
# K42 S
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
head(env.K42.S)
colnames(env.K42.S)
env.K42.S.sel <- env.K42.S[,-c(15,16)]
colnames(env.K42.S.sel)
env.K42.S.log <- log(env.K42.S.sel)
colnames(env.K42.S.log)
rownames(env.K42.S.log)
envpluslog.K42.S <- cbind(env.K42.S.sel, env.K42.S.log,DM)
colnames(envpluslog.K42.S)
rownames(envpluslog.K42.S)
colnames(envpluslog.K42.S)[15:28] <- c("logAlkalinity", "logCa", "logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.K42.S)
rownames(envpluslog.K42.S)

#---------------------
# K42 F
DM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
head(env.K42.F)
colnames(env.K42.F)
env.K42.F[c(14),12] <- 0.00000000000001
env.K42.F.sel <- env.K42.F[,-c(15,16)]
colnames(env.K42.F.sel)
env.K42.F.log <- log(env.K42.F.sel)
colnames(env.K42.F.log)
rownames(env.K42.F.log)
envpluslog.K42.F <- cbind(env.K42.F.sel, env.K42.F.log,DM)
colnames(envpluslog.K42.F)
rownames(envpluslog.K42.F)
colnames(envpluslog.K42.F)[15:28] <- c("logAlkalinity", "logCa", "logChla","logCl","logDO","logNH4","logNO3","logNTK","logpH","logSecchi","logSi","logS","logT","logTP")
colnames(envpluslog.K42.F)
rownames(envpluslog.K42.F)


#---------------------
#---------------------
#---------------------
#---------------------
#---------------------
#---------------------
##### PHYTO Transformation

# Hellinger transformation on phytoplankton species composition

# Annual phyto

# C1
phyto.C1.2p.hel <- decostand(phyto.C1.2p, "hellinger")
phyto.C1.1p.hel <- decostand(phyto.C1.1p, "hellinger")

# C6
phyto.C6.2p.hel <- decostand(phyto.C6.2p, "hellinger")
phyto.C6.1p.hel <- decostand(phyto.C6.1p, "hellinger")

# C9
phyto.C9.2p.hel <- decostand(phyto.C9.2p, "hellinger")
phyto.C9.1p.hel <- decostand(phyto.C9.1p, "hellinger")

# E51
phyto.E51.2p.hel <- decostand(phyto.E51.2p, "hellinger")
phyto.E51.1p.hel <- decostand(phyto.E51.1p, "hellinger")

# K39
phyto.K39.2p.hel <- decostand(phyto.K39.2p, "hellinger")
phyto.K39.1p.hel <- decostand(phyto.K39.1p, "hellinger")

# K42
phyto.K42.2008.2p.hel <- decostand(phyto.K42.2008.2p, "hellinger")
phyto.K42.2008.1p.hel <- decostand(phyto.K42.2008.1p, "hellinger")

# K42 phyto.K42.2012
phyto.K42.2012.2p.hel <- decostand(phyto.K42.2012.2p, "hellinger")
phyto.K42.2012.1p.hel <- decostand(phyto.K42.2012.1p, "hellinger")

# K45
phyto.K45.2p.hel <- decostand(phyto.K45.2p, "hellinger")
phyto.K45.1p.hel <- decostand(phyto.K45.1p, "hellinger")

# S15
phyto.S15.2p.hel <- decostand(phyto.S15.2p, "hellinger")
phyto.S15.1p.hel <- decostand(phyto.S15.1p, "hellinger")

# annual
phyto.annual.all.2p.hel <- decostand(phyto.annual.all.2p, "hellinger")
phyto.annual.all.1p.hel <- decostand(phyto.annual.all.1p, "hellinger")

#------------------------------------
# Seasonal phyto
# C1
phyto.C1.S.F.2p.hel <- decostand(phyto.C1.S.F.2p, "hellinger")
phyto.C1.S.F.1p.hel <- decostand(phyto.C1.S.F.1p, "hellinger")
phyto.C1.S.hel <- decostand(phyto.C1.S, "hellinger")
phyto.C1.F.hel <- decostand(phyto.C1.F, "hellinger")

# E51
phyto.E51.S.F.2p.hel <- decostand(phyto.E51.S.F.2p, "hellinger")
phyto.E51.S.F.1p.hel <- decostand(phyto.E51.S.F.1p, "hellinger")
phyto.E51.S.hel <- decostand(phyto.E51.S, "hellinger")
phyto.E51.F.hel <- decostand(phyto.E51.F, "hellinger")

# K42 phyto.K42.2012
phyto.K42.S.F.2p.hel <- decostand(phyto.K42.S.F.2p, "hellinger")
phyto.K42.S.F.1p.hel <- decostand(phyto.K42.S.F.1p, "hellinger")
phyto.K42.S.hel <- decostand(phyto.K42.S, "hellinger")
phyto.K42.F.hel <- decostand(phyto.K42.F, "hellinger")

# annual
phyto.seasonal.all.2p.hel <- decostand(phyto.seasonal.all.2p, "hellinger")
phyto.seasonal.all.1p.hel <- decostand(phyto.seasonal.all.1p, "hellinger")

