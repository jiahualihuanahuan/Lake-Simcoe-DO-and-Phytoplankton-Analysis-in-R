# Plot FQI over year

plot(phyto.FQI[,1], phyto.FQI[,2], xlab="Year", ylab="FQI",ylim=c(0.4,0.82))
points(phyto.FQI[,1], phyto.FQI[,3], pch=2)
points(phyto.FQI[,1], phyto.FQI[,4], pch=3)
points(phyto.FQI[,1], phyto.FQI[,5], pch=4)
points(phyto.FQI[,1], phyto.FQI[,6], pch=5)
points(phyto.FQI[,1], phyto.FQI[,7], pch=6)
points(phyto.FQI[,1], phyto.FQI[,8], pch=7)
points(phyto.FQI[,1], phyto.FQI[,9], pch=8)

legend ("topleft", c("C1", "C6", "C9", "E51", "K39", "K42", "K45", "S15"), pch=c(1,2,3,4,5,6,7,8))

library(Kendall)
Kendall(phyto.FQI[,2],phyto.FQI[,1])
Kendall(phyto.FQI[,3],phyto.FQI[,1])
Kendall(phyto.FQI[,4],phyto.FQI[,1])
Kendall(phyto.FQI[,5],phyto.FQI[,1])
Kendall(phyto.FQI[,6],phyto.FQI[,1])
Kendall(phyto.FQI[,7],phyto.FQI[,1])
Kendall(phyto.FQI[,8],phyto.FQI[,1])
Kendall(phyto.FQI[,9],phyto.FQI[,1])
