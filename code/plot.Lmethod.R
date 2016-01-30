setwd("I:/R Data/spatial 2014")

sd= readRDS("sts.rds")
distance = readRDS("distances.rds")
temp.d = sort(distance)
temp.sd = sd[order(distance)]

# temp.sd = temp.sd[-1]
# temp.d = temp.d[-1]

b = length(temp.sd)
rmse = numeric(b)
for(i in 2:(b-2))
{
  x = temp.d[1:i]
  y = temp.sd[1:i]
  fit.l = sqrt(mean(lm(y~x)$res^2))
  
  x = temp.d[(i+1):b]
  y = temp.sd[(i+1):b]
  fit.r = sqrt(mean(lm(y~x)$res^2))
  
  rmse[i] = i/b*fit.l + (b-i)/b *fit.r
}
rmes = rmse[!is.na(temp.sd)]
 rmse = rmse[rmse>0]
temp.d = temp.d[!is.na(temp.d)]
temp.sd = temp.sd[!is.na(temp.sd)]
i = which.min(rmse)+1
temp.d[i]

x = temp.d[1:i]
y = temp.sd[1:i]
fit.l = lm(y~x)

x = temp.d[(i+1):b]
y = temp.sd[(i+1):b]
fit.r = lm(y~x)

b = length(temp.sd)

plot.save = paste(plot.dir, "Lmethod1_", interval, "min.png", sep="") 
png(plot.save, width = 700, height = 600)
plot(distance, sd, ylab="std dev", xlab = "distance (in km)",col="darkgrey")
lines(temp.d[1:i],fit.l$fitted.values,lwd = 2)
lines(temp.d[(i+1):b],fit.r$fitted.values,lwd=2)
abline(v=temp.d[i], col="red", lty=2)
dev.off()


library(gstat)
show.vgms()









