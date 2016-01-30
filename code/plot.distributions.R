

setwd("I:/R Data/spatial 2014")

max.lag.corr = readRDS("maxlag.rds")
distance = readRDS("dist.rds")


library(sp)
sst.ll = cbind(distance,0)
sst.ll = SpatialPoints(sst.ll)

mx = max(max.lag.corr, na.rm = T)
time = seq(-mx,mx, by = 1)
values = matrix(nrow=length(time), ncol = ncol(max.lag.corr))
for(j in 1:ncol(max.lag.corr))
{
  for(i in 1:length(time))
    values[i,j] = log(sum(max.lag.corr[,j] == time[i],na.rm=T)+1)
  
#   for(i in 1:length(time))
#     values[i,j] = sum(max.lag.corr[,j] == time[i],na.rm=T)/sum(!is.na(max.lag.corr[,j]),na.rm=T)
  
}


time = 1:(2*mx+1)
sst.Date = as.POSIXct(strptime(paste(trunc(time/60), time%%60, sep=" "),"%H %M"), format ="%H:%M" )


# spacetime:
library(spacetime)
library(xts)
library(fields)#for tim.colors()
sst.st = STFDF(sst.ll, sst.Date, data.frame(sst = as.vector(t(values))))
# doy.t = formatC(doy, width = 3, format = "d", flag = "0") 
plot.save = paste(plot.dir, "distribution_", interval, "min.png", sep="") 
png(plot.save, width = 600, height = 700)
print(stplot(sst.st, 
             yaxt = "n",
             mode = "xt",#xt tp ts
             col.regions=tim.colors(), at = c(0,seq(.6,5,length=62), max(values)), #cuts = 64,
             main=" ",
             xlab="Distance", ylab = "Time",
             scales=list(y=list(at = sst.Date[seq(1,length(sst.Date), by=2)] , labels=seq(-mx,mx, by = 2)),
                         x=list(at = seq(0, max(distance), by=400))),
             scaleX=1))
dev.off()
