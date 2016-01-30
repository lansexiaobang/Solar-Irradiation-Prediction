setwd("I:/R Data/spatial 2014/Guoxin Project")

# fit data
fit = readRDS("csm_sens1.rds")
for (i in 2:50)  
{
  x = paste0("csm_sens",i,".rds")
  a = readRDS(x)
  fit = cbind(fit,a)
}
saveRDS(fit, file = "detrend")
fit = readRDS("detrend.rds")

# read real data
setwd("I:/R Data/spatial 2014")
real = read.csv("SMUD_2012.csv", header = FALSE)

### detrend by ratio
ratio = real

for(t in 1:50)
{
  ratio[,t+6] = real[,t+6]/fit[,t]
}

saveRDS(ratio, file = "ratio 50")

########################### find max lag
dat = readRDS("ratio 50.rds")
dat = ratio

# find all pairs
x = combn(50,2)

# loop for month
for (mon in 1:12)
{
  datam = dat[dat[,2]==mon,]
  
  # m is the last day of the month
  m = max(datam[,3])
  
  # loop for day
  for( doy in 1:m)
  {
    # one day data for all station
    data = datam[datam[,3]==doy,]
  
    # find lag between each station
    y = numeric()
      for (i in 1:ncol(x))
      {
        sens1 = x[1,i]
        sens2 = x[2,i]
        
        # only get value greater than 10
        ind = apply(data[,c(sens1+6, sens2+6)], 1, function(x) !any(x >= 10))
        as.numeric(ind)
        
        # remove NA
        if(sum(ind %in% NA)>=400)
        {
          y[i] = NA
      
        } else{
      
        irrad1 = data[ind,sens1+6]
        irrad2 = data[ind,sens2+6]
        
        # calculate correlation
        cor = ccf(irrad1, irrad2,lag.max =30, na.action = na.pass)
        
        # find the max lag
        y[i] = cor$lag[which(abs(cor$acf)==  max(abs(cor$acf)))]
      }
    
    } 
  
    lag = y
    x = rbind(x,lag)
  }
}

xnew = x[-c(1:2),]
row.names(xnew) = c(1:335)
saveRDS(xnew, file = "timelag")


### get lat and long
library(geosphere)
setwd("I:/R Data/spatial 2014")
dis = read.csv("SMUD_latlong.csv")

# calculate distance
dist = numeric()

for (i in 1:1225)
{
  sens1 = x[1,i]
  sens2 = x[2,i]
  
  
  Lo1 = dis[sens1,2]
  La1 = dis[sens1,1]
  
  Lo2 = dis[sens2,2]
  La2 = dis[sens2,1]
  
  dist[i] = distCosine(c(Lo1, La1),c(Lo2, La2),r=6378137)
}

saveRDS(dist, file="distnew.rds")

# load data
max.lag.corr = readRDS("timelag.rds")
distance = readRDS("distnew.rds")

# Hovmoller diagram
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
png(plot.save, width = 600, height = 700)
print(stplot(sst.st, 
             yaxt = "n",
             mode = "xt",#xt tp ts
             col.regions=tim.colors(), at = c(0,seq(.6,5,length=62), max(values)), #cuts = 64,
             main=" ",
             xlab="Distance", ylab = "Time",
             scales=list(y=list(at = sst.Date[seq(1,length(sst.Date), by=2)] , labels=seq(-mx,mx, by = 2)),
                         x=list(at = seq(0, max(distance), by=1000))),
             scaleX=1))



# L- method
sd= apply(max.lag.corr, 2, function(x) sd(x, na.rm=T))
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
rmse = rmse[rmse>0]
i = which.min(rmse)+1
temp.d[i]

x = temp.d[1:i]
y = temp.sd[1:i]
fit.l = lm(y~x)

x = temp.d[(i+1):b]
y = temp.sd[(i+1):b]
fit.r = lm(y~x)


plot.save = paste(plot.dir, "Lmethod1_", interval, "min.png", sep="") 
png(plot.save, width = 700, height = 600)
plot(distance,sd, ylab="std dev", xlab = "distance (in km)",col="darkgrey")
lines(temp.d[1:i],fit.l$fitted.values,lwd = 2)
lines(temp.d[(i+1):b],fit.r$fitted.values,lwd=2)
abline(v=temp.d[i], col="red", lty=2)


