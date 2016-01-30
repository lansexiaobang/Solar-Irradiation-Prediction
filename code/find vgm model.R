# load package and data
library(geosphere)
library(SDMTools)
setwd("I:/R Data/spatial 2014")
sd= readRDS("sts.rds")
dis = read.csv("SMUD_latlong.csv",header = F)

# calculate distance and angle
dist = matrix(nrow = 2145,ncol = 2)
x = combn(66,2)
for( i in 1:2145)
{
  
  sens1 = x[1,i]
  sens2 = x[2,i]
  
  
  Lo1 = dis[sens2,2]
  La1 = dis[sens2,1]
  
  Lo2 = dis[sens1,2]
  La2 = dis[sens1,1]
  
  dist[i,1] = distance(La1, Lo1 , La2, Lo2 , bearing =TRUE )$distance
  dist[i,2] = distance(La1, Lo1 , La2, Lo2 , bearing =TRUE )$bearing
}

# save into a data frame
dist = data.frame(dist)
dist$sd = sd
colnames(dist) = c("distance", "angle", "sd")
dist = rbind(dist,dist1)
saveRDS(dist, file = "DistWithAngle.rds")

###############################################################
dist = readRDS("DistWithAngle.rds")

# average function, average distance, instead of using all data
ave = function(dist, x)
{
  distcut = cut(dist[,1], seq(min(dist[,1]), max(dist[,1]), by=x))
  
  splitsd = split(dist[,3],distcut)
  
  mean = lapply(splitsd, mean)
  
  mean = as.numeric(unlist(mean))
  dist = seq(min(dist[,1]), max(dist[,1]), by=x)
  dist = dist[1:length(mean)]
  
  data = cbind(dist,mean)
  
  return(data)
}

# get plot with fitted model, and calculate MSE
plotandMES = function(start,end,x,m,y,z)
{

 #dist1 = dist[dist[,2]>=start & dist[,2]<=end,]
 dist1 = dist[dist[,2]>=start | dist[,2]<=end,]
 data = ave(dist1,500)
 
 d = c[1:nrow(data),]
 d[,2] = data[,1]
 d[,3] = data[,2]
 d = na.omit(d)
 d[,1] = rep(1,nrow(d))
 a = fit.variogram(d, vgm(x,m,y,z))
 
 fitted_value = variogramLine(a, maxdist= max(d[,2]), dist_vector = d[,2])
 MSE = sqrt(mean((d[,3]-fitted_value[,2])^2))
  x = paste0("Angles between ",start," and ",end)
 p = plot(d, model = a,ylab = "variance", main = x, cex.lab = 1.5)
 print(p)
 #return(list(p,MSE))
#return(MSE)
}

############################### model selection
a = function(x,y,z,a,b,c)
{
  for (i in 1:7)
  {
    test =  plotandMES (x,y,a,m = z[i],b,c)
    mse = data.frame(test,z[i])
    names(mse) = c("MSE", "Model")
    MSE = rbind(MSE,mse)
  }
  return(MSE)
}

# model name
z = c("Cir", "Exp", "Pen", "Sph","Mat", "Ste", "Log")
test =  plotandMES (22.5,67.5,4,m = "Cir",38000,16)
MSE = data.frame(test,"Cir")
names(MSE) = c("MSE", "Model")
MSE = a(22.5,67.5,z,4,38000,16)
MSE = a(67.5,112.5,z,5,25000,15)
MSE = a(112.5,157.5,z,4,30000,15)
MSE = a(157.5,202.5,z,5,40000,16)
MSE = a(202.5,247.5,z,5,38000,15)
MSE = a(247.5,292.5,z,5,25000,15)
MSE = a(292.5,377.5,z,4,38000,15)
MSE = a(337.5,22.5,z,5,38000,15)
MSE = MSE[-1,]
MSE$angle = rep(1:8, each = 7)
sub = split(MSE, MSE$angle)

#selecte model by min MSE
min = lapply(1:length(sub), function(x) sub[[x]][order(sub[[x]][,1])[1],])


plotandMES(22.5,67.5,4,m ="Cir" ,38000,15)
plotandMES(67.5,112.5,5,m ="Cir" ,25000,15)
plotandMES(112.5,157.5,4,m ="Cir" ,30000,15)
plotandMES(157.5,202.5,5,m ="Cir" ,40000,15)
plotandMES(202.5,247.5,5,m ="Cir" ,38000,15)
plotandMES(247.5,292.5,5,m ="Cir" ,25000,15)
plotandMES(292.5,377.5,4,m ="Cir" ,38000,15)
plotandMES(337.5,22.5,5,m ="Cir" ,38000,15)


