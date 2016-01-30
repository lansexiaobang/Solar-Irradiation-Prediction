setwd("I:/R Data/spatial 2014")
library(gstat)
library(sp)
data(meuse)

# get the data set from vgm
coordinates(meuse) = ~x+y

plot(variogram(log(zinc)~x+y, meuse))
vgm1 <- variogram(log(zinc)~1, meuse)
b = vgm1
c = vgm1
# c need to match the rows data we have
for (i in 1:150)
{

    c = rbind(c,b)
    i = i+1
}
c = c[1:2145,]

# read data
sd= readRDS("sts.rds")
distance = readRDS("DistWithAngle.rds")

# fit the data by vgm return fitted value with distance
fit = function(x,m,y,z)
{
  d = c[1:nrow(data),]
  d[,2] = data[,1]
  d[,3] = data[,2]
  d = na.omit(d)
  d[,1] = rep(1,nrow(d))
  a = fit.variogram(d, vgm(x,m,y,z))
  plot(d, model = a)

  fitted_value = variogramLine(a, maxdist= max(d[,2]), dist_vector = d[,2])
  return(fitted_value)
}

# get the fitted value
data = cbind(distance[,1],sd)
fitted_value = fit(5,m = "Pen",38000,16)

# sort by distance
fitted_value = fitted_value[order(fitted_value[,1]),]
 


########################## start predict
# read data
irrad = readRDS("ratio.rds")
lalo = read.csv("SMUD_latlong.csv",header = F)

# get Nov data
Nov = irrad[439201:482400,]

#  prediction weight by distance
library(geosphere)
library(SDMTools)

# find the distance between one station to all others
dist = matrix(nrow = 66, ncol = 66)
for(i in 1:66)                                            # column 1 means is the distance between station 1 and others
{
 

  dist[,i] = sapply(1:66, function(x){
                
                  Lo1 = lalo[i,2]
                  La1 = lalo[i,1]
  
                  Lo2 = lalo[x,2]
                  La2 = lalo[x,1]
  
                  distance(La1, Lo1 , La2, Lo2 , bearing =TRUE )$distance
                 })
}      

# bulid weight matrix, which is replace the distance I get from above to weight from fitted_value
weight = matrix(nrow = 66, ncol = 66)

for(i in 1:66)          # column 1 means is the weight between station 1 and others
{
  temp = lapply(1:66, function(x)  fitted_value[fitted_value$dist == dist[x,i],2])
  temp = sapply(temp,function(x) {if (length(x)>0) x[1]})
  for (q in 1:66)
  {
    if (length(temp[[q]]) == 0)
    temp[[q]] = 0
  }
  weight[,i] = unlist(temp)
}


# since weight matrix is symmetric, it doesn't matter I use col or row
# stadardize weight
sd_weight = lapply(1:ncol(weight1), function(x) weight1[,x]/sum(weight1[,x]))
sd_weight1 = do.call(rbind, sd_weight)

# use weight start to prediction
pred2 = matrix(nrow = 43200, ncol = 66)

for(i in 1:66)
{
pred = lapply(1:66, function(x) Nov[,x]*sd_weight1[x,i])
pred1 = do.call(cbind,pred)
pred2[,i] = rowSums(pred1, na.rm = T) 

}

# get MSE
MSE = numeric()
for (i in 1:66)
{
  mse = lapply(1:43200, function(x) (Nov[x,i]- pred2[x,i])^2  )
  MSE[i] = mean(unlist(mse),na.rm = T)

}
 
MSE

# compare MSE
SMSE = readLines("prediction MSE.txt")
SMSE1 = strsplit(SMSE, "   ")
SMSE = as.numeric(unlist(SMSE1))
 
(SMSE-MSE) <0
