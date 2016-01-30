
setwd("I:/R Data/spatial 2014")
library(gstat)
library(sp)
data(meuse)
dist = readRDS("DistWithAngle.rds")
c = readRDS("vgmdata.rds")

# function for getting fited_value
fitted = function(start,end,x,m,y,z)
{

 dist1 = dist[dist[,2]>=start & dist[,2]<=end,]
 #dist1 = dist[dist[,2]>=start | dist[,2]<=end,]  # only for 337.5 to 22.5 only
 data = dist1
 
 d = c[1:nrow(data),]
 d[,2] = data[,1]
 d[,3] = data[,3]
 d = na.omit(d)
 d[,1] = rep(1,nrow(d))
 a = fit.variogram(d, vgm(x,m,y,z))
 
 fitted_value = variogramLine(a, maxdist= max(d[,2]), dist_vector = d[,2])
 return(fitted_value)
}

## fittedvalue by angle
p1 = fitted(22.5,67.5,4,m ="Cir" ,38000,15)
p2 = fitted(67.5,112.5,5,m ="Cir" ,25000,15)
p3 = fitted(112.5,157.5,4,m ="Cir" ,30000,15)
p4 = fitted(157.5,202.5,5,m ="Cir" ,40000,15)
p5 = fitted(202.5,247.5,5,m ="Cir" ,38000,15)
p6 = fitted(247.5,292.5,5,m ="Cir" ,25000,15)
p7 = fitted(292.5,377.5,4,m ="Cir" ,38000,15)
p8 = fitted(337.5,22.5,5,m ="Cir" ,38000,15)

# get irrad data
irrad = readRDS("ratio.rds")

# select Nov 
Nov = irrad[439201:482400,]

# read lat and long data
lalo = read.csv("SMUD_latlong.csv",header = F)

# distance function package
library(geosphere)
library(SDMTools)

# calcualte distance matrix
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

# calcualte angle matrix
angle = matrix(nrow = 66, ncol = 66)
for(i in 1:66)                                           
{
 
  angle[,i] = sapply(1:66, function(x){
                
                  Lo1 = lalo[i,2]
                  La1 = lalo[i,1]
  
                  Lo2 = lalo[x,2]
                  La2 = lalo[x,1]
  
                  distance(La1, Lo1 , La2, Lo2 , bearing =TRUE )$bearing
                 })
}  


# calculate weight matrix one by one will get 8 weigh martix
m = matrix(0,nrow = 66, ncol = 66)
weight = list()
for(j in 1:8){
  for(i in 1:66)          
{
  o = get(paste0("p",j))
  temp = lapply(1:66, function(x)  o[o$dist == dist[x,i],2])
  temp = sapply(temp,function(x) {if (length(x)>0) x[1]})
  for (q in 1:66)
  {
    if (length(temp[[q]]) == 0)
    temp[[q]] = 0
  }
  m[,i] = unlist(temp) 
  }
    weight[[j]] = m
}

# combine 8 weight matrix together
result = matrix(0,nrow = 66, ncol = 66)
for(j in 1:8){
  result = weight[[j]] + result
}

# invert 
result = -result + 100

# stadardize weight
for(i in 1:ncol(result)){
  result[,i] = result[,i]/sum(result[,i])
}

sd_weight1 = result

# use weight start to prediction
pred2 = matrix(nrow = 43200, ncol = 66)
for(i in 1:66)
{
pred = lapply(1:66, function(x) Nov[,x]*sd_weight1[x,i])
pred1 = do.call(cbind,pred)
pred2[,i] = rowSums(pred1, na.rm = T) 

}

#get MSE
MSE = numeric()
for (i in 1:66)
{
  mse = lapply(1:43200, function(x) (Nov[x,i]- pred2[x,i])^2  )
  MSE[i] = mean(unlist(mse),na.rm = T)

}
 
MSE
 
#compare MSE
# simple mean
SMSE = readLines("simple prediction MSE.txt")
SMSE1 = strsplit(SMSE, "   ")
SMSE = as.numeric(unlist(SMSE1))

# direction only
dMSE = readLines("direction prediction MSE.txt")
dMSE = strsplit(dMSE, "   ")
dMSE = as.numeric(unlist(dMSE))

(SMSE-MSE) > 0
