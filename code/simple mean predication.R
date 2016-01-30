setwd("I:/R Data/spatial 2014")
data = read.csv("SMUD_2012.csv",header = F)
data = readRDS("ratio.rds")
# no Dec
data[nrow(data),]

# get Nov data
Nov = data[439201:482400,]

# how many stations
ncol(Nov)-6

# remove time
Nov1 = Nov[,-c(1:6)]

# find NA
na = lapply(1:66, function(x) sum(is.na(Nov[,x])))

# simple mean prediction
mean = lapply(1:66, function(x) rowMeans(Nov[,-x], na.rm = T) ) # change it to distance 
pred_mean = do.call(cbind, mean)

# MSE
# Becasue NA, so need to find each difference and then unlist to find mean
MSE = numeric()
for (i in 1:66)
{
  mse = lapply(1:43200, function(x) (Nov[x,i]- pred_mean[x,i])^2  )
  MSE[i] = mean(unlist(mse),na.rm = T)

}

MSE