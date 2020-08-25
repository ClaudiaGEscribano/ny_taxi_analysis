## Download and prepare data for the analysis.

library("dplyr")
library("ggplot2")
library("latticeExtra")
library('gridExtra')
library('sf')
  
source('clean.R')

months <- c("03","06","11") ## months to be read from the website for 2017

data <- lapply(months, FUN=function(x) read(x))
data <- do.call(rbind, data)

data  <- cleanBasic(data)
 
##  Ratecode ID goes from 1 to 6, but there are different values that should be removed.
 
data <-  code(data)

## extra charges are 0.5 or 1 $

## the amount of extra charges 4.5$ shows a pattern where, the fare_amount is equal in every case, independently of the distance. Maybe they should be removed.

data <-  extra2(data)

## tips are included in the total amount, so they should not be larger than this variable.

data <-  tips(data)

## Remove the distances smaller than 0.5

data <-  zero(data)

## passengers more than 0 and below 6

data  <- data[data$passenger_count <= 6,]
data  <- data[data$passenger_count != 0,]

## improvement surchage only can be 0.3
data  <- data[data$improvement_surcharge == 0.3,]

write.csv(data, "data/taxi_sample_data.csv", row.names=FALSE)
#########################################################################3

## TAXI ZONES DATA:

## Look up to the zones maps

zones <-  read.csv("https://s3.amazonaws.com/nyc-tlc/misc/taxi+_zone_lookup.csv")

## save this as .csv

write.csv(zones, "data/taxi_zones.csv", row.names=FALSE)

## SHAPE FILE OF TLC TAXI

## load the shape file data
 
spZones <- st_read("data/taxi_zones.shp")

## Vizualize the TLC zones:

plot(spZones["zone"])

library(ggplot2)
borough <-  ggplot() + geom_sf(data=spZones, aes(fill = borough))

## merge with the .csv with service zone

a <- merge(spZones, zones[,c(1,4)], by='LocationID')
service <-  ggplot() + geom_sf(data=a, aes(fill = service_zone))









