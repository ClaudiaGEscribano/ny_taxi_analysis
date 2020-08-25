## Prepare the data for the model:

## 1. Download the data
## 2. Clean the sample
## 3. Prepare the train and test.


library("dplyr")
library("lubridate")
zones <-  read.csv("taxi_zones.csv")

read <-  function(month) {
 
    data <-  read.csv(paste("https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2017-", month, ".csv", sep=""))
 
    sampleSize  <- floor(0.02*nrow(data))

    set.seed(123)

    sampleID  <- sample(seq_len(nrow(data)), size=sampleSize)
    sampleData  <- data[sampleID,]

    print(nrow(sampleData))
    out  <- sampleData

    out
}

months <- c("03","06","11") ## months to be read from the website for 2017

data <- lapply(months, FUN=function(x) read(x))
data <- do.call(rbind, data)


source('clean.R')

data  <- cleanBasic(data)
data <-  code(data)
data <-  extra(data)
data <-  tips(data)
data <-  zero(data)

data  <- data[data$passenger_count <= 6,]
data  <- data[data$passenger_count != 0,]
data  <- data[data$improvement_surcharge == 0.3,]

## fare and tip amount:

data  <- data[data$fare_amount > data$tip_amount,]
data  <-  data[data$tip_amount > 0.1*data$fare_amount,]
data  <-  data[data$tip_amount < 0.3*data$fare_amount,] 


write.csv(data, "taxi_model_sample_data.csv", row.names=FALSE)

## Create the trip time length function:
#aqui

data$tpep_pickup_datetime <- as.POSIXct(as.character(data$tpep_pickup_datetime), tz = "GMT", format="%Y-%m-%d %H:%M:%S")
data$tpep_dropoff_datetime <- as.POSIXct(as.character(data$tpep_dropoff_datetime), tz = "GMT",format="%Y-%m-%d %H:%M:%S")

## transform to CEST

data$tpep_pickup_datetime <- with_tz(data$tpep_pickup_datetime, tzone = "America/New_York")
data$tpep_dropoff_datetime <- with_tz(data$tpep_dropoff_datetime, tzone = "America/New_York")

## new column with the differences:

## it takes some minutes

data$trip_timelength  <- apply(data, 1, FUN=function(x) difftime(x[3], x[2]))


## The data have negative values that should be removed

data  <- data[data[,"trip_timelength" ] > 5,]

## location data !problema aqui

data <- data[data$PULocationID <= 263,]
data <- data[data$DOLocationID <= 263,]

zones <- zones[1:263,]
 
 
## create the intratrip variable and include them:

data$PU_boroughzone <-  data$PULocationID
data$DO_boroughzone <-  data$DOLocationID

## replace the ID code with the zone in the zone dataset

repl <-  function(x) {
    for (i in 1:263) {
        x[which(x["PU_boroughzone"] == i), "PU_boroughzone"]  <-  as.character(zones[i,"Borough"])
        x[which(x["DO_boroughzone"] == i), "DO_boroughzone"]  <-  as.character(zones[i,"Borough"])
    }
    x }
 
data <-  repl(data)
  
data$PU_boroughzone <-  factor(data$PU_boroughzone, levels=c("Bronx", "Brooklyn", "EWR", "Manhattan", "Queens", "Staten Island"))
data$DO_boroughzone <-  factor(data$DO_boroughzone, levels=c("Bronx", "Brooklyn", "EWR", "Manhattan", "Queens", "Staten Island"))
 

## the intratrip variable:

data$intratrip <- seq(1:nrow(data)) 
data[which(data$PU_boroughzone != data$DO_boroughzone), "intratrip"] <- "yes"
data[which(data$PU_boroughzone == data$DO_boroughzone), "intratrip"] <- "no"

## the day of the week variable

data$weekday  <-  weekdays(data$tpep_dropoff_datetime)

## leave only the variables for the moedel

data  <- data[,c(5,11,14,17,18,21,22)]
 
write.csv(data, "data/taxi_model_sample_data.csv", row.names=FALSE)

#########################
## sample the data for modelling:

# Random sample indexes
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- data[train_index, -3]
y_train <- data[train_index, "tip_amount"]

X_test <- data[test_index, -3]
y_test <- data[test_index, "tip_amount"]

## save these datasets:

write.csv(X_train, "data/xtrain.csv", row.names=FALSE)
write.csv(y_train, "data/ytrain.csv",row.names=FALSE)
write.csv(X_test, "data/xtest.csv",row.names=FALSE)
write.csv(y_test, "data/ytest.csv",row.names=FALSE)

##############################

