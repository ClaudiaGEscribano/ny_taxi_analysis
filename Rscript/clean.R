library("dplyr")
library("ggplot2")
library("latticeExtra")
library('gridExtra')

 
read <-  function(month) {
 
    data <-  read.csv(paste("https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2017-", month, ".csv", sep=""))
 
    ## remove data < 0 in variables that should be positive:

    sampleSize  <- floor(0.005*nrow(data))

    set.seed(123)

    sampleID  <- sample(seq_len(nrow(data)), size=sampleSize)
    sampleData  <- data[sampleID,]

    print(nrow(sampleData))
    out  <- sampleData

    out
}

cleanBasic  <- function(x) {
    
    df <- x %>%
        filter(fare_amount > 0) %>%
        filter(mta_tax > 0) %>%
        filter(extra >= 0) %>%
        filter(tip_amount > 0) %>%
        filter(tolls_amount >= 0) %>%
        filter(improvement_surcharge >= 0) %>%
        filter(total_amount > 0)

    ## include a month column in this step

    ##df$Month <- month

    out <- df
}
 
code <-  function(x) {
 
    df <- x %>%
            filter(RatecodeID <= 6)

    out <-  df
    out
}

extra <-  function(x) {

    df <- x %>%
        filter(extra == 0.50 | extra == 1)

    out <-  df
    out
}

tips <-  function(x) {

    df <- x %>%
        filter(tip_amount < total_amount)

    out <- df
    out
}

##! eliminar las distancias == 0
 
zero <-  function(x) {

    df <- x %>%
        filter(trip_distance > 0.5)

    out <- df
    out
}
