## Once the data has been downloaded and cleaned, the .csv is explore to understand the data
 
library("dplyr")
library("ggplot2")
library("latticeExtra")
library('gridExtra')
library('rasterVis')
library('sf')
library(reshape2)

## 1. Read the taxi data

data <-  read.csv("data/taxi_sample_data.csv")

## 2. Load the taxi zone data

zones <-  read.csv("data/taxi_zones.csv")

## 3. plot the area of the analysis

spZones <- st_read("data/taxi_zones.shp")

## the TLC zones:

plot(spZones["zone"])

## From the shp file, we have also information about boroughs:

borough <-  ggplot() + geom_sf(data=spZones, aes(fill = borough))
borough

## There is also information about the kind of service area in the .csv
a <- merge(spZones, zones[,c(1,4)], by='LocationID')
service <-  ggplot() + geom_sf(data=a, aes(fill = service_zone))
service

#################################################

## 4. Make a summary of the dataset for a first analysis.

summary(data)

## histogramas:

df  <-  data[, c(5,11,14,15,17)]
df  <-  melt(df)

myTheme  <- custom.theme(pch=20, cex=0.7)
myTheme$strip.background$col  <- "white"

histogram(~value|variable, data=df, par.settings=myTheme)
bwplot(~value|variable, data=df, par.settings=myTheme)
    
histogram(data$trip_distance)
histogram(data$fare_amount)
histogram(data$tolls_amount)
histogram(data$total_amount)
histogram(data$tip_amount)

## every variable seems to be skewed. Remove the outliers:

## First, calculate the quantiles and the IQR in the numeric variables:

quant  <-  function(x) {
    q  <-  quantile(x, probs=c(.25 ,.75))
    iqr  <- IQR(x)
    
    up  <-  q[2] + 1.5 *iqr
    down  <- q[1] - 1.5 *iqr

    c  <- c(up,down)
    c
}

## apply this function to the 5 relevant numeric variables:

var  <-  list("trip_distance","fare_amount","tip_amount","tolls_amount","total_amount")
q  <- lapply(var, FUN=function(x) quant(data[,x]))
names(q) <- var

## filter the dataset with these conditions:

subsetq <-  function(x) {
    for (i in 1:length(var)) {
        out  <- subset(x, x[var[[i]]] < (q[[i]][1]) & x[var[[i]]] > (q[[i]][2]))
        
    }
    out
}

data  <- subsetq(data)
 
df  <-  data[, c(5,11,14,15,17)]
df  <-  melt(df)
 
histogram(~value|variable, data=df, par.settings=myTheme)
bwplot(~value|variable, data=df, par.settings=myTheme)

## The toll doesn't seem to be very relevant to the tip amount, due to the fact that 80 percent of the data have similar toll values.

## Some records have lower fares than tips. Aditionally, the tip should be at least 10% of the fare. We also consider only tips below 60% of the fare.

## Remove records with fares lower than tips and tips lower than 10%:

data  <- data[data$fare_amount > data$tip_amount,]
data  <-  data[data$tip_amount > 0.1*data$fare_amount,]
data  <-  data[data$tip_amount < 0.3*data$fare_amount,] 

df  <-  data[, c(11,14,17,18)]
df  <-  melt(df)
  
histogram(~value|variable, data=df, par.settings=myTheme)
histogram(data$fare_amount)

## Another numeric variable that could be relevant is the time that each trip last. Calculate this variable from the pick up and drop off time.

##  Time stamp are loaded as factor. Translate into daytime:

class(data$tpep_pickup_datetime)

## It is not clear if the time zone is UTC or UTC-5
data$tpep_pickup_datetime <- as.POSIXct(as.character(data$tpep_pickup_datetime), tz='GMT',format="%Y-%m-%d %H:%M:%S")
data$tpep_dropoff_datetime <- as.POSIXct(as.character(data$tpep_dropoff_datetime), tz='GMT', format="%Y-%m-%d %H:%M:%S")

## new column with the differences:

## it takes some minutes
data$trip_timelength  <- apply(data, 1, FUN=function(x) difftime(x[3], x[2]))

df  <-  data[, c(5,11,14,15,17,18)]
df  <-  melt(df)

histogram(~value|variable, data=df, par.settings=myTheme) ## shows negatime trip time_length values. Remove them.

## remove negative time length

data  <- data[data[,"trip_timelength" ] > 5,]
df  <-  data[, c(5,11,14,15,17,18)]
df  <-  melt(df)
 
histogram(~value|variable, data=df, par.settings=myTheme)

histogram(data$trip_timelength)

## have a look on the numeric variables

splom(data[,c(5,11,14,17,18)], par.settings=myTheme, auto.key=TRUE, data=data)

## Create new columns with the time (hour) of the pick up and drop off.

data$t_pu  <- as.numeric(strftime(data$tpep_pickup_datetime, format="%H"))
data$t_do  <- as.numeric(strftime(data$tpep_dropoff_datetime, format="%H"))

df  <- data[, c("tip_amount","t_pu", "t_do")]
histogram(df$t_do)
histogram(df$t_pu)

xyplot(tip_amount~t_do, data=df, pch=20)

df <- df %>% 
group_by(t_pu) %>%
    summarise(m = median(tip_amount))%>%
    as.data.frame()
 
dotplot(m~as.factor(t_pu), data=df, pch=20, grid=TRUE, cex=2, ylab='median tip', xlab='time', par.settings=myTheme)
xyplot(m~t_pu, data=df, pch=20, type='l',lwd=3,grid=TRUE, cex=2, ylab='mean tip', par.settings=myTheme)


## 5. scatterplots with the 5 numeric relevant variables. Linear relationship ¿?

splom(data[,c(5,11,14,17,18)], par.settings=myTheme, auto.key=TRUE, data=data)

###########################################################################
##  categorical
#########################################################################

## payment type: only card

## RateCodeID

histogram(data$RateCodeID)
data  <-  data[data$RateCodeID ==1,]

## there is no 'Nassau ..." in the zones maps. Removing this data.

## PU and DO location ID:

## From the zones csv there is information about the zones  LocationID, Borough and service zone. I create 2 columns in my dataframe with the PU and DO service and borough zone.

## create 2 variables for the PU and DO service zone

data$PU_servicezone <-  data$PULocationID
data$DO_servicezone <-  data$DOLocationID

## replace the ID code with the zone in the zone dataset

repl <-  function(x) {
    for (i in 1:265) {
        x[which(x["PU_servicezone"] == i), "PU_servicezone"]  <-  as.character(zones[i,"service_zone"])
        x[which(x["DO_servicezone"] == i), "DO_servicezone"]  <-  as.character(zones[i,"service_zone"])
    }
    x }

data <-  repl(data)
   
data$PU_servicezone <-  factor(data$PU_servicezone, levels=c("Airports","Boro Zone", "EWR", "N/A","Yellow Zone"))
data$DO_servicezone <-  factor(data$DO_servicezone, levels=c("Airports","Boro Zone", "EWR", "N/A","Yellow Zone"))
 
## create 2 variables for the PU and DO borough
data$PU_boroughzone <-  data$PULocationID
data$DO_boroughzone <-  data$DOLocationID

## replace the ID code with the zone in the zone dataset

repl <-  function(x) {
    for (i in 1:265) {
        x[which(x["PU_boroughzone"] == i), "PU_boroughzone"]  <-  as.character(zones[i,"Borough"])
        x[which(x["DO_boroughzone"] == i), "DO_boroughzone"]  <-  as.character(zones[i,"Borough"])
    }
    x }
 
data <-  repl(data)
 
data$PU_boroughzone <-  factor(data$PU_boroughzone, levels=c("Bronx", "Brooklyn", "EWR", "Manhattan", "Queens", "Staten Island", "Unknown"))
data$DO_boroughzone <-  factor(data$DO_boroughzone, levels=c("Bronx", "Brooklyn", "EWR", "Manhattan", "Queens", "Staten Island", "Unknown"))

## plot some histograms

histogram(data$PU_boroughzone, par.settings=myTheme)
histogram(data$DO_boroughzone, par.settings=myTheme)

## The unknown correspond to a Location ID 264 and 265, which are not identified in the zones .csv

## We can see also with hiher saptial resolution:

histogram(data$PULocationID, par.settings=myTheme)
histogram(data$DOLocationID, par.settings=myTheme) ## in the pick up most are in one location.

## see it on a map:

PU <- data %>% 
    group_by(PU_boroughzone) %>%
    summarise(no_rows = length(PU_boroughzone))%>%
    as.data.frame()    
names(PU) <- c("borough","PU_ztimes")

c <- merge(spZones, PU, by='borough')
pu <-  ggplot() + geom_sf(data=c, aes(fill = PU_ztimes))+scale_fill_continuous(trans = "log10",type='viridis')

DO <- data %>% 
    group_by(DO_boroughzone) %>%
    summarise(no_rows = length(DO_boroughzone))%>%
    as.data.frame()    
names(DO) <- c("borough","DO_ztimes")

c <- merge(spZones, DO, by='borough')
do <-  ggplot() + geom_sf(data=c, aes(fill = DO_ztimes))+scale_fill_continuous(trans = "log10",type='viridis')

## Location ID


PU <- data %>% 
    group_by(PULocationID) %>%
    summarise(no_rows = length(PULocationID)) %>%
    as.data.frame()    
names(PU) <- c("LocationID","PU_ztimes")


b <- merge(spZones, PU, by='LocationID')

## logaritmic scale because there is a lot of difference between airports (maximum) and the rest
pu <-  ggplot() + geom_sf(data=b, aes(fill = PU_ztimes))+scale_fill_continuous(trans = "log10", type='viridis')

DO <- data %>% 
    group_by(DOLocationID) %>%
    summarise(no_rows = length(DOLocationID))%>%
    as.data.frame()    
names(DO) <- c("LocationID","DO_ztimes")

c <- merge(spZones, DO, by='LocationID')
do <-  ggplot() + geom_sf(data=c, aes(fill = DO_ztimes))+scale_fill_continuous(trans = "log10",type='viridis')
 
########################

## select the range for day-time and night time
 
## * hay que seleccionar bien las horas
data[which(data[ ,"t_pu"] >= 6 & data[ ,"t_pu"] <= 20), 't_pu']  <- "day"
data[which(data[ ,"t_pu"] != "day"), 't_pu']  <- "night" 

data[which(data[ ,"t_do"] >= 6 & data[ ,"t_do"] <= 20), 't_do']  <- "day"
data[which(data[ ,"t_do"] != "day"), 't_do']  <- "night" 

data$t_pu  <-  as.factor(data$t_pu)
data$t_do  <-  as.factor(data$t_do)
## plot this variables:

df  <-  data[, c("t_pu","t_do")]

summary(df)

histogram(data$t_pu, par.settings=myTheme)
histogram(data$t_do, par.settings=myTheme)

#############################################

## Numeric variables with categorical variables



## 4.1 boxplot for trip variables

## I will represent them depending on some categorical variables like: the pick up or drop off area or the rate code at the end of the trip.

## trip distance
 
library(reshape2)
library(RColorBrewer)
 
trip  <-  melt(data[,c(5,20,21)], 'trip_distance')
ggplot(trip, aes(x=variable,y=trip_distance)) +
    geom_boxplot(aes(fill=value)) + scale_fill_brewer(palette="Paired")
ggplot(trip, aes(x=value,y=trip_distance)) +
    geom_boxplot(aes(fill=variable)) + scale_fill_brewer(palette="Paired")


## fare amount

fare  <-  melt(data[,c(11,20,21)], 'fare_amount')
ggplot(fare, aes(x=variable,y=fare_amount, color=value)) +
    geom_boxplot()
ggplot(fare, aes(x=value,y=fare_amount, color=variable)) +
    geom_boxplot()

fare  <- data[,c(11,6)]
ggplot(fare, aes(x=as.factor(RatecodeID),y=fare_amount, color=as.factor(RatecodeID))) +
    geom_boxplot()

## tip amount

tip  <-  melt(data[,c(14,20,21)], 'tip_amount')
ggplot(tip, aes(x=variable,y=tip_amount, color=value)) +
    geom_boxplot()
ggplot(tip, aes(x=value,y=tip_amount, color=variable)) +
    geom_boxplot()

tip  <- data[,c(14,6)]
ggplot(tip, aes(x=as.factor(RatecodeID),y=tip_amount, color=as.factor(RatecodeID))) +
    geom_boxplot()

## total amount
 
total  <-  melt(data[,c(17,20,21)], 'total_amount')
ggplot(total, aes(x=variable,y=total_amount, color=value)) +
    geom_boxplot()
ggplot(total, aes(x=value,y=total_amount, color=variable)) +
    geom_boxplot()

total  <- data[,c(17,6)]
ggplot(total, aes(x=as.factor(RatecodeID),y=total_amount, color=as.factor(RatecodeID))) +
    geom_boxplot()
## tolls amount
 
toll  <-  melt(data[,c(15,20,21)], 'tolls_amount')
ggplot(toll, aes(x=variable,y=tolls_amount, color=value)) +
    geom_boxplot()
ggplot(toll, aes(x=value,y=tolls_amount, color=variable)) +
    geom_boxplot()
 
toll  <- data[,c(15,6)]
ggplot(toll, aes(x=as.factor(RatecodeID),y=tolls_amount, color=as.factor(RatecodeID))) +
    geom_boxplot()

## all together:

all  <-  cbind(trip, tip, toll, total, fare)
all  <-  all[,c(1,4,7,10,13,14,15)]

b  <-  melt(all)
names(b)  <-  c("pu_do","borough","variable","value")

ggplot(b, aes(x=borough,y=value)) +
    geom_boxplot(aes(fill=pu_do)) + scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')

## there is a lot of outliers, mostly in largest values showing an skewed distribution. We can see the density funcitons.

## 4.2 density for trip variables

p <- ggplot(trip, aes(x=trip_distance)) + 
  geom_density()


## It is clear that there not a normal distribution.

## 4.2 Try to find the outliers and remove them. follow with that new database.

            
## we made the previous boxplot figure:

## trip

trip  <-  melt(df[,c(5,20,21)], 'trip_distance')

## fare amount

fare  <-  melt(df[,c(11,20,21)], 'fare_amount')

## tip amount

tip  <-  melt(df[,c(14,20,21)], 'tip_amount')

 
p <- ggplot(data, aes(x=trip_distance, color=PU_boroughzone)) + 
  geom_density()
p <- ggplot(df, aes(x=tip_amount, color=DO_boroughzone)) + 
  geom_density()
p <- ggplot(df, aes(x=fare_amount, color=DO_boroughzone)) + 
  geom_density()

ggplot(df, aes(trip_distance)) +
    geom_boxplot(outlier.size = 0.3,outlier.alpha = 0.3)

## total amount
 
total  <-  melt(df[,c(17,20,21)], 'total_amount')


## tolls amount
 
toll  <-  melt(df[,c(15,20,21)], 'tolls_amount')
 

all  <-  cbind(trip, tip, toll, total, fare)
all  <-  all[,c(1,4,7,10,13,14,15)]

b  <-  melt(all)
names(b)  <-  c("pu_do","borough","variable","value")

ggplot(b, aes(x=borough,y=value)) +
    geom_boxplot(aes(fill=pu_do)) + scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')


## 4.5 Show the trip time length as another variable:

trip_time  <-  melt(data[,c(24,20,21)], 'trip_timelength')

summary(trip_time) ## shows negative values.

histogram(trip_time)

## remove them from data:

data  <- data[which(data[,"trip_timelength" ] > 5),] 

trip_time  <-  melt(data[,c(24,20,21)], 'trip_timelength')

## I create again the trip, toll, total and fare variables from melting.

trip  <-  melt(data[,c(5,20,21)], 'trip_distance')
fare  <-  melt(data[,c(11,20,21)], 'fare_amount')
tip  <-  melt(data[,c(14,20,21)], 'tip_amount')
total  <-  melt(data[,c(17,20,21)], 'total_amount')
toll  <-  melt(data[,c(15,20,21)], 'tolls_amount')

all  <-  cbind(trip, tip, toll, total, fare, trip_time)
all  <-  all[,c(1,4,7,10,13,16,17,18)]

b  <-  melt(all)
names(b)  <-  c("pu_do","borough","variable","value")

ggplot(b, aes(x=borough,y=value)) +
    geom_boxplot(aes(fill=pu_do),outlier.size = 0.1,outlier.alpha = 0.3) + scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')


## 4.6 Density functions:

## Besides the boxplots, the density functions are very useful to see diferences in the variables.
p <- ggplot(data, aes(x=trip_timelength, color=DO_boroughzone)) + 
  geom_density()

ggplot(b, aes(x=value, color=borough)) + 
  geom_density()+ scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')

b %>%
    filter(pu_do == 'PU_boroughzone') %>%
    ggplot(aes(x=value, color=borough)) + 
    geom_density()+ scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
    facet_wrap(~ variable, scales='free')

b %>%
    filter(pu_do == 'DO_boroughzone') %>%
    ggplot(aes(x=value, color=borough)) + 
    geom_density()+ scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
    facet_wrap(~ variable, scales='free')

## Functions from and to EWR show different behaviour. If we take a closer look, we see that:

summary(data[,"PU_boroughzone"] == "EWR")
summary(data[,"DO_boroughzone"] == "EWR")
  
## only 2 and 9 records are from or to EWR area. Few records for a model.

ewrpu  <-  data[data$PU_boroughzone ==  'EWR', ]
ewrdo  <-  data[data$DO_boroughzone == "EWR",]

## The toll functions seem to be lambda functions for certain values but with a long tail.

## Few records present large toll amounts:

summary(data[data$tolls_amount > data$fare_amount,])

## how is the density function without these records:

df  <-  data[data$tolls_amount < data$fare_amount,]

trip_time  <-  melt(df[,c(24,20,21)], 'trip_timelength')
trip  <-  melt(df[,c(5,20,21)], 'trip_distance')
fare  <-  melt(df[,c(11,20,21)], 'fare_amount')
tip  <-  melt(df[,c(14,20,21)], 'tip_amount')
total  <-  melt(df[,c(17,20,21)], 'total_amount')
toll  <-  melt(df[,c(15,20,21)], 'tolls_amount')

all  <-  cbind(trip, tip, toll, total, fare, trip_time)
all  <-  all[,c(1,4,7,10,13,16,17,18)]

b  <-  melt(all)
names(b)  <-  c("pu_do","borough","variable","value")

b %>%
    filter(pu_do == 'DO_boroughzone') %>%
    ggplot(aes(x=value, color=borough)) + 
    geom_density()+ scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
    facet_wrap(~ variable, scales='free')

## It does not change much! so better to keep them

###################




## 4.7 Considering the day or night time off pick up as variable.

df  <-  data[,c(5,11,14,15,17,20,22,24)]

b  <-  melt(df)
names(b)  <-  c("pu_borough","time_pu","variable","value")

ggplot(b, aes(x=time_pu,y=value)) +
    geom_boxplot(aes(fill=pu_borough),outlier.size = 0.1,outlier.alpha = 0.3) + scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')

ggplot(b, aes(x=pu_borough,y=value)) +
    geom_boxplot(aes(fill=time_pu),outlier.size = 0.1,outlier.alpha = 0.3) + scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')


#######################################################

## 5. scatterplots with the 5 numeric relevant variables. Linear relationship ¿?

splom(data[,c(5,11,14,17,24)],pch=20, groups=as.factor(data$t_pu),auto.key=TRUE, data=data)

pairs(data[,c(5,11,14,17,24)]), col=data$t_pu)


library(GGally)

df  <- data %>%
    filter(DO_servicezone == 'Airports')

ggpairs(df[,c(5,11,14,17,24)])+ theme_bw()


ggpairs(data[,c(5,11,14,17,24)])+ theme_bw() 

## Calculate the tip percentage with respect to the fare:

df  <-  data[,c("tip_amount","fare_amount")]
data$tip_per  <- df[,1]/df[,2]*100

## tips below 10%

below10  <- data[data$tip_per < 10,]

ggpairs(below10[,c(5,11,14,17,24)])+ theme_bw() 

df  <-  below10[,c(5,11,14,15,17,20,22,24)]

b  <-  melt(df)
names(b)  <-  c("pu_borough","time_pu","variable","value")

ggplot(b, aes(x=time_pu,y=value)) +
    geom_boxplot(aes(fill=pu_borough),outlier.size = 0.1,outlier.alpha = 0.3) + scale_fill_brewer(palette="Dark2") +
    labs(x=" ", y=" ") +
     facet_wrap(~ variable, scales='free')


histogram(below10$PU_boroughzone)
histogram(below10$PU_servicezone)
histogram(below10$DO_boroughzone)
histogram(below10$DO_servicezone)

histogram(below10$PULocationID)
dotplot(below10$PULocationID~)
 
ID  <- factor(below10$PULocationID, levels=seq(1:263))
ID2  <- factor(below10$DOLocationID, levels=seq(1:263))

table(ID)
table(ID2)

## for the pick up and drop off both show a maximum on the Location 138, which correspond to the airport.

## We can assume that the tip is 0 in this case because it is fixed on the fare.

## how the data looks like if we remove the rest of data  below 10%:
 
df  <- data[which(data$tip_per < 10),]
rows  <- row.names(df[df$PULocationID !=138,])

df  <- data[-c(as.numeric(rows)),]

rows  <- row.names(df[df$DOLocationID !=138,])
df  <- data[-c(as.numeric(rows)),]

##ggpairs(df[,c(5,11,14,17,24)])+ theme_bw() 

splom(df[,c(5,11,14,17,24)], groups=as.factor(df$t_pu), cex=0.2, auto.key=TRUE, panel.superpose)

splom(df[,c(5,11,14,17,24)], groups=as.factor(df$DO_servicezone), cex=0.2, auto.key=TRUE, panel.superpose)     


## remove all the unknown do and pu borough zones! because we do not know where are these places.

df  <- df[df$PU_boroughzone != "Unknown",]
df  <- df[df$DO_boroughzone != "Unknown",]

splom(df[,c(5,11,14,17,24)], groups=as.factor(df$t_pu), cex=0.2, auto.key=TRUE, panel.superpose)

histogram(df$tip_amount)
histogram(df$tip_per)
densityplot(df$tip_per)
densityplot(df$tip_amount)

splom(df[,c(5,11,14,17,24,25)], groups=as.factor(df$t_pu), cex=0.2, auto.key=TRUE, panel.superpose)
splom(df[,c(5,11,14,17,24,25)], groups=as.factor(df$DO_boroughzone), cex=0.2, auto.key=TRUE, panel.superpose)

d  <-  df %>%
    filter(PU_boroughzone == "Bronx")
    splom(d[,c(5,11,14,17,24,25)], groups=as.factor(df$t_pu), cex=0.5, auto.key=TRUE, panel.superpose) ## pocos datos
  
d  <-  df %>%
    filter(DO_boroughzone == "Bronx" & t_do == "night")
    splom(d[,c(5,11,14,17,24,25)], cex=0.5, auto.key=TRUE, panel.superpose)

d  <-  df %>%
    filter(DO_boroughzone == "Manhattan" & t_do == "night")
    splom(d[,c(5,11,14,17,24,25)], cex=0.5, auto.key=TRUE, panel.superpose)

d  <-  df %>%
    filter(DO_boroughzone == "Staten Island" & t_do == "day")
    splom(d[,c(5,11,14,17,24,25)], cex=0.5, auto.key=TRUE, panel.superpose)


## explore areas in a map:

## where are the they ask, where are they drop out.
  
data %>% 
    group_by(PU_boroughzone) %>%
    summarise(no_rows = length(PU_boroughzone))

tb <- table(data$PU_boroughzone, data$DO_boroughzone)

tb <- table(data$PULocationID, data$DOLocationID) 
levelplot(tb, zscaleLog=TRUE, xlab='drop off', ylab='pick up')

tb2 <- tb[c(1,2,4,5,7),c(1,2,4,5,7)]
levelplot(tb, zscaleLog=TRUE, xlab='pick up', ylab='drop off')

tb3 <-tb[-7,-7]
levelplot(tb3, zscaleLog=TRUE, xlab='pick up', ylab='drop off')


 
PU_tip<- data %>% 
    group_by(PULocationID) %>%
    summarise(no_rows =  mean(tip_amount)) %>%
    as.data.frame()    
names(PU_tip) <- c("LocationID","PU_mean")



DO_tip<- data %>% 
    group_by(DOLocationID) %>%
    summarise(no_rows =  mean(tip_amount)) %>%
    as.data.frame()    
names(DO_tip) <- c("LocationID","DO_mean")


PU_length  <-  data %>%
    group_by(PULocationID) %>%
    summarise(no_rows = mean(trip_distance))%>%
    as.data.frame()    
names(PU_length) <- c("LocationID","PU_length")

DO_length  <-  data %>%
    group_by(DOLocationID) %>%
    summarise(no_rows = mean(trip_distance))%>%
    as.data.frame()    
names(DO_length) <- c("LocationID","DO_length")


     
