library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(reshape2)

weather <- read_rds('weather.Rds')
call2017 <- read_rds('911.Rds') %>% as_tibble() # obtain by event.R

# keep US stations only
us <- str_detect(weather$ID,'^US')
weather_us <- weather[us,]
# remove value without state,lat,long
weather_us <- weather_us %>%
    filter(!is.na(state))
# keep 2017 only
weather_us$Date <- ymd(weather_us$Date)
weather_us <- weather_us %>%
    filter(year(Date) == 2017)
# cast to a wide data frame
weather_us = weather_us %>% 
    dcast(ID+Date+lat+long~Type,value.var="Value")
# remove NA in TMIN, TMAX, PRCP 
weather_us = weather_us %>% 
    filter(!is.na(TMIN) & !is.na(TMAX) & !is.na(PRCP)) 
weather_us$SNOW <- NULL
weather_us$SNWD <- NULL


###  find the nearest station to event ###
# get stations ID,lat,long in us
station <- weather_us %>% 
    select(ID,lat,long) %>% 
    unique() 
# find the nearest station to event
station_ID_by_call <- sapply(seq(1,nrow(call2017)), function(i){
    cat(i,end="\r")
    imin = which.min((call2017$lat[i] - station$lat)^2 + (call2017$lng[i] - station$long)^2)
    return(station$ID[imin])
})
# add into dataframe
call2017 <- call2017 %>% 
    mutate(station_ID=station_ID_by_call)

# join weather into call
call2017$e <- NULL
call <- call2017 %>% 
    left_join(weather_us,by=c('station_ID' = 'ID','date' = 'Date'))
colnames(call) <- c('lat','lng','desc','zip','title','time','borough','address',
                    'date','year','month','week','weekday','hour','season','period',
                    'event.type','station.id','lat.station','lng.station',
                    'prcp','tmax','tmin')

apply(call,2,function(x){return(sum(is.na(x)))})
# clean the missing weather data after merge
call <- call %>% 
    filter(!is.na(tmax))

saveRDS(call, file="data2017.Rds")

# test <- "Traffic:DISABLED VEHICLE -"
# str_split(test,':')




