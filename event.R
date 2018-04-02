library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

my.getSeason <- function(mon){
    res <- rep(NA, length(mon))
    res[mon<=12] <- "winter"
    res[mon<=9] <- "fall"
    res[mon<=6] <- "summer"
    res[mon<=3] <- "spring"
    return(res)
}

my.getPeriod <- function(hour){
    res <- rep(NA, length(hour))
    res[hour<=max(hour)] <- "night"
    res[hour<=19] <- "afternoon"
    res[hour<=14] <- "noon"
    res[hour<=10] <- "morning"
    res[hour<=5] <- "early morning"
    return(res)
}

my.getcall2017Class <- function(title){
    res <- rep(NA, length(title))
    res[grepl("EMS:", title)] <- "EMS"
    res[grepl("Fire:", title)] <- "Fire"
    res[grepl("Traffic:", title)] <- "Traffic"
    return(res)
}

call2017 <- fread("911.csv")

# timeStamp: conver string to Date 
call2017$date = as.Date(call2017$timeStamp)

# timing information
call2017 <- call2017 %>%
    mutate(year = year(timeStamp),
           month = month(timeStamp), # happend in which month in a year (1,12)
           week  = week(timeStamp), # happen in which week in a year (1,52)
           weekday = wday(timeStamp), # happen in which day in a week (1,7)
           hour  = hour(timeStamp)) # happen in which hour of a day (1,24)

# focus on 2017
call2017 <- call2017 %>% filter(year==2017)

# self-defined function in the beginning
call2017 <- call2017 %>% 
    mutate(season = my.getSeason(month), # happen in which season, (spring ,summer, fall, winter)
           period = my.getPeriod(hour), # happen in which period, (early morning, morning, noon, afternoon, night)
           class = my.getcall2017Class(title)) # which class of call2017, (EMS, Fire, Traffic)

saveRDS(call2017, file="911.Rds")