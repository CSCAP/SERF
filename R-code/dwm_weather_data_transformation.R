## THIS CODE READS WEATHE DATA AND RESTRUCTURES IT FOR DPAC AND SERF sites
#install.packages("weathermetrics")
library(weathermetrics)
library(lubridate)

setwd("C:/Users/gio/Documents/CSCAP/Data/DPAC")
dpac_weather <- read.table("FieldWweather_hourly_2005_2015.txt", header = TRUE)
dpac_weather$date_time <- ymd(paste(dpac_weather$YEAR, dpac_weather$MONTH, dpac_weather$DAY, sep = "-"))
dpac_weather$date_time <- with_tz(dpac_weather$date_time + hours(dpac_weather$HOUR+5), tz = "UTC")
dpac_weather$prec_mm <- dpac_weather$PREC
dpac_weather$tempair_C <- dpac_weather$TAIR

dpac_weather <- dpac_weather[, c(1,9:11)]
head(dpac_weather)


save(dpac_weather, file = "dpac_weather.Rda")



setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA")
a <- read.csv("C:\\Users\\gio\\Documents\\CSCAP\\Data\\SERF_IA\\SERF_Weather_2013-2015.csv")
a$date_time <- ymd_hm(a$valid)
a$prec_mm <- a$precip * 25.4 #convert in to mm
a$tempair_C <- fahrenheit.to.celsius(a$tmpf, 2) #convert C to F
a$YEAR <- year(a$date_time)
a <- a[, c("YEAR", "date_time", "prec_mm", "tempair_C")]

b <- read.csv("C:\\Users\\gio\\Documents\\CSCAP\\Data\\SERF_IA\\SERF_Weather_2011-2014.txt", header = TRUE)
b$date_time <- ymd_hm(b$valid)
b$prec_mm <- b$Hourly.Precip * 25.4 #convert in to mm
b$tempair_C <- fahrenheit.to.celsius(b$Air.Temp, 2)
b$YEAR <- year(b$date_time)
b <- b[, c("YEAR", "date_time", "prec_mm", "tempair_C")]


a$date_time[1] -> END
b$date_time[length(b$date_time)] -> START

serf_weather <- rbind(b[b$date_time<END,],a)


proba <- which(diff(serf_weather$date_time)==0)
probb <- c(proba-1, proba, proba+1)
probb <- probb[order(probb)]


serf_weather$newTime <- force_tz(serf_weather$date_time, tzone = "America/Chicago")
serf_weather$newTime <- with_tz(serf_weather$newTime, tzone = "UTC")

serf_weather$newTime[proba] <- serf_weather$newTime[proba] - hours(1)

serf_weather$date_time <- serf_weather$newTime
serf_weather$newTime <- NULL

head(serf_weather$date_time)

save(serf_weather, file = "serf_weather.Rda")

