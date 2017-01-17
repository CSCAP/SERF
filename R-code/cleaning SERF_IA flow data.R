library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)

setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA/Tile Flow")

p2 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot2")
p3 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot3")
p4 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot4")
p5 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot5")
p6 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot6")

p2$plot <- "Plot 2"
p3$plot <- "Plot 3"
p4$plot <- "Plot 4"
p5$plot <- "Plot 5"
p6$plot <- "Plot 6"


# combine data and add timestamp
bind_rows(p2, p3, p4, p5, p6) %>% 
  filter(Year > 2010) %>%
  select(plot, Year, Month, Day, Time, `Incremental drainage, mm`, `Nitrate loss, kg/ha`) %>%
  mutate(Time = round_date(Time, "mins")) %>%
  mutate(Time = format.Date(Time, "%H:%M")) %>% 
  mutate(timestamp = parse_date_time(paste(paste(Year, Month, Day, sep = "-"), Time), "ymd HM")) -> loss

# rename columns
names(loss) <- c("plot", "year", "month", "day", "time", "drainage_mm", "n_loss", "timestamp")


# round time to nearest 15 min 
loss %>%
  mutate(roundtime = as.numeric(timestamp)) %>%
  mutate(roundtime = round(roundtime/(15*60))*(15*60)) %>%  # round to 15 min intervals
  mutate(roundtime = as.POSIXct(roundtime, origin = "1970-01-01", tz = "UTC")) %>% # convert back to dym hms
  mutate(timediff = difftime(roundtime, timestamp, units = "mins")) -> loss

# see when conscutive round times are the same (not different)
loss %>% 
  select(roundtime) %>% 
  mutate(dif = c(1800, diff(roundtime))) %>%
  #select(dif) %>% unique() %>% arrange(dif)
  filter(dif == 0)

# look at the data
loss[rep(which(diff(loss$roundtime) == 0), each =3) + (-1:1), ]

# n_loss value following duplicated date (i.e. the second duplicate) is always 0
# get rid of the second duplicated value (there are 18 of them)
loss[-(which(diff(loss$roundtime)==0)+1), ] -> loss

save(loss, file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss.RData")

# find measurement frequency 
loss[which((diff(loss$roundtime) == 15)) %>% rep(each = 5) + (-1:3), ]
loss[which((diff(loss$roundtime) == 15)), ] %>%
  filter(year == 2011) %>% 
  select(roundtime) -> tempo
# 2012-2015 drainage data has measurement frequency of 15 min;
# 2011 was measured at 30 min;
# due to rounding there is one case in plot 2 when difference between the measurements are 15 min
# instead of 30 min in 2011 data;
# code below should fix this 

loss$roundtime[loss$year == 2011 & loss$plot == "Plot 2" & loss$roundtime > tempo$roundtime] <-
  loss$roundtime[loss$year == 2011 & loss$plot == "Plot 2" & loss$roundtime > tempo$roundtime] + 
  minutes(15)

loss[which((diff(loss$roundtime) == 15)) %>% rep(each = 5) + (-2:2), ] #%>%
filter(plot != "Plot 2")
