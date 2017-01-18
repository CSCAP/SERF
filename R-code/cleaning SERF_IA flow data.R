library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(zoo)

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

save(loss, file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss.Rda")
load(file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss.Rda")

loss %>% 
  arrange(plot, roundtime) %>%
  mutate(timediff = c(1800, diff(roundtime))/60) -> loss

# find measurement frequency 
loss[which(loss$timediff == 15) %>% rep(each = 5) + (-2:2), ]
loss[which(loss$timediff == 15 & loss$year == 2011) %>% rep(each = 5) + (-2:2), ]
loss[which((loss$timediff == 15)), ] %>%
  filter(year == 2011) %>% 
  select(roundtime) -> tempo
which(loss$timediff == 15 & loss$year == 2011) -> tempoRow
# 2012-2015 drainage data has measurement frequency of 15 min;
# 2011 was measured at 30 min;
# due to rounding there is one case in plot 2 when difference between the measurements are 15 min
# instead of 30 min in 2011 data;
# code below should fix this (see row 2992 for more info)

loss[loss$plot == "Plot 2" & loss$year == 2011 & loss$roundtime > tempo$roundtime, "roundtime"] <-
  loss[loss$plot == "Plot 2" & loss$year == 2011 & loss$roundtime > tempo$roundtime, "roundtime"] - duration(mins = 15)

loss <- loss[-tempoRow, ]

# find more duplicates and fix
loss %>% 
  filter(timediff == 0)
loss[rep(which(loss$timediff == 0), each =5) + (-2:2), ]
loss[-which(loss$timediff == 0 & loss$plot == "Plot 4"), ] -> loss
loss[which(loss$timediff == 0)-1, "roundtime"] <- loss[which(loss$timediff == 0)-1, "roundtime"] - duration(mins = 30)



# round up 2011 roundtime to 30 min interval 
loss %>%
  mutate(t = as.numeric(timestamp)) %>%
  mutate(roundtime = ifelse(year == 2011, round(t/(30*60))*(30*60), roundtime)) %>%  
  mutate(roundtime = as.POSIXct(roundtime, origin = "1970-01-01", tz = "UTC")) %>% # convert back to dym hms
  mutate(timediff = c(1800, diff(roundtime))/60) %>%
  select(plot, year, month, day, time, roundtime, drainage_mm, n_loss) %>%
  mutate(diff = c(1800, diff(roundtime))/60) -> loss

# an more time duplicate check
loss %>%
  filter(diff == 0)
loss[which(loss$diff == 0) + (-2:2),]
loss <- loss[-which(loss$diff == 0), ]



loss %>% 
  select(plot, year, roundtime, n_loss) %>%
  spread(key = plot, value = n_loss) -> loss_n

loss %>% 
  select(plot, year, roundtime, drainage_mm) %>%
  spread(key = plot, value = drainage_mm) -> loss_d


na.approx(loss_n$`Plot 2`, loss_n$roundtime, na.rm = FALSE, maxgap = 3) -> loss_n$p2n
na.approx(loss_n$`Plot 3`, loss_n$roundtime, na.rm = FALSE, maxgap = 3) -> loss_n$p3n
na.approx(loss_n$`Plot 4`, loss_n$roundtime, na.rm = FALSE, maxgap = 3) -> loss_n$p4n
na.approx(loss_n$`Plot 5`, loss_n$roundtime, na.rm = FALSE, maxgap = 3) -> loss_n$p5n
na.approx(loss_n$`Plot 6`, loss_n$roundtime, na.rm = FALSE, maxgap = 3) -> loss_n$p6n
