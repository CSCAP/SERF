library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(zoo)

setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA/Tile Flow")

p1 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot1")
p2 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot2")
p3 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot3")
p4 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot4")
p5 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot5")
p6 <- read_excel("~/CSCAP/Data/SERF_IA/Tile Flow/SERF Drainage Data 07 to 15.xlsx", sheet = "Plot6")

p1$plot <- "plot1"
p2$plot <- "plot2"
p3$plot <- "plot3"
p4$plot <- "plot4"
p5$plot <- "plot5"
p6$plot <- "plot6"


# combine data and add timestamp
bind_rows(p1, p2, p3, p4, p5, p6) %>% 
  filter(Year > 2010) %>%
  mutate(Time = round_date(Time, "mins")) %>%
  mutate(Time = format.Date(Time, "%H:%M")) %>% 
  mutate(timestamp = parse_date_time(paste(paste(Year, Month, Day, sep = "-"), Time), "ymd HM")) -> loss


# find non-consequtive dates
loss %>%   mutate(diff = c(1800, diff(timestamp))/60) -> loss
loss[which(loss$diff < 0 & loss$diff > -60*24*365*4), ]
# there are three non-consequtive entries 
# look at those values in the "context" (with neighboring data)
which(loss$diff < 0 & loss$diff > -60*24*365*4) -> tempo
loss[rep(tempo[1], each = 10) + (-8:1), ]  # remove this row
loss[rep(tempo[2], each = 10) + (-8:1), ]  # need to fix "month" (replace 1 with 3)
loss[rep(tempo[3], each = 10) + (-5:4), ]  # need to fix "time" (should be 7:46 instead of 3:46)


loss[tempo[3], "Time"] <- "07:46"
loss[tempo[2], "Month"] <- 3
loss <- loss[-tempo[1], ]

names(loss)

# redo timestamp to fix corrected values and add rounded time to the nearest 15 min
loss %>%
  mutate(timestamp = parse_date_time(paste(paste(Year, Month, Day, sep = "-"), Time), "ymd HM")) %>%
  mutate(diff = c(1800, diff(timestamp))/60) %>% 
  mutate(roundtime = as.numeric(timestamp)) %>%
  mutate(roundtime = round(roundtime/(15*60))*(15*60)) %>%  # round to 15 min intervals
  mutate(roundtime = as.POSIXct(roundtime, origin = "1970-01-01", tz = "UTC")) %>% # convert back to dym hms
  mutate(timediff = difftime(roundtime, timestamp, units = "mins")) -> loss



# see when conscutive round times are the same (not different)
loss %>% 
  mutate(temp = c(1800, diff(roundtime))) %>%
  filter(temp == 0)

# look at the data
loss[rep(which(diff(loss$roundtime) == 0), each =3) + (-1:1), ]

# n_loss value following duplicated date (i.e. the second duplicate) is always 0
# get rid of the second duplicated value (there are 18 of them)
loss[-(which(diff(loss$roundtime)==0)+1), ] -> loss


loss %>% 
  arrange(plot, roundtime) %>%
  group_by(plot) %>%
  mutate(timediff = c(30, diff(roundtime))) -> loss

# find measurement frequency 
loss[which(loss$timediff == 15) %>% rep(each = 5) + (-2:2), ]
loss[which(loss$timediff == 15 & loss$Year == 2011) %>% rep(each = 5) + (-2:2), ]
loss[which((loss$timediff == 15)), ] %>%
  filter(Year == 2011) %>% 
  select(roundtime) -> tempo
which(loss$timediff == 15 & loss$Year == 2011) -> tempoRow
# 2012-2015 drainage data has measurement frequency of 15 min;
# 2011 was measured at 30 min;
# due to rounding there is one case in plot 2 when difference between the measurements are 15 min
# instead of 30 min in 2011 data;
# code below should fix this (see row 2992 for more info)

loss[loss$plot == "plot2" & loss$Year == 2011 & loss$roundtime > tempo$roundtime, "roundtime"] <-
  loss[loss$plot == "plot2" & loss$Year == 2011 & loss$roundtime > tempo$roundtime, "roundtime"] - duration(mins = 15)

loss <- loss[-tempoRow, ]

# find more duplicates and fix
loss %>% filter(timediff == 0)


# round up 2011 roundtime to 30 min interval 
loss %>%
  mutate(t = as.numeric(timestamp)) %>%
  mutate(roundtime = ifelse(Year == 2011, round(t/(30*60))*(30*60), roundtime)) %>%  
  mutate(roundtime = as.POSIXct(roundtime, origin = "1970-01-01", tz = "UTC")) %>% # convert back to dym hms
  mutate(timediff = c(1800, diff(roundtime))/60) %>%
  #select(plot, year, month, day, time, roundtime, drainage_mm, n_loss) %>%
  mutate(diff = c(1800, diff(roundtime))/60) -> loss

# an more time duplicate check
loss %>% filter(diff == 0)
loss[which(loss$diff == 0) + (-2:2),]
which(loss$diff == 0) -> tempo 
loss[tempo, "roundtime"] <- loss[tempo, "roundtime"] + duration(mins = 30)
loss$diff <- NULL


# TRANSFORM dataframe ========================================
loss %>%
  select(-c(Month, Day, Time, timestamp, t, timediff)) -> loss

names(loss) <- c("year", "no3_con", "no3_int", "flow_mm", "no3_loss", "plot", "date")

loss %>%
  select(plot, year, date, no3_con, no3_int, flow_mm, no3_loss) %>%
  arrange(plot, date) -> loss

# check that annual NO3-N loss are accurate
loss %>% 
  group_by(plot, year) %>% 
  summarize_at(vars(matches("_loss")), sum, na.rm = TRUE) %>%
  spread(key=plot, value=no3_loss)


# check the first no3-n conc values each year and plot
loss %>% 
  group_by(plot, year) %>%
  filter(row_number()==1) %>%
  select(-c(date, flow_mm, no3_loss)) %>%
  spread(key=plot, value=no3_int)




# # INTERPOLATE NO3-N by date =====
# loss %>% 
#   group_by(plot) %>%
#   mutate(no3_int_NEW = na.approx(no3_con, date, na.rm = F)) %>% 
#   filter(!is.na(no3_int_NEW)) %>%
#   select(-c(flow_mm, no3_loss)) %>%
#   mutate(diff = no3_int_NEW - no3_int) %>%
#   #summarize_at(vars(diff), mean)
#   head(16)
# 
# loss %>% 
#   group_by(plot) %>%
#   mutate(no3_int_NEW = na.approx(no3_con, date, na.rm = F)) %>% 
#   select(-c(flow_mm, no3_loss)) %>% 
#   group_by(plot, year) %>%
#   filter(!is.na(no3_int_NEW)) %>%
#   summarise_at(vars(matches("no3")), sum, na.rm = T) %>%
#   mutate(diff = no3_int_NEW - no3_int)
# 
# # check if all NO3 concentrations are present after rounding time
# loss %>% 
#   filter(!is.na(no3_con)) %>% 
#   select(plot, year, no3_con) -> a 
  
# DO a REAL INTERPOLATION for NO3-N concentration ========
loss %>%
  group_by(plot, year) %>%
  mutate(no3_int_NEW = na.approx(no3_con, date, na.rm = F)) %>% 
  mutate(no3_int_NEW = na.locf(no3_int_NEW, na.rm = F)) %>%
  mutate(no3_int_NEW = na.locf(no3_int_NEW, na.rm = F, fromLast = TRUE)) -> loss


# interpolate tile flow if gap is <= 10
loss %>%
  group_by(plot, year) %>%
  mutate(flow_mm_NEW = na.approx(flow_mm, date, na.rm = FALSE, maxgap = 10)) %>% 
  mutate(loss = flow_mm_NEW * 10000 * no3_int_NEW / 10^6) -> loss
  


loss %>%
  group_by(plot, year) %>%
  summarize_at(vars(matches("loss")), sum, na.rm = TRUE) -> SERF_NO3_Loss_Summary






save(loss, 
     file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss_with_NO3_interp.Rda")

write.csv(loss, 
          file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/SERF_Flow_n_NO3_with_NO3_interp.csv",
          quote = FALSE,
          row.names = FALSE)
