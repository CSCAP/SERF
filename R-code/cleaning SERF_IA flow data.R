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
  select(plot, Year, Month, Day, Time, `Incremental drainage, mm`, `Nitrate loss, kg/ha`) %>%
  mutate(Time = round_date(Time, "mins")) %>%
  mutate(Time = format.Date(Time, "%H:%M")) %>% 
  mutate(timestamp = parse_date_time(paste(paste(Year, Month, Day, sep = "-"), Time), "ymd HM")) -> loss

# rename columns
names(loss) <- c("plot", "year", "month", "day", "time", "drainage_mm", "n_loss", "timestamp")


# find non-consequtive dates
loss %>%
  mutate(diff = c(1800, diff(timestamp))/60) -> loss
loss[which(loss$diff < 0 & loss$diff > -60*24*365*4), ]
# there are three non-consequtive entries 
# look at those values in the "context" (with neighboring data)
which(loss$diff < 0 & loss$diff > -60*24*365*4) -> tempo
loss[rep(tempo[1], each = 10) + (-8:1), ]  # remove this row
loss[rep(tempo[2], each = 10) + (-8:1), ]  # need to fix "month" (replace 1 with 3)
loss[rep(tempo[3], each = 10) + (-5:4), ]  # need to fix "time" (should be 7:46 instead of 3:46)

loss[tempo[3], "time"] <- "07:46"
loss[tempo[2], "month"] <- 3
loss <- loss[-tempo[1], ]

# redo timestamp to fix corrected values and add rounded time to the nearest 15 min
loss %>%
  mutate(timestamp = parse_date_time(paste(paste(year, month, day, sep = "-"), time), "ymd HM")) %>%
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

loss[loss$plot == "plot2" & loss$year == 2011 & loss$roundtime > tempo$roundtime, "roundtime"] <-
  loss[loss$plot == "plot2" & loss$year == 2011 & loss$roundtime > tempo$roundtime, "roundtime"] - duration(mins = 15)

loss <- loss[-tempoRow, ]

# find more duplicates and fix
loss %>% filter(timediff == 0)


# round up 2011 roundtime to 30 min interval 
loss %>%
  mutate(t = as.numeric(timestamp)) %>%
  mutate(roundtime = ifelse(year == 2011, round(t/(30*60))*(30*60), roundtime)) %>%  
  mutate(roundtime = as.POSIXct(roundtime, origin = "1970-01-01", tz = "UTC")) %>% # convert back to dym hms
  mutate(timediff = c(1800, diff(roundtime))/60) %>%
  select(plot, year, month, day, time, roundtime, drainage_mm, n_loss) %>%
  mutate(diff = c(1800, diff(roundtime))/60) -> loss

# an more time duplicate check
loss %>% filter(diff == 0)
loss[which(loss$diff == 0) + (-2:2),]
which(loss$diff == 0) -> tempo 
loss[tempo, "roundtime"] <- loss[tempo, "roundtime"] + duration(mins = 30)
loss$diff <- NULL


# TRANSFORM dataframe ========================================
loss %>% 
  gather(variable, value, drainage_mm:n_loss) %>% 
  unite(temp, plot, variable, sep = " ") %>%
  select(-(month:time)) %>%
  spread(temp, value) -> serf_loss

save(loss, file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss.Rda")
#load(file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss.Rda")


save(serf_loss, file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss_wide.Rda")

write.csv(loss, 
          file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/SERF_Flow_n_NO3.csv",
          quote = FALSE,
          row.names = FALSE)

write.csv(serf_loss, 
          file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/SERF_Flow_n_NO3_wide.csv",
          quote = FALSE,
          row.names = FALSE)


# check that annual NO3-N loss are accurate
serf_loss %>% 
  group_by(year) %>% 
  summarize_at(vars(matches("n_loss")), sum, na.rm = TRUE)


# FILL IN the NAs =====
# shows how you can change size of the gap
serf_loss %>% 
  group_by(year) %>%
  mutate(`plot2 n_loss NEW` = na.approx(`plot2 n_loss`, roundtime, na.rm = F, maxgap = 1)) %>%
#serf_loss %>% 
  select(roundtime, `plot2 drainage_mm`, `plot2 n_loss`, `plot2 n_loss NEW`) %>% 
  filter(roundtime > "2011-01-01 21:00") %>%
  head(16)



# # interpolate if gap is <= 10

serf_loss %>%
  group_by(year) %>%

  mutate(plot1_n_loss = na.approx(`plot1 n_loss`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot2_n_loss = na.approx(`plot2 n_loss`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot3_n_loss = na.approx(`plot3 n_loss`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot4_n_loss = na.approx(`plot4 n_loss`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot5_n_loss = na.approx(`plot5 n_loss`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot6_n_loss = na.approx(`plot6 n_loss`, roundtime, na.rm = FALSE, maxgap = 10)) %>%

  mutate(plot1_flow = na.approx(`plot1 drainage_mm`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot2_flow = na.approx(`plot2 drainage_mm`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot3_flow = na.approx(`plot3 drainage_mm`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot4_flow = na.approx(`plot4 drainage_mm`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot5_flow = na.approx(`plot5 drainage_mm`, roundtime, na.rm = FALSE, maxgap = 10)) %>%
  mutate(plot6_flow = na.approx(`plot6 drainage_mm`, roundtime, na.rm = FALSE, maxgap = 10)) %>%

  # select(roundtime, year,
  #        plot2_flow, plot2_n_loss,
  #        plot3_flow, plot3_n_loss,
  #        plot4_flow, plot4_n_loss,
  #        plot5_flow, plot5_n_loss,
  #        plot6_flow, plot6_n_loss) %>%

  ungroup()-> serf_loss_FILLED

serf_loss_FILLED %>%
  group_by(year) %>%
  summarize_at(vars(matches("n_loss")), sum, na.rm = TRUE) %>% 
  select(contains("plot2"))


serf_loss_FILLED %>%
  group_by(year) %>%
  summarize_at(vars(matches("_n_loss")), sum, na.rm = T)

save(serf_loss_FILLED, 
     file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/no3_loss_wide_FILLED.Rda")

write.csv(serf_loss_FILLED, 
          file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/SERF/SERF_Flow_n_NO3_wide_FILLED.csv",
          quote = FALSE,
          row.names = FALSE)
