suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ggplot2)

#DPAC drain flow data transformation

a <- choose.files() # "C:\\Users\\gio\\Documents\\CSCAP\\Data\\DPAC\\2011_2015_DPAC_Drainflow_mm.txt"
dpac <- read.delim(a, header = TRUE)
dpac_df <- dpac
dpac_df$Date.Time <- parse_date_time(dpac_df$Date.Time, "m/d/Y HM")
dpac_df$date_time <- dpac_df$date_time + hours(5)
names(dpac_df) <- c("date_time", "NW", "NE", "SW", "SE")

dpac_df_long <- gather(dpac_df, key = plotID, value = Discharge_mm, 2:5)
dpac_df_long$YEAR <- year(dpac_df_long$date_time)
dpac_df_long <- dpac_df_long[, c(4, 1:3)]

setwd("C:/Users/gio/Documents/CSCAP/Data/DPAC")
save(dpac_df_long, file = "dpac_df.Rda")


ggplot(data = dpac_df_long, aes(date_time, Discharge_mm, group = plotID, colour = plotID)) + 
  geom_line() + facet_wrap(~ YEAR, scales = "free")


#SERF drain flow data transformation
library(readxl)

a <- choose.files() 
#"C:\\Users\\gio\\Documents\\CSCAP\\Data\\SERF_IA\\Tile Flow\\SERF Drainage Data 07 to 15  _ time fixed.xlsx"
setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA/Tile Flow")

#read each excel sheet separately 
for(i in 1:6) {
  assign(paste("S",i,"row",sep = ""), read_excel(a, sheet = i, col_names = TRUE))
}


#combine year, month and daym, and remove redundant rows based on new column "date_time"
for(i in 1:6) {
  Sname <- paste("S",i,"row",sep = "")
  nam <- get(Sname)
  nam[,13] <- ymd(paste(nam$Year, nam$Month, nam$Day, sep = "-"))
  #Nname <- paste("na", i, sep = "")       #this was creating name for a variable to store 
  #assign(Nname, which(is.na(nam[,13])))   #row numbers with NA data in column 13
  nam <- nam[complete.cases(nam[,13]),]
  names(nam)[13] <- "date_time"
  nam[,14] <- ymd_hm(paste(nam$date_time, nam$NT))
  assign(Sname, nam)
}
rm(nam)

#NEED tp correct water sampling date for S3 in 3/27/2010, should be 4/27/2010

for(i in 1:6) {
  Sname <- paste("S",i,"row", sep = "")
  nam <- get(Sname)
  Sname <- paste("S",i,"df", sep = "")
  nam[,"NT"] <- paste("S",i,sep = "")
  nam <- nam[, c("Year", "V14", "NT", "Incremental drainage, mm")]
  names(nam) <- c("YEAR", "date_time", "plotId", "Discharge_mm")
  nam$date_time <- nam$date_time + hours(6)
  nam <- nam[order(nam$date_time),]
  assign(Sname, nam)
}


#find unique time steps in minutes 
for(i in 1:6) {
  Sname <- paste("S",i,"df", sep = "")
  nam <- get(Sname)
  diffe <- unique(diff(nam$date_time))/60
  nam <- paste("diff",i, sep = "")
  assign(nam, diffe[order(diffe)])
}

#remove duplicate values (where time diff = 0)
for(i in 1:6) {
  Sname <- paste("S",i,"df", sep = "")
  nam <- get(Sname)
  nam <- nam[-(which(diff(nam$date_time)==0)+1),]
  assign(Sname, nam)
}
  
##the same as above done individualy 
# rm1 <- which(diff(S1df$date_time)==diff1[1])+1
# S1df <- S1df[-rm1,]
# 
# rm2 <- which(diff(S2df$date_time)==diff2[1])+1
# S2df <- S2df[-rm2,]
# 
# rm3 <- which(diff(S3df$date_time)==diff3[1])+1
# S3df <- S3df[-rm3,]
# 
# rm4 <- which(diff(S4df$date_time)==diff4[1])+1
# S4df <- S4df[-rm4,]
# 
# rm5 <- which(diff(S5df$date_time)==diff5[1])+1
# S5df <- S5df[-rm5,]
# 
# rm6 <- which(diff(S6df$date_time)==diff6[1])+1
# S6df <- S6df[-rm6,]



sum(diff(S6df$date_time)==sort(unique(diff(S6df$date_time)))[5])

#aggregate data hourly
for(i in 1:6) {
  Sname <- paste("S",i,"df", sep = "")
  nam <- get(Sname)
  new <- aggregate(list(Discharge_mm = nam$Discharge_mm),
                   list(date_time = cut(nam$date_time, "1 hour")),
                   sum)
  new$plotID <- paste("S", i, sep = "")
  assign(paste("S",i, sep = "_"), new)
}
rm(new)

serf_df_long <- do.call(rbind,(lapply(ls(pattern = "S_"), get)))
serf_df_long$YEAR <- year(serf_df_long$date_time)
serf_df_long$date_time <- ymd_hms(serf_df_long$date_time)
serf_df_long <- serf_df_long[, c(4,1,3,2)]


setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA")
save(serf_df_long, file = "serf_df_2007-2015.Rda")


library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)

#creat NA-less data for plotting
plot_serf_df_long <- serf_df_long[complete.cases(serf_df_long$Discharge_mm) & serf_df_long$YEAR %in% 2007:2015,]
  
qplot(x = date_time, y = Discharge_mm,
      data = serf_df_long, na.rm = TRUE,
      main = "SERF Hourly Tile Flow\n 2007-2015",
      xlab = "Date", ylab = "Tile Flow (mm)")


ggplot(plot_serf_df_long, aes(date_time, Discharge_mm)) + 
  geom_point(na.rm = TRUE, color = "navyblue", size = 3, pch = 20) +
  ylim(c(0,1)) +
  xlab("Date") + ylab("Tile Flow (mm)") +
  ggtitle("SERF\n Hourly Tile Flow")


#plot subset of data by Time
startTime <- as.POSIXct("2013-01-01 00:00:00") #NOTICE time zone is choosen according to your system time zone
endTime <- as.POSIXct("2014-01-01 00:00:00")

start.end <- c(startTime, endTime)

ggplot(plot_serf_df_long, aes(date_time, Discharge_mm)) + 
  geom_point(na.rm = TRUE, color = "purple", size = 2, alpha = 0.25) +
  labs(x = "Date", y = "Tile Flow (mm)", title = "SERF\n Hourly Tile Flow") + 
  ylim(c(0,1)) +
  scale_x_datetime(limits = start.end, 
                   breaks = date_breaks("2 months"), 
                   labels = date_format("%b %y")) -> SERF2011DrainFlow

SERF2011DrainFlow +
  theme(plot.title = element_text(lineheight = 1.2, face = "bold", size = 18)) +
  theme(text = element_text(size = 16))
  


#group by plot ID
ggplot(plot_serf_df_long, aes(date_time, Discharge_mm, colour = plotID)) + 
  geom_point(na.rm = TRUE, size = 1) +
  labs(x = "Date", y = "Tile Flow (mm)", title = "SERF\n Hourly Tile Flow") + 
  ylim(c(0,1)) +
  scale_x_datetime(limits = start.end, 
                   breaks = date_breaks("2 months"), 
                   labels = date_format("%b %y")) -> SERF2011DrainFlow

SERF2011DrainFlow +
  theme(plot.title = element_text(lineheight = 1.2, face = "bold", size = 18, family = "TT Times New Roman")) +
  theme(text = element_text(size = 16)) 


ggplot(data = plot_serf_df_long, aes(date_time, Discharge_mm, group = plotID, colour = plotID)) + 
  geom_point() + facet_wrap(~ YEAR, scales = "free") + scale_x_datetime(labels = date_format("%b"))

