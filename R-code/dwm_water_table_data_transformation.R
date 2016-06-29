suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ggplot2)

cat("\f")

#DATA is in this file "C:\Users\gio\Documents\CSCAP\Data\DPAC\DPAC_20110101_20200801.csv"
a <- choose.files() # "C:\\Users\\gio\\Documents\\CSCAP\\Data\\DPAC\\DPAC_20110101_20200801.csv"
dpac <- read.csv(a, stringsAsFactors = FALSE)
dpac_wt_long <- dpac

#get rid of last column (water table depth in ft)
dpac_wt_long$depth_f <- NULL
names(dpac_wt_long)[3] <- "wt_depth_mm"

##convert to date format with UTC time
dpac_wt_long$timestamp <- ymd_hms(dpac_wt_long$timestamp) 
##OMIT this because it screws up "spread" function due to daytime saving
#d$timestamp <- ymd_hms(d$timestamp, tz = "America/Indiana/Indianapolis") 

#make data wide
dpac_wt_wide <- spread(dpac_wt_long, key = plotid, value = wt_depth_mm)

library(datamart)
dwm_plot <- read.delim("C:\\Users\\gio\\Documents\\CSCAP\\Analysis\\DWM paper\\dwm_plot_info_DATA.txt")
dwm_plot$TileDepth_mm <-uconv(dwm_plot$TileDepth_.ft., "ft", "mm", uset = "Length")
setwd("C:/Users/gio/Documents/CSCAP/Analysis/DWM paper")
load(file = "dwm_mngt_info.Rda", verbose = TRUE)
dwm_mngt<-a
dwm_mngt$outletdepth_mm <- uconv((dwm_mngt$outletdepth), "cm", "mm", uset = "Length")
rm(a)


#extract DPAC dwm management data
dpac_mngt <- dwm_mngt[dwm_mngt$siteID=="DPAC",]
#assign time (12:00) in accordance to time zone and convert to UTC then
dpac_mngt$outlet_date <- with_tz(force_tz(update(dpac_mngt$outlet_date, hour=12),
                                          "America/Indiana/Indianapolis"),"UTC")
#make the data wide again 
dpac_mngt_wide <- spread(dpac_mngt, key = plotID, value = outletdepth_mm)
dpac_mngt_wide$outletdepth <- NULL
dpac_mngt_wide <- dpac_mngt_wide[order(dpac_mngt_wide$outlet_date),]
dpac_mngt_wide$NW[dpac_mngt_wide[,5]==1000] <- 910 #assign tile depth to free drainage mode
#NOTICE that I only corrected free drainage tile depth for row 5 (NE) but it applies to both plots


#extract SERF dwm management data
serf_mngt <- dwm_mngt[dwm_mngt$siteID=="SERF",]
#get read of redandunt columns (years when outlet hight was not changed)
serf_mngt <- serf_mngt[!is.na(serf_mngt$outlet_date),]
#assign time (12:00) in accordance to time zone and convert to UTC then
serf_mngt$outlet_date <- with_tz(force_tz(update(serf_mngt$outlet_date, hour=12),
                                          "America/Chicago"),"UTC")
#make the data wide again 
serf_mngt_wide <- spread(serf_mngt, key = plotID, value = outletdepth_mm)
serf_mngt_wide$outletdepth <- NULL
#add a dummy entry for outlet hight before April 2011 
serf_mngt_wide <- rbind(serf_mngt_wide, serf_mngt_wide[2,])
serf_mngt_wide$outlet_date[dim(serf_mngt_wide)[1]] <- update(serf_mngt_wide$outlet_date[dim(serf_mngt_wide)[1]], 
                                                             year=2010, month=12, day=31)
#add comment that this was intorduced by you
serf_mngt_wide$comments[dim(serf_mngt_wide)[1]] <- "Added for calculation purpose by Gio"
#order by date
serf_mngt_wide <- serf_mngt_wide[order(serf_mngt_wide$outlet_date),]



#code to calculate difference between outlet hight and water table 
#above outlet in Controlled DWM and above tile depth in Free DWM
#(both are reported as depth below ground in mm)
str(dpac_wt_wide)
str(dpac_mngt_wide)

dpac_wt_wide$atNW <- NA
dpac_wt_wide$atSE <- NA
dpac_wt_wide$atSW <- NA
dpac_wt_wide$atNE <- NA


for(i in 1:(length(dpac_mngt_wide$outlet_date)-1)) {
  
  dpac_mngt_wide$outlet_date[i] -> START
  dpac_mngt_wide$outlet_date[i+1] -> FINISH
  
  dpac_wt_wide[dpac_wt_wide$timestamp>START & dpac_wt_wide$timestamp<=FINISH,c("atNW", "atSE")] <- 
    dpac_mngt_wide[dpac_mngt_wide$outlet_date==START, 5] - 
    dpac_wt_wide[dpac_wt_wide$timestamp>START & dpac_wt_wide$timestamp<=FINISH,c("NW", "SE")]
}

dpac_wt_wide[dpac_wt_wide$timestamp>FINISH,c("atNW", "atSE")] <- 
  dpac_mngt_wide[dpac_mngt_wide$outlet_date==FINISH, 5] - 
  dpac_wt_wide[dpac_wt_wide$timestamp>FINISH,c("NW", "SE")]

dpac_wt_wide[,c("atSW", "atNE")] <- dwm_plot[dwm_plot$SiteID=="DPAC",][1,9] - 
  dpac_wt_wide[,c("SW", "NE")]

#SAVE DATA HERE
setwd("C:/Users/gio/Documents/CSCAP/Data/DPAC")
save(dpac_wt_wide, file = "dpac_wt.Rda")
write.table(dpac_wt_wide, file = "dpac_wt", quote = FALSE, sep = "\t", row.names = FALSE)
write.csv(dpac_wt_wide, file = "dpac_wt.txt", quote = FALSE, row.names = FALSE)

plot_data <- gather(data = dpac_wt_wide[,-(2:5)], key = plotID, value = wt_above, -timestamp)
plot_data$plotID <- as.factor(plot_data$plotID)
plot_data$year <- as.factor(year(plot_data$timestamp))
#plot_data$timestamp <- as.Date(plot_data$timestamp)

#2011-2015 DPAC water table above outlet/tile
ggplot(data = plot_data, aes(x=timestamp, y=wt_above, color=plotID, group =plotID)) +
  geom_line() +
  
  ggtitle("Water Table Height \n(above outlet/tile)") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  labs(x = "Year", y = "Height above tile/outlet, mm") +
  theme(axis.title = element_text(size = 12)) +
  scale_colour_discrete(name = "Plot ID",
                       breaks = c("atNE", "atNW", "atSE", "atSW"),
                       labels = c("NE", "NW", "SE", "SW")) +
  theme(legend.title = element_text(colour = "chocolate", face = "bold"),
        legend.text = element_text(colour = "chocolate"))
ggsave("DPAC.png", width = 20, height = 12, dpi = 72)

#Faceted by years
ggplot(data = plot_data, aes(x=timestamp, y=wt_above, color=plotID, group =plotID)) +
  geom_line() + 
  facet_wrap(~year, ncol = 3, scales = "free")

for(i in 2011:2015) {
ggplot(data = plot_data[plot_data$year==i,], aes(x=timestamp, y=wt_above, color=plotID, group =plotID)) +
  geom_line() + 
  ggtitle("Water Table Height \n(above outlet/tile)") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  #scale_x_date(date_labels = "%b") +
  labs(x = "", y = "Height above tile/outlet, mm") +
  theme(axis.title = element_text(size = 12)) +
  scale_colour_discrete(name = "Plot ID",
                        breaks = c("atNE", "atNW", "atSE", "atSW"),
                        labels = c("NE", "NW", "SE", "SW")) +
  theme(legend.title = element_text(colour = "chocolate", face = "bold"),
        legend.text = element_text(colour = "chocolate"))
ggsave(filename = paste("DPAC",i,".png"), width = 20, height = 12, dpi = 72)  
}

#DPAC water table depth 2011:2015
dpac_wt_long$year <- as.factor(year(dpac_wt_long$timestamp))
qplot(timestamp, wt_depth_mm, data = dpac_wt_long, colour = plotid, geom = "line") +
  scale_y_reverse() +
  facet_wrap(~year, ncol = 2, scales = "free")
ggsave("DPAC_wt_depth.png", width = 20, height = 15, dpi = 72)






#SERF STARTs HERE
setwd("C:\\Users\\gio\\Documents\\CSCAP\\Data\\SERF_IA")
load("serf_wt_2007-2015.Rda")
str(serf_wt)
#reduce data to 2011_2015
serf_wt_long <- serf_wt[serf_wt$year %in% 2011:2015,]
serf_wt_long <- droplevels(serf_wt_long[!serf_wt_long$plotID %in% c("S7", "S8"),])

str(serf_wt_long)
str(serf_mngt_wide)

serf_wt_long$atS <- NA

for(i in 1:(length(serf_mngt_wide$outlet_date)-1)) {
  START <- serf_mngt_wide$outlet_date[i]
  FINISH <- serf_mngt_wide$outlet_date[i+1]
  
  serf_wt_long[serf_wt_long$date_time>START & serf_wt_long$date_time<=FINISH & 
                 serf_wt_long$plotID %in% c("S3", "S4"),"atS"] <- 
    serf_mngt_wide[serf_mngt_wide$outlet_date==START,5] -
    serf_wt_long[serf_wt_long$date_time>START & serf_wt_long$date_time<=FINISH & 
                   serf_wt_long$plotID %in% c("S3", "S4"),"wt_depth_mm"]
}

serf_wt_long[serf_wt_long$date_time>FINISH & serf_wt_long$plotID %in% c("S3", "S4"), "atS"] <-
  serf_mngt_wide[serf_mngt_wide$outlet_date==FINISH, 5] - 
  serf_wt_long[serf_wt_long$date_time>FINISH & serf_wt_long$plotID %in% c("S3", "S4"), "wt_depth_mm"]


serf_wt_long[serf_wt_long$plotID %in% c("S1", "S6"), "atS"] <-
  dwm_plot[dwm_plot$PlotID=="S1",9] - serf_wt_long[serf_wt_long$plotID %in% c("S1", "S6"), "wt_depth_mm"]

serf_wt_long[serf_wt_long$plotID %in% c("S2", "S5"), "atS"] <-
  dwm_plot[dwm_plot$PlotID=="S2",9] - serf_wt_long[serf_wt_long$plotID %in% c("S2", "S5"), "wt_depth_mm"]


#SAVE DATA HERE
setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA")
save(serf_wt_long, file = "serf_wt.Rda")
write.table(serf_wt_long, file = "serf_wt", quote = FALSE, sep = "\t", row.names = FALSE)
write.csv(serf_wt_long, file = "serf_wt.txt", quote = FALSE, row.names = FALSE)


qplot(date_time, atS, data = serf_wt_long, colour = plotID, geom = "point", size = I(0.75))


qplot(date_time, atS, data = serf_wt_long[serf_wt_long$year==2011,], colour = plotID, geom = "point", size = I(0.75))
ggsave(filename = "2011_SERF_IA_wt_diff_plots_1-6.png", width = 20, height = 12, dpi = 72)
qplot(date_time, atS, data = serf_wt_long[serf_wt_long$year==2012,], colour = plotID, geom = "point", size = I(0.75))
ggsave(filename = "2012_SERF_IA_wt_diff_plots_1-6.png", width = 20, height = 12, dpi = 72)
qplot(date_time, atS, data = serf_wt_long[serf_wt_long$year==2013,], colour = plotID, geom = "point", size = I(0.75))
ggsave(filename = "2013_SERF_IA_wt_diff_plots_1-6.png", width = 20, height = 12, dpi = 72)
qplot(date_time, atS, data = serf_wt_long[serf_wt_long$year==2014,], colour = plotID, geom = "point", size = I(0.75))
ggsave(filename = "2014_SERF_IA_wt_diff_plots_1-6.png", width = 20, height = 12, dpi = 72)
qplot(date_time, atS, data = serf_wt_long[serf_wt_long$year==2015,], colour = plotID, geom = "point", size = I(0.75))
ggsave(filename = "2015_SERF_IA_wt_diff_plots_1-6.png", width = 20, height = 12, dpi = 72)


#
##
###

# >>> STOP <<< #

qplot(timestamp, wt_above, data = plot_data[plot_data$year==2011,], geom = "line", colour = plotID) +
  ggtitle("Water Table Height above Outlet/Tile") +
  xlab("") +
  ylab("Height above tile/outlet, mm")
ggsave("DPAC_2011.png", width = 20, height = 12, dpi = 72)

qplot(timestamp, atNE, data = dpac_wt_wide[which(year(dpac_wt_wide$timestamp)==2012),])

qplot(timestamp, atSE, data = dpac_wt_wide[which(year(dpac_wt_wide$timestamp)==2012),], 
      geom = "line", colour = I("blue"), size = I(1))
qplot(timestamp, atSE, data = dpac_wt_wide[which(year(dpac_wt_wide$timestamp)==2012 & dpac_wt_wide$atSE>-600),], 
      geom = "point", colour = I("blue"), size = I(1))
qplot(timestamp, atSE, data = dpac_wt_wide[which(year(dpac_wt_wide$timestamp)==2012),], 
      geom = "line", colour = I("blue"), size = I(1), ylim = c(-700,500))


#
##
###


# >>> STOP <<< #

#extract STJOHNS dwm management data
stjo_mngt <- dwm_mngt[dwm_mngt$siteID=="STJOHNS",]
#get read of redandunt columns (years when outlet hight was not changed)
serf_mngt <- serf_mngt[!is.na(serf_mngt$outlet_date),]
#assign time (12:00) in accordance to time zone and convert to UTC then
serf_mngt$outlet_date <- with_tz(force_tz(update(serf_mngt$outlet_date, hour=12),
                                          "America/Chicago"),"UTC")
#make the data wide again 
serf_mngt_wide <- spread(serf_mngt, key = plotID, value = outletdepth_mm)
serf_mngt_wide$outletdepth <- NULL
#add a dummy entry for outlet hight before April 2011 
serf_mngt_wide <- rbind(serf_mngt_wide, serf_mngt_wide[2,])
serf_mngt_wide$outlet_date[dim(serf_mngt_wide)[1]] <- update(serf_mngt_wide$outlet_date[dim(serf_mngt_wide)[1]], 
                                                             year=2010, month=12, day=31)
#order by date
serf_mngt_wide <- serf_mngt_wide[order(serf_mngt_wide$outlet_date),]



