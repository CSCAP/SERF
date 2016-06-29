setwd("C:/Users/gio/Documents/CSCAP/Data/SERF_IA")

S1_row <- read.csv("S1_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S2_row <- read.csv("S2_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S3_row <- read.csv("S3_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S4_row <- read.csv("S4_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S5_row <- read.csv("S5_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S6_row <- read.csv("S6_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S7_row <- read.csv("S7_SERF Water Table Depth 07 to 15 _06.16.2016.csv")
S8_row <- read.csv("S8_SERF Water Table Depth 07 to 15 _06.16.2016.csv")

#OR you can use full path 
#example: ("C:\\Users\\gio\\Documents\\CSCAP\\Data\\SERF_IA\\S1_SERF Water Table Depth 07 to 15 _06.16.2016.csv")

library(tidyr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))


#fix timestamps
for(i in 1:8) {
  nam <- paste("S",i,"_row", sep = "")
  a <- get(nam)
  a$timestamp <- paste(a[,1], a[,2], sep = " ")
  a$timestamp <- parse_date_time(a$timestamp, c("mdY HMS", "mdY HM"))
  a$timestamp <- with_tz((a$timestamp + hours(6)), tzone = "America/Chicago")
  a$timestamp <- round_date(a$timestamp, "minute")
  a$year <- year(a$timestamp)
  a$plotID <- paste("S", i, sep = "")
  #a$wt_depth_mm <- as.numeric(a$Level.Below.Ground.Surface..m.)*1000
  a$wt_depth_mm <- (as.numeric(as.character(a$Level.Below.Ground.Surface..m.)))*1000
  a <- a[,c("year","timestamp","plotID","wt_depth_mm","Date", "Time")]
  assign(nam,a)
}


#calculate time diff
for(i in 1:8) {
  nam <- paste("S",i,"_row", sep = "")
  a <- get(nam)[,"timestamp"]
  a <- diff(a)
  nam <- paste("S",i,"_diff", sep = "")
  assign(nam, a)
}


#check if diff is 0 (indicator of duplicated dates)
serf_time_diffs <- data.frame("plotID"=rep(NA,8),"dup_date"=rep(NA,8), "count"=rep(NA,8), stringsAsFactors = FALSE)
for(i in 1:8) {
  nam <- paste("S",i,"_diff", sep = "")
  a <- get(nam)
  #if(any(unique(a)==0)) cat(paste(nam,"contains duplicated date(s); "))
  serf_time_diffs[i,1] <- paste("S",i, sep = "")
  serf_time_diffs[i,2] <- any(unique(a)==0)
  serf_time_diffs[i,3] <- sum(a==0)
}
serf_time_diffs[serf_time_diffs$dup_date==TRUE,]


#get section of data that contains duplicated dates (to choose which row to remove)
nam <- serf_time_diffs[serf_time_diffs$dup_date==TRUE,1]
serf_time_diffs_col <- data.frame(plotid = character(), row = as.numeric(character()))
for(i in 1:length(nam)) {
  plotid <- paste(nam[i],"_diff", sep = "")
  a <- get(plotid)
  b <- cbind(plotid = nam[i], row = which(a==0))
  serf_time_diffs_col <- rbind(serf_time_diffs_col,b)
}
serf_time_diffs_col$row <- as.numeric(as.character(serf_time_diffs_col$row))


serf_time_diffs_row <- data.frame(rep(character(),6))
for(i in 1:nrow(serf_time_diffs_col)) {
  a <- get(paste(serf_time_diffs_col[i,1],"_row", sep = ""))[(serf_time_diffs_col[i,2]+(-1:2)),]
  serf_time_diffs_row <- rbind(serf_time_diffs_row, a, NA)
}
serf_time_diffs_row

#check wt depth values before and after rep dates to choose correct date to remove
#there were 11 duplicated dates in 5 plots
#after revieing the data following rows were decided to remove froom each plot
# S3 > 10471, 31187, 31188
# S4 > 2, 13394?, 19251
qplot(x = timestamp, y = wt_depth_mm ,data = S4_row[13390:13399,])
# S4 > 13394 there are two 11 am readings and no 12 am > 
# one reading was shifted forward by an hour after examning a plot of the data (10 hr time interval)
# S5 > 11770, 23376, 29863
# S7 > 17495
# S8 > 32426

S4_row[13394,2] <- S4_row[13394,2] + hours(1)

S3_row <- S3_row[-c(10471, 31187, 31188),]
S4_row <- S4_row[-c(2,19251),]
S5_row <- S5_row[-c(11770, 23376, 29863),]
S7_row <- S7_row[-17495,]
S8_row <- S8_row[-32426,]


## FROM HERE THE DATA CLEANING IS MOSTELY MANUAL ##
## objects and their constituents are specified manually in the code most of the time ## 



#>>>>START
a <- S8_row
a$newTime <- with_tz(a$timestamp, tz = "UTC")
head(a,10)


a$newTime <- a$newTime - minutes(16)
head(a, 10)

#>>>>REPS
(x <- unique(minute(a$newTime)))
(y <- head(which(minute(a$newTime)==x[6]),1))
a[y+(-1:3),]

a$newTime[y:length(a$newTime)] <- a$newTime[y:length(a$newTime)] - minutes(26)
a[y+(-1:3),]

a[y,"newTime"] <- NA
a[y+(-1:3),]
#


unique(diff(a$newTime))
unique(diff(a$newTime)) %%30

S8 <- a[,c("year","newTime", "plotID", "wt_depth_mm")]
names(S8)[2] <- "date_time"
head(S8)

qplot(date_time, wt_depth_mm, data = S8, geom = "point") + scale_y_reverse("Depth Below Ground, mm")
qplot(timestamp, wt_depth_mm, data = S8_row, geom = "point") + scale_y_reverse("Depth Below Ground, mm")

#save each plot wt as an individual txt file in C:\Users\gio\Documents\CSCAP\Data\SERF_IA
write.table(S8, choose.files(), sep='\t', quote = FALSE, row.names = FALSE)


serf_wt <- rbind(S1,S2,S3,S4,S5,S6,S7,S8)
serf_wt$plotID <- as.factor(serf_wt$plotID)
serf_wt$year <- as.factor(serf_wt$year)
str(serf_wt)
serf_wt <- serf_wt[complete.cases(serf_wt),]



#SAVE COMPILED DATA
save(serf_wt, file = "serf_wt_2007-2015.Rda")
write.table(serf_wt, "serf_wt_2007-2015", sep='\t', quote = FALSE, row.names = FALSE)
write.csv(serf_wt, "serf_wt_2007-2015.txt", quote = FALSE, row.names = FALSE)

#SAVE INDIVIDUAL PLOT DATA (DATES/TIME CLEANED BUT WITH FEW NAs)
for(i in 1:8) {
  a <- get(paste("S",i, sep = ""))
  namR <- paste("serf_plot", i, "wt_2007-2015.Rda", sep = "_")
  save(a, file = namR)
  nam <- paste("serf_plot", i, "wt_2007-2015", sep = "_")
  write.table(a, file = nam, sep='\t', quote = FALSE, row.names = FALSE)
  namT <- paste("serf_plot", i, "wt_2007-2015.txt", sep = "_")
  write.csv(a, file = namT, quote = FALSE, row.names = FALSE)
}

#SAVE INDIVIDUAL PLOT DATA (DATES/TIME NOT CLEANED)
for(i in 1:8) {
  a <- get(paste("S",i,"_row", sep = ""))
  nam <- paste("ROW_serf_plot", i, "wt_2007-2015.Rda", sep = "_")
  save(a, file = nam)
}









qplot(date_time, wt_depth_mm, data = serf_wt, 
      colour = plotID, geom = "point") + scale_y_reverse()
ggsave(filename = "2007-2015_SERF_IA_wt.png", width = 20, height = 12, dpi = 72)

qplot(date_time, wt_depth_mm, data = serf_wt[serf_wt$year %in% c(2011:2015),], 
      colour = plotID, geom = "point") + scale_y_reverse()
ggsave(filename = "2011-2015_SERF_IA_wt.png", width = 20, height = 12, dpi = 72)

qplot(date_time, wt_depth_mm, data = serf_wt[serf_wt$year %in% 2015,], 
      colour = plotID, geom = "point", size = I(1)) + scale_y_reverse()
ggsave(filename = "2015_SERF_IA_wt.png", width = 20, height = 12, dpi = 72)

qplot(date_time, wt_depth_mm, data = 
        serf_wt[serf_wt$year %in% c(2011:2015) & serf_wt$plotID %in% c("S1", "S2", "S3", "S4", "S5", "S6"),], 
      colour = plotID, geom = "point", size = I(1)) + scale_y_reverse()
ggsave(filename = "2011-2015_SERF_IA_wt_plots_1-6.png", width = 20, height = 12, dpi = 72)

qplot(date_time, wt_depth_mm, data = 
        serf_wt[serf_wt$year %in% 2014 & serf_wt$plotID %in% c("S1", "S2", "S3", "S4", "S5", "S6"),], 
      colour = plotID, geom = "point", size = I(1)) + scale_y_reverse()
ggsave(filename = "2014_SERF_IA_wt_plots_1-6.png", width = 20, height = 12, dpi = 72)






###
##
#

#calculate time diff again after above data correction
for(i in 1:8) {
  nam <- paste("S",i, sep = "")
  a <- get(nam)[,"date_time"]
  a <- diff(a)
  nam <- paste("S",i,"_diff", sep = "")
  assign(nam, a)
}


#find the non-regular differences in time (when time interval is not proportional to 30 min)
for(i in 1:8) {
  nam <- paste("S",i,"_diff", sep = "")
  a <- get(nam)
  b1 <- unique(a)[order(unique(a))][(unique(a)[order(unique(a))] %% 30) != 0]
  b2 <- sum(a==b1)
  assign(paste("S",i,"odd",sep = ""),b)
}


odd1 <- data.frame(character())
for(i in 1:length(S1odd)) {
  a <- S1_row[which(S1_diff==S1odd[i])+(-1:1),]
  odd1 <- rbind(odd1, a)
}
odd1 <- odd1[order(odd1$timestamp),]





####
##
#

# ploting data for visual examination 
qplot(x=timestamp, y=wt_depth_mm, data=S1_row[S1_row$year==2010,], geom = "point") + 
  scale_y_reverse("Depth Below Ground, mm")

# order unique differences 
unique(S1_diff)[order(unique(S1_diff))]
which((S1_diff)<60)
# find wich one are different by 60 min (hourly appart)
unique(S1_diff)[order(unique(S1_diff))][(unique(S1_diff)[order(unique(S1_diff))] %% 30) != 0]


