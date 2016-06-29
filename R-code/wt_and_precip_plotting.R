###
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(scales)
library(lubridate)


START <- serf_weather$date_time[1]
END <- serf_weather$date_time[length(serf_weather$date_time)]

#create data that is same length (in terms of dates) as precipitation 
plot_serf_wt <- serf_wt_long[serf_wt_long$date_time >= START & serf_wt_long$date_time <= END,]
plot_serf_wt$type <- "water table, mm"

#combined Weather and Water Table data
a5 <- serf_weather
a5$plotID <- NA
a5$type <- "precipitation, mm"
a5 <- a5[,c("YEAR","date_time","plotID","prec_mm","tempair_C","type")]
names(a5) <- names(plot_serf_wt)
a5 <- rbind(a5,plot_serf_wt)

#combined Weather, Water Table and Tile Flow data
a3 <- serf_df_long[serf_df_long$date_time >= START & serf_df_long$date_time <= END,]
a3$type <- "drain flow, mm"
a3$tempair_C <- NA
a3 <- a3[,c("YEAR","date_time","plotID","Discharge_mm","tempair_C","type")]
names(a3) <- names(plot_serf_wt)
a3 <- rbind(a5,a3)
names(a3)[names(a3)=="wt_depth_mm"] <- "meas_mm"

a3$plotID <- as.factor(a3$plotID)
a3$year <- as.factor(a3$year)
a3$type <- as.factor(a3$type)
SERF_sld <- a3
setwd("~/CSCAP/Data/SERF_IA")
save(SERF_sld, file = "SERF_sld.Rda")

a2011 <- a5[a5$year==2011,]
a2012 <- a5[a5$year==2012,]
a2013 <- a5[a5$year==2013,]
a2014 <- a5[a5$year==2014,]
a2015 <- a5[a5$year==2015,]

a <- a2015

star
#EURICA!!! 
p <- ggplot(a, aes(x=date_time, y=wt_depth_mm, colour = plotID)) + 
  facet_grid(type ~ ., scales = "free") + 
  theme(axis.title = element_text(size = 14, face = "plain"), 
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "plain"),
        legend.text = element_text(size = 10),
        # strip.background = element_rect(fill = "lemonchiffon1"),
        strip.text = element_text(size = 14, face = "plain")) +
  scale_color_discrete(name = "Plot ID") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  geom_point(data = subset(a, type == "water table, mm"), size=0.75) +
  geom_step(data = subset(a, type == "precipitation, mm"), colour = "skyblue3", size = I(1.25)) +
  scale_y_reverse() + ylab("") + xlab("")
g <- ggplotGrob(p)
g$heights[[3]] <- unit(0.25, "null")

grid.newpage()
grid.draw(g)
ggsave("DPAC_plot_1-6_wt&prec_2015.png", g, width = 20, height = 12, dpi = 72)
###





b2011 <- a3[a3$year==2011,]
b2012 <- a3[a3$year==2012,]
b2013 <- a3[a3$year==2013,]
b2014 <- a3[a3$year==2014,]
b2015 <- a3[a3$year==2015,]

i <- 2015
b <- get(paste("b", i, sep = ""))
startTime <- as.POSIXct(ymd_hms(paste(i,"1","1", sep = "-"), tz = "UTC", truncated = 3)) 
endTime <- startTime + years(1)
start.end.time.x <- with_tz(c(startTime, endTime), "UTC")

Precip_hourly <- 
  ggplot(data = b[b$type=="precipitation, mm",], aes(x = date_time, y = meas_mm)) + 
  geom_step(color = "skyblue2", size = 1) +
  #ggtitle(paste("Precipitation at SERF\n",i)) + 
  #xlab("Date") + 
  xlab("") + 
  ylab("Precipitation (mm)") +
  scale_y_reverse() +
  scale_x_datetime(limits = start.end.time.x,
                   breaks = date_breaks("2 months"),
                   labels = NULL) +
  theme(axis.ticks.x = element_blank(),
        axis.text = element_text(size = 12),
        text = element_text(size = 16),
        panel.grid.minor.y = element_blank())
  
  

WaterTable_hourly <-
  ggplot(data = b[b$type=="water table, mm",], aes(x = date_time, y = meas_mm, colour = plotID)) + 
  geom_point(size = 0.75, alpha = 0.5) +
  #ggtitle(paste("Water Table Depth below ground at SERF\n",i)) + 
  #xlab("Date") + 
  xlab("") +
  ylab("Water Table Depth (mm)") +
  scale_y_reverse() +
  scale_x_datetime(limits = start.end.time.x,
                   breaks = date_breaks("2 months"),
                   labels = NULL) +
  theme(axis.ticks.x = element_blank(),                           #remove ticks from x axis
        axis.text = element_text(size = 12),                      #adjust axis text size (font size of values)
        legend.title = element_text(size = 12, face = "bold"),    #adjust font size of the legends header
        legend.text = element_text(size = 10),                    #adjust fond size of the legends text
        text = element_text(size = 16)) +                         #format all text that isn't specifically formatted elsewhere
  guides(colour = guide_legend(override.aes = list(size = 3))) +  #increase size of symbol in legends
  scale_color_discrete(name = "Plot ID")                          #rename legend's header
  



DrainFlow_hourly <-
  ggplot(data = b[b$type=="drain flow, mm",], aes(x = date_time, y = meas_mm, colour = plotID)) + 
  geom_path(size = 1, alpha = 0.75) +
  #ggtitle(paste("Drain Flow at SERF\n",i)) + 
  #xlab("Date") + 
  xlab("") +
  ylab("Drain Flow (mm)") +
  scale_x_datetime(limits = start.end.time.x,
                   breaks = date_breaks("month"),
                   labels = date_format("%b")) +
  theme(axis.text = element_text(size = 12),
        legend.position = "none",
        text = element_text(size = 16),
        panel.grid.minor.x = element_blank())                   #remove the vertical minor grid lines 




##grid.arrange(Precip_hourly, WaterTable_hourly, DrainFlow_hourly, ncol = 1)



#fix the width of the plots
gPrecip <- ggplotGrob(Precip_hourly)
gWaterTable <- ggplotGrob(WaterTable_hourly)
gDrain <- ggplotGrob(DrainFlow_hourly)

gPrecip$widths <- gWaterTable$widths
gDrain$widths <- gWaterTable$widths

gPrecip$heights <- gWaterTable$heights
gDrain$widths <- gWaterTable$widths

grid.newpage()
grid.arrange(gPrecip, gWaterTable, gDrain, heights=c(0.2, 0.4, 0.4), ncol = 1, 
             top = "SERF 2015\n (hourly measurements)")
#
  
  













#ggplot(a, aes(x=date_time, y=wt_depth_mm, colour = plotID)) + 
#  facet_grid(type ~ ., scales = "free", space = "fixed") + 
#  geom_point(size=0.75) +
#  scale_y_reverse() + ylab("") + xlab("Date")






# qplot(date_time, wt_depth_mm, data = plot_serf_wt, colour = plotID, geom = "point", size = I(0.75)) +
#   scale_y_reverse()
# 
# qplot(date_time, prec_mm, data = serf_weather, geom = "step", colour = I("skyblue4")) + scale_y_reverse()
# 
# 
# 
# 
# p1 <- qplot(date_time, wt_depth_mm, data = plot_serf_wt, colour = plotID, geom = "point", size = I(0.75)) +
#   scale_y_reverse() + labs(x="Date", y = "aa") + theme(legend.position = "none")
# p2 <- qplot(date_time, prec_mm, data = serf_weather, geom = "step", colour = I("skyblue4")) + 
#   scale_y_reverse() + labs(x=NULL, y = "bb")  
# grid.arrange(p2, p1, nrow=2, heights = c(0.25, 0.75), top="SERF") 
# 
# #removing legends 
# #g1 <- ggplotGrob(p1)
# #g1[["grobs"]][[which(g1$layout$name=="guide-box")]][["grobs"]] <- NULL
# grid.arrange(p2, g1, heights = c(0.25, 0.75))
# 
# #make both the same width << you need to remove legends ahead
# grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(data = plot_serf_wt, aes(x=date_time, y=wt_depth_mm, color=plotID)) +
#   geom_point(size = 0.75, alpha = 0.75) +
#   scale_y_reverse() +
#   geom_step(data = serf_weather, aes(x=date_time, y=prec_mm), colour="#4A708B")
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# grid.newpage()
# 
# #two plots
# p1 <- ggplot(data = plot_serf_wt, aes(x=date_time, y=wt_depth_mm, color=plotID)) +
#   geom_point(size = 0.75, alpha = 0.75) +
#   scale_y_reverse() 
# p2 <- ggplot(data = serf_weather, aes(x=date_time, y=prec_mm)) +
#   geom_step(colour=I("skyblue4")) +
#   scale_y_reverse()
# 
# #extract gtable
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# #overlap the panel of 2nd plot on that of 1st plot
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
#                      pp$l, pp$b, pp$l)
# 
# # axis tweaks
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # draw it
# grid.draw(g)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# g1_grob <- ggplotGrob(g1)
# g2_grob <- ggplotGrob(g2)
# 
# #xmin <- min(plot_serf_wt$date_time); xmax <- max(plot_serf_wt$date_time)
# #ymin <- round(max(plot_serf_wt$wt_depth_mm),-2)+200; ymax <- round(max(plot_serf_wt$wt_depth_mm),-2)+600
# #g1 + annotation_custom(grob = g2_grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
# g1 + annotation_custom(grob = g2_grob, ymin = 200, ymax = 600)
# 
