#libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
#read in water level
#setwd("//babylon/visitors/rickard/Well analysis/Ranchbot")
#setwd("C:/Users/ricka/OneDrive/Documents/JLDP-report-analysis")
PixleyWell<-read.csv("data/raw/Pixley/Pixley_ Tulare_ Capinero creek Well 20241201-20250214.csv")
head(PixleyWell)
#Change to date format
PixleyWell$Date<-format(PixleyWell$Date.and.Time,format="%Y-%m-%d")
PixleyWell$Date<-as.Date(PixleyWell$Date)
PixleyWell$Date.and.Time<-as.POSIXct(PixleyWell$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
PixleyWell<-PixleyWell[PixleyWell$Date.and.Time>="2024-12-12",]
DailyPixleyWell<-aggregate(ft..below.ground.~Date,PixleyWell, FUN=mean)
#read in E5 rainfall
PixleyRain<-read.csv("data/raw/Pixley/Pixley_Tulare Rain Gauge Rain Gauge 20241201-20250214.csv")
head(PixleyRain)

#change to date format
PixleyRain$Date<-as.Date(PixleyRain$Date,format="%Y-%m-%d (%a)")
#merge
Pixleyall<-merge(PixleyRain,DailyPixleyWell,by="Date")

#Plot
scaleFUN <- function(x) sprintf("%.1f", x)
g1 <- ggplot(PixleyRain, aes(x=Date)) +
  geom_bar(aes(y=Rain..in.), stat="identity", size=.1, fill="#69b3a2", color="black",alpha=0.4) +
  ggtitle("Rainfall and Groundwater Depth at Pixley Well (2025 WY)")+theme_bw()+ 
  scale_y_reverse(labels=scaleFUN)+ylab("Rainfall (Inches)")+
  theme(title =element_text(size=14, face='bold'),
        axis.text.x     = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x    = element_blank(),
        axis.ticks.x    = element_blank())

g2 <- ggplot(PixleyWell, aes(x=Date.and.Time)) +
  geom_line( aes(y=ft..below.ground.), size=1) +
  ylab("Groundwater Depth")+
  theme_bw()+ scale_y_reverse()+
  theme(axis.title.y = element_text(size=14, face='bold'),
        legend.title    = element_blank(),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=14))
g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
grid.arrange(g1,g2, ncol = 1, heights = c(1, 3))

