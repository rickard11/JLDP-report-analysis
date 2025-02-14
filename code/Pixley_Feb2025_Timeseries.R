#libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
#read in E5 water level
setwd("//babylon/visitors/rickard/Well analysis/Ranchbot")
PixleyWell<-read.csv("Pixley_ Tulare_ Capinero creek Well 20250101-20250213.csv")
head(PixleyWell)
#Change to date format
PixleyWell$Date<-format(PixleyWell$Date.and.Time,format="%Y-%m-%d")
PixleyWell$Date<-as.Date(PixleyWell$Date)
DailyPixleyWell<-aggregate(ft..below.ground.~Date,PixleyWell, FUN=mean)
DailyPixleyWell<-DailyPixleyWell[DailyPixleyWell$Date!="2023-10-19",] #Remove first date because measured during deployment
head(DailyPixleyWell)
#read in E5 rainfall
PixleyRain<-read.csv("Pixley_Tulare Rain Gauge Rain Gauge 20250101-20250213.csv")
head(PixleyRain)

#change to date format
PixleyRain$Date<-as.Date(PixleyRain$Date,format="%Y-%m-%d (%a)")
#merge
E5all<-merge(PixleyRain,DailyPixleyWell,by="Date")
E5all
#Plot
scaleFUN <- function(x) sprintf("%.1f", x)
g1 <- ggplot(E5all, aes(x=Date)) +
  geom_bar(aes(y=Rain..in.), stat="identity", size=.1, fill="#69b3a2", color="black",alpha=0.4) +
  ggtitle("Rainfall and Groundwater Depth at Pixley Well (2025 WY to date)")+theme_bw()+ 
  scale_y_reverse(labels=scaleFUN)+ylab("Rainfall (Inches)")+
  theme(title =element_text(size=15, face='bold'),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=13),
        axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g2 <- ggplot(E5all, aes(x=Date)) +
  geom_line( aes(y=ft..below.ground.), size=1.5) +
  ylab("Groundwater Depth")+
  theme_bw()+ scale_y_reverse()+
  theme(axis.title.y = element_text(size=14),
        legend.title    = element_blank(),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=14))
g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
grid.arrange(g1,g2, ncol = 1, heights = c(1, 3))

