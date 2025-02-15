library(ggplot2)
library(dplyr)
#install.packages("plotly")
#install.packages("hrbrthemes")
library(plotly)
library(hrbrthemes)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

# Load rainfall datasets from Ranchbot
E2R<-read.csv("data/raw/other CalGro Sites/Rain/Dangermond- Escondido 2 Rain Gauge 20240101-20250214.csv")
E2R$Date<-as.POSIXct(E2R$Date, format="%Y-%m-%d")
E2R<-E2R[E2R$Date>="2024-02-01",]
E2R<-E2R %>% arrange(E2R$Date)
E2R$Location<-"Escondido 2"
E2R$cumrain<-cumsum(E2R$Rain..in.)

E3R<-read.csv("data/raw/other CalGro Sites/Rain/Dangermond- Escondido 3 Rain Gauge 20240101-20250214.csv")
E3R$Date<-as.POSIXct(E3R$Date, format="%Y-%m-%d")
E3R<-E3R[E3R$Date>="2024-02-01",]
E3R<-E3R %>% arrange(E3R$Date)
E3R$Location<-"Escondido 3"
E3R$cumrain<-cumsum(E3R$Rain..in.)

O5R<-read.csv("data/raw/other CalGro Sites/Rain/Dangermond- Oaks 5 Rain Gauge 20240101-20250214.csv")
O5R$Date<-as.POSIXct(O5R$Date, format="%Y-%m-%d")
O5R<-O5R %>% arrange(O5R$Date)
O5R$Location<-"Oaks 5"
O5R$cumrain<-cumsum(O5R$Rain..in.)

T6R<-read.csv("data/raw/other CalGro Sites/Rain/Dangermond- Tinta 6 Rain Gauge 20240101-20250214.csv")
T6R$Date<-as.POSIXct(T6R$Date, format="%Y-%m-%d")
T6R<-T6R %>% arrange(T6R$Date)
T6R$Location<-"Tinta 6"
T6R$cumrain<-cumsum(T6R$Rain..in.)

T10R<-read.csv("data/raw/other CalGro Sites/Rain/Dangermond- Tinta 10 Rain Gauge 20240101-20250214.csv")
T10R$Date<-as.POSIXct(T10R$Date, format="%Y-%m-%d")
T10R<-T10R %>% arrange(T10R$Date)
T10R$Location<-"Tinta 10"
T10R$cumrain<-cumsum(T10R$Rain..in.)

LPR<-read.csv("data/raw/other CalGro Sites/Rain/Las Piletas Cooper Rain Gauge 20241205-20250214.csv")
LPR$Date<-as.POSIXct(LPR$Date, format="%Y-%m-%d")
LPR<-LPR %>% arrange(LPR$Date)
LPR$Location<-"Las Piletas"
LPR$cumrain<-cumsum(LPR$Rain..in.)

WCR<-read.csv("data/raw/other CalGro Sites/Rain/Dangermond- Wood Canyon Rain Gauge 20241001-20250214.csv")
WCR$Date<-as.POSIXct(WCR$Date, format="%Y-%m-%d")
WCR<-WCR %>% arrange(WCR$Date)
WCR$Location<-"Wood Canyon"
WCR$cumrain<-cumsum(WCR$Rain..in.)
#merge all together so I can plot with a for loop
raingauge<-rbind(E2R,E3R,O5R,T6R,LPR,WCR)

# Load well depth datasets from ranchbot
E2W<-read.csv("data/raw/other CalGro Sites/Well/Dangermond- Escondido 2 Well 20240101-20250214.csv")
E2W$Date.and.Time<-as.POSIXct(E2W$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
E2W<-E2W[E2W$Date.and.Time>="2024-02-01",]
E2W$Date<-format(E2W$Date.and.Time,format="%Y-%m-%d")
E2W$Date<-as.POSIXct(E2W$Date,format="%Y-%m-%d")
E2W<-aggregate(ft..below.ground.~Date,E2W,FUN=mean)

E3W<-read.csv("data/raw/other CalGro Sites/Well/Dangermond- Escondido 3 Well 20240101-20250214.csv")
E3W$Date.and.Time<-as.POSIXct(E3W$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
E3W<-E3W[E3W$Date.and.Time>="2024-02-01",]
E3W$Date<-format(E3W$Date.and.Time,format="%Y-%m-%d")
E3W$Date<-as.POSIXct(E3W$Date,format="%Y-%m-%d")
E3W<-aggregate(ft..below.ground.~Date,E3W,FUN=mean)

O5W<-read.csv("data/raw/other CalGro Sites/Well/Dangermond- Oaks 5 Well 20240101-20250214.csv")
O5W$Date.and.Time<-as.POSIXct(O5W$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
O5W<-O5W[O5W$Date.and.Time>="2024-02-01",]
O5W$Date<-format(O5W$Date.and.Time,format="%Y-%m-%d")
O5W$Date<-as.POSIXct(O5W$Date,format="%Y-%m-%d")
O5W<-aggregate(ft..below.ground.~Date,O5W,FUN=mean)

T6W<-read.csv("data/raw/other CalGro Sites/Well/Dangermond- Tinta 6 Well 20240101-20250214.csv")
T6W$Date.and.Time<-as.POSIXct(T6W$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
T6W<-T6W[T6W$Date.and.Time>="2024-02-01",]
T6W$Date<-format(T6W$Date.and.Time,format="%Y-%m-%d")
T6W$Date<-as.POSIXct(T6W$Date,format="%Y-%m-%d")
T6W<-aggregate(ft..below.ground.~Date,T6W,FUN=mean)

T10W<-read.csv("data/raw/other CalGro Sites/Well/Dangermond- Tinta 10 Well 20240101-20250214.csv")
T10W$Date.and.Time<-as.POSIXct(T10W$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
T10W<-T10W[T10W$Date.and.Time>="2024-02-01",]
T10W$Date<-format(T10W$Date.and.Time,format="%Y-%m-%d")
T10W$Date<-as.POSIXct(T10W$Date,format="%Y-%m-%d")
T10W<-aggregate(ft..below.ground.~Date,T10W,FUN=mean)

LPW<-read.csv("data/raw/other CalGro Sites/Well/Las Piletas  Well 20241205-20250214.csv")
LPW$Date.and.Time<-as.POSIXct(LPW$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
LPW<-LPW[LPW$Date.and.Time>="2024-02-01",]
LPW$Date<-format(LPW$Date.and.Time,format="%Y-%m-%d")
LPW$Date<-as.POSIXct(LPW$Date,format="%Y-%m-%d")
LPW<-aggregate(ft..below.ground.~Date,LPW,FUN=mean)

WCW<-read.csv("data/raw/other CalGro Sites/Well/Dangermond- Wood canyon Well 20240101-20250214.csv")
WCW$Date.and.Time<-as.POSIXct(WCW$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
WCW<-WCW[WCW$Date.and.Time>="2024-02-01",]
WCW$Date<-format(WCW$Date.and.Time,format="%Y-%m-%d")
WCW$Date<-as.POSIXct(WCW$Date,format="%Y-%m-%d")
WCW<-aggregate(ft..below.ground.~Date,WCW,FUN=mean)

#Oaks 2, Jalama vaqueros and Tinta 2 are gone for now.
Welldepth<-rbind(E2W,E3W,O5W,T6W,LPW,WCW)
Welldepth<-Welldepth[Welldepth$Date.and.Time>="2024-02-01",]

#################################################################################
#####Calculate groundwater recharge

#First sum all the rain for the year an put it in a dataframe
rainsum<-aggregate(Rain..in.~Location,raingauge,FUN=sum)
rainsum

# Mostly looks good, but there are errors in Tinta 10.
# Get max and min of well level- this will be trickier if there are outliers- should remove in plotting function






###Testing hydrograph

#E2
g1 <- ggplot(E2R, aes(Date, Rain..in.)) +
  geom_bar(stat = 'identity', fill = "lightblue", color="black") +
  theme_bw() +
  ylab("Precip.") +
  labs(title = "Escondido 2") +
  scale_y_reverse(labels = label_number(accuracy = 0.1))+
  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g2 <- ggplot(E2W, aes(Date, ft..below.ground.))+
  geom_line() +
  ylab("Well Depth") +
  theme_bw() +scale_y_continuous(trans = "reverse")
g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
grid.arrange(g1, g2, ncol = 1, heights = c(1, 3))


#E3
g1 <- ggplot(E3R, aes(Date, Rain..in.)) +
  geom_bar(stat = 'identity', fill = "lightblue", color="black") +
  theme_bw() +
  ylab("Precip.") +
  labs(title = "Escondido 3") +
  scale_y_reverse(labels = label_number(accuracy = 0.1))+
  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g2 <- ggplot(E3W, aes(Date, ft..below.ground.))+
  geom_line() +
  ylab("Well Depth") +
  theme_bw() +scale_y_continuous(trans = "reverse")
g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
grid.arrange(g1, g2, ncol = 1, heights = c(1, 3))

#O5
g1 <- ggplot(O5R, aes(Date, Rain..in.)) +
  geom_bar(stat = 'identity', fill = "lightblue", color="black") +
  theme_bw() +
  ylab("Precip.") +
  labs(title = "Oaks 5") +
  scale_y_reverse(labels = label_number(accuracy = 0.1))+
  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g2 <- ggplot(O5W, aes(Date, ft..below.ground.))+
  geom_line() +
  ylab("Well Depth") +
  theme_bw() +scale_y_continuous(trans = "reverse")
g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
grid.arrange(g1, g2, ncol = 1, heights = c(1, 3))

#T6
g1 <- ggplot(T6R, aes(Date, Rain..in.)) +
  geom_bar(stat = 'identity', fill = "lightblue", color="black") +
  theme_bw() +
  ylab("Precip.") +
  labs(title = "Tinta 6") +
  scale_y_reverse(labels = label_number(accuracy = 0.1))+
  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g2 <- ggplot(T6W, aes(Date, ft..below.ground.))+
  geom_line() +
  ylab("Well Depth") +
  theme_bw() +scale_y_continuous(trans = "reverse")
g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth
grid.arrange(g1, g2, ncol = 1, heights = c(1, 3))


#Las Piletas
g6 <- ggplot(LPR, aes(Date, Rain..in.)) +
  geom_bar(stat = 'identity', fill = "lightblue", color="black") +
  theme_bw() +
  ylab("Precip.") +
  labs(title = "Las Piletas") +
  scale_y_reverse(labels = label_number(accuracy = 0.1))+
  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g7 <- ggplot(LPW, aes(Date, ft..below.ground.))+
  geom_line() +
  ylab("Well Depth") +
  theme_bw() +scale_y_continuous(trans = "reverse")
g6 <- ggplot_gtable(ggplot_build(g6))
g7 <- ggplot_gtable(ggplot_build(g7))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g6$widths[2:3] <- maxWidth
g7$widths[2:3] <- maxWidth
grid.arrange(g6, g7, ncol = 1, heights = c(1, 3))

#Wood Canyon
g6 <- ggplot(WCR, aes(Date, Rain..in.)) +
  geom_bar(stat = 'identity', fill = "lightblue", color="black") +
  theme_bw() +
  ylab("Precip.") +
  labs(title = "Wood Canyon") +
  scale_y_reverse(labels = label_number(accuracy = 0.1))+
  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

g7 <- ggplot(WCW, aes(Date, ft..below.ground.))+
  geom_line() +
  ylab("Well Depth") +
  theme_bw() +scale_y_continuous(trans = "reverse")
g6 <- ggplot_gtable(ggplot_build(g6))
g7 <- ggplot_gtable(ggplot_build(g7))
maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])

g6$widths[2:3] <- maxWidth
g7$widths[2:3] <- maxWidth
grid.arrange(g6, g7, ncol = 1, heights = c(1, 3))




