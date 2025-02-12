#Can well water depth change and watershed size be used to estimate base streamflow?

#Libraries and csv's
library(tidyr)
library(ggplot2)
library(lubridate)
library(cowplot)
library(dplyr)
setwd("C:/Users/ricka/Documents/Dangermond/2010 Well depth data/2025 analysis")

#1. input Escondido wells depth 

E2<-read.csv("data/raw/Dangermond- Escondido 2 Well 20240101-20250210.csv")
E2$Site<-"Escondido 2"
E3<-read.csv("data/raw/Dangermond- Escondido 3 Well 20240101-20250210.csv")
E3$Site<-"Escondido 3"
E5<-read.csv("data/raw/Dangermond- Escondido 5 Old Well 20240101-20250210.csv")
E5$Site<-"Escondido 5"
Eall<-rbind(E2,E3,E5) #Combine to one data set
Eall$Date.and.Time<-as.POSIXct(Eall$Date.and.Time,format="%Y-%m-%d %H:%M:%S") #Convert date
ggplot(Eall, aes(x=Date.and.Time))+geom_line(aes(y=ft..below.ground.,color=Site))+theme_bw()+ylim(50,60)
#2. Isolate August (could do for other months in future)

dataAugust<-Eall[Eall$Date.and.Time<="2024-08-31"&Eall$Date.and.Time>="2024-08-01",]
ggplot(dataAugust, aes(x=Date.and.Time))+geom_line(aes(y=ft..below.ground.,color=Site))+theme_bw()+
  ylim(54.5,58.5)
#only use E3 and E5 because E2 is a deep water/ confined aquifer?

#3. Calculate the change in water depth from the begining of the month to the end
dataAugust<-dataAugust[order(dataAugust$Date.and.Time),]
# Filter the data for 'escondido 3' and get the first and last depths
escondido_3_data <- dataAugust %>%
  filter(Site == 'Escondido 3'& !is.na(ft..below.ground.)) 

first_row_3 <- escondido_3_data[1, ]
colnames(first_row_3)<-c("Start_Date","Depth_Start","Site")
last_row_3 <- escondido_3_data[nrow(escondido_1_data), ]
colnames(last_row_3)<-c("End_Date","Depth_End","Site")

#reapeat with Escondido 5
escondido_5_data <- dataAugust %>%
  filter(Site == 'Escondido 5'& !is.na(ft..below.ground.)) 

first_row_5 <- escondido_5_data[1, ]
colnames(first_row_5)<-c("Start_Date","Depth_Start","Site")
last_row_5 <- escondido_5_data[nrow(escondido_5_data), ]
colnames(last_row_5)<-c("End_Date","Depth_End","Site")

Esco5_Change<-merge(first_row_5,last_row_5,by="Site")
Esco3_Change<-merge(first_row_3,last_row_3,by="Site")
Esco_Change<-rbind(Esco5_Charnge,Esco3_Charnge)
Esco_Change$Change_ft<-Esco_Change$Depth_End-Esco_Change$Depth_Start
Esco_Change$Change_in<-Esco_Change$Change_ft*12

#4. Input well lithology

#5. Estimate specific yield for each well
# example specific yeild is 0.1- Escondido is shale which has SY of about 3%
SY<-0.03
#6. Estimate the real waterdepth equvilant from each well
Esco_Change$Water_Equivalent<-Esco_Change$Change_in*SY

#7. Calculate the average from all the wells
Esco_Mean_depletion<-Esco_Change %>% 
  summarise(mean_value=mean(Water_Equivalent))
#This means 0.92 inches of water was depleted across the Escondido watershed (2443 acres) in August

#8. Multiply by escondido watershed size
Acre_to_Inch<-6.273e+6
EscondidoWatershed_Inches<-2443*Acre_to_Inch
EscondidoCuinwater<-Esco_Mean_depletion*EscondidoWatershed_Inches
EscondidoCuftwater<-EscondidoCuinwater/1728
#total of 8,194,585 ft3 were depleted from the escondido watershed on average at 0.1 SY.
# 2,458,376 ft3 at a SY of 0.03

#10. Upload the Fullpipe water depth from vusitu
Fullpipe_depth<-read.csv("data/raw/VuSitu_Log_2024-02-22_11-57-16_Full_pipe_Log_2024-02-22.csv")
str(Fullpipe_depth)
Fullpipe_depth<-Fullpipe_depth[,c(1,17)]
Fullpipe_depth$Date.Time<-as.POSIXct(Fullpipe_depth$Date.Time,format="%m/%d/%Y %H:%M")
Fullpipe_August<-Fullpipe_depth[Fullpipe_depth$Date.Time>="2024-08-01"&Fullpipe_depth<="2024-08-31",]
Fullpipe_August<-Fullpipe_August[!is.na(Fullpipe_August$Depth..ft.),]
mean(Fullpipe_August$Depth..ft.)
#Example:Fullpipe sensor is ~3 inches wide so -.06 is about 1.5 inches.
#11. Use the slope and the pipe diameter to calculate the flow
culvert_diameter_ft<-6.4
culvert_slope_degrees<-7.3

#example estimate for simplicity- will actually calculate later- lets say 14 inches wide
#using simple rectangle calculation, not a semicircle for simplicity
water_depth<-3
water_width<-20
SA<-water_width*water_depth
SA_ft<-SA/12
#Melissa measured about 0.3ft/s at base flow
Base_velocity<-0.5
out_ft3_sec<-SA_ft*Base_velocity

#12. Multiple flow by time factor (One month/ 30 days) to estimate monthly flow
seconds<-30*24*60*60
out_ft3_sec*seconds

#13. Compare the well water loss to the stream flow
#Stream discharge is 1,360,800 ft3 in August, high end is 6,480,000. Still much less than well loss
#Well loss is 8,194,585 ft3 with SY of 0.1 or 2,458,376 with a SY of 0.03 (clay/shale)

#14. Copy over to a quarto document and add in nice figures
