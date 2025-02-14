
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
Esco_Change