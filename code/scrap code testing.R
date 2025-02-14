
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

##Plotly export
library(plotly)
fig <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
fig
install.packages("remotes")
install.packages("devtools")
devtools::install_github("plotly/dashCoreComponents")
devtools::install_github("plotly/dashHtmlComponents")

remotes::install_version("dashCoreComponents", version = "0.1.0")
remotes::install_version("dashHtmlComponents", version = "0.3.0")
remotes::install_github("plotly/dashR", upgrade = "always")

install.packages("dashCoreComponents")
install.packages("dashHtmlComponents")
install.packages("plotly")
version

devtools::install_github("dash")
devtools::install_github("plotly/dash-html-components")


library(dash)
library(dash-html-components)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)

1+1













library(dash)
library(dashCoreComponents)

# Create a Dash app
app <- dash_app()

# Set the layout of the app
app %>% set_layout(
  h1('Hello Dash'),
  div("Dash: A web application framework for your data."),
  dccGraph(
    figure = list(
      data = list(
        list(
          x = list(1, 2, 3),
          y = list(4, 1, 2),
          type = 'bar',
          name = 'SF'
        ),
        list(
          x = list(1, 2, 3),
          y = list(2, 4, 5),
          type = 'bar',
          name = 'Montr\U{00E9}al'
        )
      ),
      layout = list(title = 'Dash Data Visualization')
    )
  )
)

# Run the app
app %>% run_app()
