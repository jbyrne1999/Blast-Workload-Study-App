library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

setwd("C:/Users/Jack/Desktop/School Stuff/VT Baseball/Blast Workload Study/January Full Team/Jan")
files = list.files(pattern = "*.csv")

x <- lapply(files, fread, skip = 7)
#names(x) <- basename( x )

master = rbindlist(x, idcol = "PlayerID")


#import data
#setwd("C:/Users/Jack/Desktop/School Stuff/VT Baseball/Blast Workload Study/full_team_report_2020_01_26_2020_02_09_1581469450/Full Team Report - 2020-01-26 - 2020-02-09 - 1581469450")
#data1 <- read.csv("cade_hunter - 1581469472.csv", skip = 7)
#data1 <- mutate(data1, name = "Cade Hunter")
#data2 <- read.csv("cade_swisher - 1581469474.csv", skip = 7)
#data2 <- mutate(data2, name = "Cade Swisher")

#Combine the data
#master <- rbind(data1,
#                data2)
#glimpse(master)
#Clean the date

master$cleanDate <- as.POSIXct(master$Date,format = "%B %e, %Y / %I:%M:%S %p")
master$day <- day(master$cleanDate)

#summaryMaster <- master %>%
#  group_by(name,day) %>%
#  mutate(swings = n(),cumSwings = row_number(), swing.number = (swings - cumSwings) + 1) %>%
#  gather(condition, measurement, Plane.Score:Peak.Hand.Speed..mph., factor_key=TRUE)

forEachSwingNum <- master %>%
  group_by(PlayerID,day) %>%
  mutate(swings = n(),cumSwings = row_number(), swing.number = (swings - cumSwings) + 1, counter = 1)

forEachSwingNum <- forEachSwingNum %>%
  group_by(swing.number) %>%
  summarise(Mean.Plane.Score = mean(`Plane Score`),
         Mean.Connection.Score = mean(`Connection Score`),
         Mean.Roatation.Score = mean(`Rotation Score`),
         Mean.Bat.Speed = mean(`Bat Speed (mph)`),
         Mean.Roatation.Acc = mean(`Rotational Acceleration (g)`),
         Mean.Power = mean(`Power (kW)`),
         Mean.Time.To.Contact = mean(`Time to Contact (sec)`),
         Mean.Peak.Hand.Speed = mean(`Peak Hand Speed (mph)`),
         freq = sum(counter))

ggplot(forEachSwingNum, aes(swing.number, freq))+geom_bar(stat = "identity")

forEachSwingNum <- filter(forEachSwingNum, freq >9)

forEachSwingNum <- gather(forEachSwingNum, key = "Metric", value = "Value", Mean.Plane.Score:Mean.Peak.Hand.Speed,
                          factor_key = T)  

#forEachSwingNum <- complete(forEachSwingNum)
#row.names(forEachSwingNum) <- 1:NROW(forEachSwingNum)

library(ggplot2)

#facet wrap
#ggplot(summaryMaster,aes(swing.number,measurement,color = name)) +
#  geom_point() +
#  facet_wrap(~condition)


ui <- fluidPage(
  
  
  titlePanel("Blast Workload Analysis"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "MetricLabel",
        "Metric",
        unique(forEachSwingNum$Metric)
      )
    ),
    
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    forEachSwingNum <- forEachSwingNum %>% 
      filter(Metric == input$MetricLabel) 
    
    ggplot(forEachSwingNum, aes(swing.number, Value))+geom_point()+geom_smooth()+labs(x = "Swing Number", y = "Value")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

