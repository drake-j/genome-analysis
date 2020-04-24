#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

 

data_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
time_series_confirmed <- read_csv(url(data_url))
time_series_confirmed <- time_series_confirmed %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")
time_series_confirmed_long <- time_series_confirmed %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Confirmed")
head(time_series_confirmed_long)
time_series_confirmed_long$Date <- mdy(time_series_confirmed_long$Date)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exercise 1: Time series info"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Date",
                        "Date:",
                        min = min(time_series_confirmed_long$Date),
                        max = max(time_series_confirmed_long$Date),
                        value = c(min(time_series_confirmed_long$Date),max(time_series_confirmed_long$Date))
       ) ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("datePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$datePlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # here is where data should go?

        # draw the histogram with the specified number of bins
      plotdata <- time_series_confirmed_long %>% 
            group_by(Country_Region, Date) %>% 
            summarise_at(c("Confirmed"), sum) %>% 
            filter(Country_Region %in% c("China","Japan", "Korea, South", "Italy", "Spain", "US", "France")) 
           
       ggplot(data= plotdata, aes(x=Date, y=Confirmed, color=Country_Region)) +
            geom_point() +
            geom_line() +
            ggtitle("Confirmed Covid-19 Cases") +
            xlim(input$Date[1],input$Date[2])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
