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
#time_series_confirmed_long$Date <- mdy(time_series_confirmed_long$Date)
#deaths
time_series_deaths <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")
time_series_deaths_long <- time_series_deaths %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Deaths" )
head(time_series_confirmed_long)
time_series_confirmed_long <- time_series_confirmed_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep=".", remove=FALSE)
head(time_series_confirmed_long)
time_series_deaths_long <- time_series_deaths_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep=".") %>% 
    select(Key, Deaths)

#use a full_join and use select to remove key

time_series_long_joined <- full_join(time_series_confirmed_long, time_series_deaths_long, by=c("Key"))

head(time_series_long_joined)
#recovered
time_series_recovered <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")

### convert to long format
time_series_recovered_long <- time_series_recovered %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Recovered")

### Create the Key
time_series_recovered_long <- time_series_recovered_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Recovered)

### Merge with prior table (only this time will delete the Key column
### because we have no further use for it)
time_series_long_joined <- full_join(time_series_long_joined,
                                     time_series_recovered_long, by = c("Key")) %>% 
    select(-Key)
head(time_series_long_joined)
time_series_long_joined$Date <-mdy(time_series_long_joined$Date)

cols<- c("Confirmed", "Deaths", "Recovered")

plotdata <- time_series_long_joined %>% 
    group_by(Country_Region, Date) %>% 
    summarise_at(c("Confirmed", "Deaths", "Recovered"), sum) %>% 
    filter(Country_Region %in% c("China","Japan", "Korea, South", "Italy", "Spain", "US", "France")) 

country <- c("China","Japan", "Korea, South", "Italy", "Spain", "US", "France")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Exercise 3: Select your Data(s)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Date",
                        "Date:",
                        min = min(plotdata$Date),
                        max = max(plotdata$Date),
                        value = c(min(plotdata$Date),max(plotdata$Date))
            ), 
            selectInput("y_variable",
                        label="Case Type", 
                        choices=cols, 
                        selected = "Recovered"),
            selectInput("country_variable",
                        label="Country", 
                        choices=country, 
                        selected = "US")
            
            
            ),
        
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
        plotdata <- time_series_long_joined %>% 
            group_by(Country_Region, Date) %>% 
            summarise_at(c("Confirmed", "Deaths", "Recovered"), sum) %>% 
            filter(Country_Region %in% c("China","Japan", "Korea, South", "Italy", "Spain", "US", "France")) 
        
        
        plotdata <- plotdata %>% 
            filter(Country_Region %in% c(input$country_variable))
        
        ggplot(data=plotdata, 
               mapping=aes_string(y=input$y_variable,
                                  x="Date",
                                  color="Country_Region")) +
            #aes(x=Date, color=Country_Region)) +
            geom_point() +
            geom_line() +
            ggtitle("Confirmed Covid-19 Cases") +
            xlim(input$Date[1],input$Date[2])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
