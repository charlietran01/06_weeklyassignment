library(tidyverse)
library(shiny)
library(lubridate)

rsconnect::setAccountInfo(name='charlietran01', token='D199D8F26F0D1523B858F4DD3624E6F9', secret='V8g1nP6fX8kIcBORkDuvDnBeTEggs7FACXHOgcob')

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(
  selectInput("state",
              "Choose States:",
              choices = covid19$state,
              multiple = TRUE),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
  
)
server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid19 %>% 
      mutate(cumsum = cumsum(cases)) %>% 
      mutate(days = difftime(ymd(date), 
                             ymd(20200128), 
                             units = "days")) %>% 
      filter(state == input$state,
             cumsum > 20) %>% 
      ggplot()+
      geom_line(aes(x = days,
                    y = cumsum,
                    color = state))+
      scale_y_log10()+
      labs(title = "US COVID19 Cases",
           y = "Cummulative Cases",
           x = "Days since 20+ Cases")+
      theme_minimal()
      
  })
}
shinyApp(ui = ui, server = server)