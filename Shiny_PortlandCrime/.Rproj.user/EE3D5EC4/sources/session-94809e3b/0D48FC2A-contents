library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(showtext)
library(lubridate)
library(viridis) 
library(zoo)
library(DT)
library(shinythemes)


# Create server function

server <- function(input, output) {
  
  # add fonts
  font_add_google(name = "Red Hat Display", family = "redhat")
  font_add_google(name = "Cherry Cream Soda", family = "cherry")
  showtext_auto()
  ##Dispatched Calls for Service by Portland Police Bureau
  ##  https://public.tableau.com/app/profile/portlandpolicebureau/viz/DispatchedCallsforService/DispatchedCalls
  
  dispatch_calls2023 <- read.csv("https://raw.githubusercontent.com/karolo89/Raw_Data/main/dispatchedcalls_opendata_2023_1.csv")%>%
    select(-ReportDateTime)%>%
    mutate(Priority = as.factor(Priority))%>%
    mutate(FinalCallCategory = as.factor(FinalCallCategory)) %>%
    mutate(FinalCallGroup = as.factor(FinalCallGroup))%>%
    mutate(Neighborhood = as.factor(Neighborhood))%>%
    mutate(ReportMonthYear = mdy(ReportMonthYear))%>%
    mutate_if(is.character, as.double)%>%
    separate("ReportMonthYear", c("Year", "Month", "Day"), sep = "-")%>%
    select(-Day)%>%
    mutate(date= as.yearmon(paste(Year, Month), "%Y %m"))
  
  dispatch_calls2022 <-read.csv("https://raw.githubusercontent.com/karolo89/Raw_Data/main/dispatchedcalls_opendata_2022_0.csv")%>%
    mutate(Priority = as.factor(Priority))%>%
    mutate(FinalCallCategory = as.factor(FinalCallCategory)) %>%
    mutate(FinalCallGroup = as.factor(FinalCallGroup))%>%
    mutate(Neighborhood = as.factor(Neighborhood))%>%
    mutate(ReportMonthYear = mdy(ReportMonthYear))%>%
    mutate_if(is.character, as.double)%>%
    separate("ReportMonthYear", c("Year", "Month", "Day"), sep = "-")%>%
    select(-Day)%>%
    mutate(date= as.yearmon(paste(Year, Month), "%Y %m"))
  
  calls <- rbind(dispatch_calls2022, dispatch_calls2023)
  
  month_call <- calls %>%
    group_by(date, Year, FinalCallGroup)%>%
    filter(FinalCallGroup!= "NULL") %>%
    summarise(count = n()) %>%
    arrange(desc(count))%>%
    na.omit()
  
  group_month <- calls%>%
    group_by(date, FinalCallGroup)%>%
    summarise(num= n())%>%
    mutate(date= as.Date(date))%>%
    arrange(desc(num))%>%
    top_n(5)
  
  
  output$plot <- renderPlot({
    
    group_month %>% 
      filter(startdate >= input$date[1], 
             startdate <= input$date[2]) %>% 
      
      filter(FinalCallGroup == input$FinalCallGroup) %>% 
      ggplot(aes(x = startdate, y = count)) +
      geom_line() +
      geom_point(size = 2) +
      scale_color_viridis_d()+
      
      scale_y_continuous(limits = c(0, 10000), labels = scales::number_format(scale = .001, suffix = "K"))+
      
      coord_cartesian(expand = F) +
      
      labs(title= "Total Calls by Group, 2022-23",
           x="",
           y= "")+ 
      theme(
        legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title = element_text(family = "cherry", hjust = 0.5, size = 14, face = "bold", 
                                  margin = margin(t = 10, b = 10)),
        
        axis.text = element_text(family = "dosis"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())
    
    plot
  })
  
  output$table <- DT::renderDataTable({
    calls%>% 
      filter(FinalCallGroup == input$FinalCallGroup) %>% 
      arrange(date)})
}

# Define the user interface
ui <- fluidPage(theme = shinytheme("flatly"),
                # Application title
                
                titlePanel("Dispatched Calls for Service- Portland Police Bureau"),
                # sidebar
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput(inputId = "date",
                                   label   = "Enter start range",
                                   start   = "2022-01-01",
                                   end     = "2023-03-01",
                                   min     = "2022-01-01",
                                   max     = "2023-03-01"),
                    
                  selectInput(inputId  = "FinalCallGroup",
                                label    = "Select Call Type",
                                choices  = group_month$FinalCallGroup,
                                multiple = TRUE)),
                  # main panel
                  mainPanel(
                    plotOutput(outputId = "plot"),
                    dataTableOutput(outputId = "table")
                  )
                )
)




# Build and run the application
shinyApp(ui = ui, server = server)

