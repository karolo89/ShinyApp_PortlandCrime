
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(showtext)
library(lubridate)
library(viridis) 
library(zoo)

# add fonts
font_add_google(name = "Red Hat Display", family = "redhat")
font_add_google(name = "Cherry Cream Soda", family = "cherry")
showtext_auto()

theme_line <-  theme(
  legend.title = element_blank(), 
  legend.position = "bottom",
  panel.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
  plot.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
  legend.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"), 
  legend.key = element_rect(fill = "transparent", colour = "transparent"),
  plot.title = element_text(family = "cherry", hjust = 0.5, size = 14, face = "bold", 
                            margin = margin(t = 10, b = 10)),
  
  axis.text = element_text(family = "dosis"),
  plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
  panel.grid.major = element_line(colour = "#DEDEDE"), 
  panel.grid.minor = element_blank())

##Dispatched Calls for Service by Portland Police Bureau
##  https://public.tableau.com/app/profile/portlandpolicebureau/viz/DispatchedCallsforService/DispatchedCalls

dispatch_calls2023 <- read.csv("https://raw.githubusercontent.com/karolo89/Raw_Data/main/dispatchedcalls_opendata_2023_1.csv")|>
  select(-ReportDateTime)|>
  mutate(Priority = as.factor(Priority))|>
  mutate(FinalCallCategory = as.factor(FinalCallCategory)) |>
  mutate(FinalCallGroup = as.factor(FinalCallGroup))|>
  mutate(Neighborhood = as.factor(Neighborhood))|>
  mutate(ReportMonthYear = mdy(ReportMonthYear))|>
  mutate_if(is.character, as.double)|>
  separate("ReportMonthYear", c("Year", "Month", "Day"), sep = "-")|>
  select(-Day)|>
  mutate(date= as.yearmon(paste(Year, Month), "%Y %m"))

dispatch_calls2022 <-read.csv("https://raw.githubusercontent.com/karolo89/Raw_Data/main/dispatchedcalls_opendata_2022_0.csv")|>
  mutate(Priority = as.factor(Priority))|>
  mutate(FinalCallCategory = as.factor(FinalCallCategory)) |>
  mutate(FinalCallGroup = as.factor(FinalCallGroup))|>
  mutate(Neighborhood = as.factor(Neighborhood))|>
  mutate(ReportMonthYear = mdy(ReportMonthYear))|>
  mutate_if(is.character, as.double)|>
  separate("ReportMonthYear", c("Year", "Month", "Day"), sep = "-")|>
  select(-Day)|>
  mutate(date= as.yearmon(paste(Year, Month), "%Y %m"))

calls <- rbind(dispatch_calls2022, dispatch_calls2023)


month_call <- calls |>
  group_by(date, Year, FinalCallGroup)|>
  filter(FinalCallGroup!= "NULL") |>
  summarise(count = n()) |>
  arrange(desc(count))|>
  na.omit() |> 
  ungroup()

group_month <- calls |>
  group_by(date, FinalCallGroup)|>
  summarise(num= n())|>
  mutate(date= as.Date(date))|>
  arrange(desc(num))|>
  top_n(5) |>
  ungroup()


p1_calls <- month_call%>%
  ggplot(aes(x= fct_rev(fct_reorder(FinalCallGroup, count)), y= count))+
  geom_bar(stat = 'identity', aes(color=FinalCallGroup, fill= FinalCallGroup))+
  
  scale_y_continuous(labels = scales::number_format(scale = .001, suffix = "K"))+
  
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  
  labs(title= "Total Calls by Group, 2022-23",
       x="",
       y= "")+
  
  theme_minimal()+
  theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), unit = "cm"), 
        legend.position = "none",
        
        plot.title = element_text(family = "cherry", hjust = 0.5, size = 20, face = "bold", 
                                  margin = margin(t = 10, b = 10)),
        panel.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
        plot.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
        legend.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"))


p1_calls


font_add_google(name = "Bungee Shade", family = "bungee")
font_add_google(name = "Dosis", family = "dosis")
showtext_auto()


line_calls <- ggplot(group_month, aes(date, num, colour= FinalCallGroup, group= FinalCallGroup))+
  geom_line(aes(linetype=FinalCallGroup)) +
  geom_point(size = 2) +
  scale_color_viridis_d()+
  
  scale_y_continuous(limits = c(0, 10000), labels = scales::number_format(scale = .001, suffix = "K"))+
  
  coord_cartesian(expand = F) +
  
  labs(title= "Total Calls by Group, 2022-23",
       x="",
       y= "")+ theme_line

line_calls
