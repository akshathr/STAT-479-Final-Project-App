# Load packages ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(maps)
library(mapproj)
library(dplyr)
library(xlsx)
library(tidyselect)
library(ggplot2)

## ---- WISCONSIN COUNTY LEVEL DATA ----

# Load Wisconsin COVID-19 Data
counties <- read.csv('covidWITotal.csv')
counties$DATE = substr(counties$DATE, start=1, stop=10)
counties = mutate(counties,  total = NEGATIVE+POSITIVE)
counties = mutate(counties,  Percent_Positive = (POSITIVE/total)*100)

#Load Wisconsin Census Data
population = read.csv('WIpop.csv')
population$CTYNAME = substr(population$CTYNAME,1,nchar(as.character(population$CTYNAME))-7)
population = filter(population, YEAR == "11")
population =  select(population, -SUMLEV, -STATE, -COUNTY, -STNAME, -YEAR)
population = rename(population, NAME = CTYNAME)
total.population = filter(population, AGEGRP == "0")

#Load the Traffic Fatlities Dataset
fatalities = read.csv('fatalities.csv')
fatalities = fatalities %>%
  rename(FATALITIES = X2020.) %>%
  rename(NAME = County) %>%
  select(NAME, FATALITIES)

#Load Social Distancing Data
social.distance = read.xlsx('social-distancing-statistics.xlsx', 1)
social.distance = social.distance %>% 
  rename(average_distance = Percent.Change.in.Average.Distance.Traveled) %>% 
  rename(non_essential_visits = Percent.Change.in.Non.essential.Visitation) %>% 
  rename(human_encounters = Decrease.in.Human.Encounters..Compared.to.National.Baseline.) %>% 
  rename(social_distance_average = SD.Grade..Average.)

social.distance$social_distance_grade = social.distance$SD.Grade..Unacast.

#Join the two datasets
counties.population = full_join(counties, total.population, by = "NAME")
final.df = full_join(counties.population, fatalities, by="NAME")

#Creating new variables/metrics
final.df = final.df %>% 
  rename(total_population = POP)
final.df = mutate(final.df, death_rate = (DEATHS/POSITIVE)*100) #death rate for positive tests
final.df = mutate(final.df, percent_negative = (NEGATIVE/total)*100) #negative test rate

## ---- NATIONAL LEVEL COMPARISON DATA ----

#National COVID-19 dataset
national.covid = read.csv('covid-national.csv')

#Population Data
national.population = read.csv('national-population.csv')

#creating new variables
national.covid = national.covid %>% 
  mutate(percent_positive = (positive/total) * 100) %>% #percent positive tests
  mutate(death_rate = (death/positive) * 100) #death rate

#National Social Distancing dataset
social.distance.national = read.xlsx('social-distancing-national.xlsx', 1)
social.distance.national = rename(social.distance.national, state = State.)
social.distance.national = rename(social.distance.national, average_distance = Percent.Change.in.Average.Distance.Traveled) 
social.distance.national = rename(social.distance.national, non_essential_visits = Percent.Change.in.Non.essential.Visitation) 
social.distance.national = rename(social.distance.national, human_encounters = Decrease.in.Human.Encounters..Compared.to.National.Baseline.)  
social.distance.national = rename(social.distance.national, social_distance_average = SD.Grade..Average.)

#Comparison Plot Data
national.population = national.population %>%
  select(NAME, CENSUS2010POP) %>%
  rename(state = NAME) %>%
  rename(state_population = CENSUS2010POP)

#Grab positive tests
national.positive.tests = national.covid %>% 
  select(state, positive)

#Join the datasets
social.distance.national = social.distance.national %>% 
  full_join(national.population, by='state')
social.distance.national = social.distance.national %>% 
  full_join(national.positive.tests, by='state')
social.distance.national$state_population[3] = 6392017
social.distance.national$positive[3] = 7202
social.distance.national = social.distance.national[-52,]
social.distance.national = mutate(social.distance.national, national_positive = (positive/state_population) * 100)

# Source helper functions
source("Helper.R")

# ---- USER INTERFACE ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(div(HTML("COVID-19 in Wisconsin"))),
  
  sidebarLayout(
    sidebarPanel(
      helpText("View COVID-19 information by county in Wisconsin."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent Positive", "Death Rate", 
                              "Change in Average Distance Traveled", 
                              "Change in Non-Essential Visits", "Change in Human Encounters", 
                              "Social Distancing Score"),
                  selected = "Death Rate"),
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("map")),
        tabPanel("Graph", plotOutput("graph")),
        tabPanel("Comparison", plotOutput("comparison"))
      )
    )
  )
)


# ---- SERVER LOGIC ----
server <- function(input, output) {
  
  #Chloropleth Map Data
  output$map <- renderPlot({
    
    data <- switch(input$var, "Percent Positive" = final.df$Percent_Positive, 
                   "Death Rate" = final.df$death_rate, 
                   "Change in Average Distance Traveled" = social.distance$average_distance,
                   "Change in Non-Essential Visits" = social.distance$non_essential_visits,
                   "Change in Human Encounters" = social.distance$human_encounters,
                   "Social Distancing Score" = social.distance$social_distance_average)
    
    color <- "red"
    
    legend <- switch(input$var, "Percent Positive" = "% Tests Positive", 
                     "Death Rate" = "% Percent Dead", 
                     "Change in Average Distance Traveled" = "% Change in Average Distance",
                     "Change in Non-Essential Visits" = "% Change in Visits", 
                     "Change in Human Encounters" = "% Change in Human Encounters",
                     "Social Distance Score" = "Social Distance Score")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
  
  #Comparison Graph Data
  output$graph <- renderPlot({
    
    if(as.character(input$var) == "Percent Positive"||as.character(input$var)== "Death Rate"){
      plot_data = national.covid
    }
    else{
      plot_data = social.distance.national
    }
    
    yvar <- switch(input$var, "Percent Positive" = plot_data$percent_positive, 
                   "Death Rate" = plot_data$death_rate, 
                   "Change in Average Distance Traveled" = plot_data$average_distance,
                   "Change in Non-Essential Visits" = plot_data$non_essential_visits,
                   "Change in Human Encounters" = plot_data$human_encounters,
                   "Social Distancing Score" = plot_data$social_distance_average)
    
    # xvar <- switch(input$var, "Percent Positive" = national.covid$state, 
    #                "Death Rate" = national.covid$state, 
    #                "Change in Average Distance Traveled" = social.distance.national$state,
    #                "Change in Non-Essential Visits" = social.distance.national$state,
    #                "Change in Human Encounters" = social.distance.national$state,
    #                "Social Distancing Score" = social.distance.national$state)
    
    
    ggplot(plot_data, aes(x=state, y=yvar)) +
      geom_segment( aes(x=state, xend=state, y=0, yend=yvar ), color=ifelse(plot_data$state %in% c("WI", "Wisconsin"), "orange", "blue"), size=ifelse(plot_data$state %in% c("WI","Wisconsin"), 1.3, 0.7),linetype="dotdash" ) +
      geom_point( color=ifelse(plot_data$state %in% c("WI", "Wisconsin"), "orange", "blue"), size=ifelse(plot_data$state %in% c("WI","Wisconsin"), 5, 2) ) +
      coord_flip() +
      theme(
        plot.title=element_text(family="Courier", face="bold", size=20)
      ) +
      xlab("State") +
      ylab("Percentage")+
      ggtitle("Comparison of National Averages")
    
    
  })
  output$comparison <- renderPlot({
    
    xvalue = social.distance.national$social_distance_average
    yvalue = national.covid$percent_positive
    data = data.frame(xvalue, yvalue)
    
    ggplot(data, aes(x=xvalue, y=yvalue)) +
      geom_smooth(method=loess) + xlab("Social Distancing Score") + ylab("Percent Positive") + ggtitle("Comparison of National Percent Positive and Social Distancing Score") +
      theme(
        plot.title=element_text(family="Courier", face="bold", size=20)
      )
    
    
  })
  
}

# ---- Run app ----
shinyApp(ui, server)