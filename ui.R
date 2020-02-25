#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tableHTML)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(RColorBrewer)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
                  
                  tags$head(
                      tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
                  ),
                  
                  titlePanel(
                      tags$img(src="https://miro.medium.com/max/840/1*N8Wj4P1t-DpCcxVkFHt63w.jpeg",height=150,width=650),
                      
                  ),
                  hr(),
                  headerPanel(
                      h4("MARIO SURVEY - NLP", 
                         style = "font-family: 'serif', cursive;
        font-weight: 500; line-height: 1.1; 
        color: #4d3a7d;")),
                  br(),
                  
                  
                  
                  
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                      sidebarPanel(
                          
                          selectInput(inputId = "Graph", label = "Select the Framework Display",
                                      choices = c("Bing Sentiment",
                                                  "NRC Sentiment",
                                                  "Afinn Sentiment",
                                                  "N-gram")),
                          
                          selectInput(inputId = "Question", label = "Select the survey Question number",
                                      choices = c("Question1",
                                                  "Question2",
                                                  "Question3",
                                                  "Question4"
                                      )),
                          
                          
                          
                      ),
                      
                      
                      
                      
                      
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                          
                          tabsetPanel(
                              tabPanel("Business Context",
                                       h3("About"),
                                       p("Nintendo is well known for its Mario Bros. videogames and different consoles that they offer, namely, Nintendo Switch and Nintendo 3DS/2DS but they haven't ventured to mobile devices"),
                                       p("For our Natural Language Processing assignment, 
                        we designed a survey to understand consumer behavior and test our 
                        business success: Are you willing to download the new Mario Bros. 
                        Augmented Reality video game for mobile devices?"),
                                       p("We designed a 5 question survey to collect data from a representative group of our potential market consisting of masters students between the ages of 20 to 30 years old. 
"),br(),
                                       h3("Survey Questions "),
                                       h5("1. Do you like playing video games? If you like playing video games or if you don't like playing video games, please state your reason why."),
                                       h5("2. If you like playing video games, what kind of games do you play? (e.g RPG, adventure, strategy)  If you don't like playing video games, what do you like to do?"),
                                       h5("3. Which platforms do you play your game and why do you play on those specific platforms (e.g PS4, Xbox, mobile)?"),
                                       h5("4.List all Mario games that you know."),
                                       h5("5.If there is a new SuperMario 2020 game, would you download and play the game?"),
                                       p("From the data collected in this study, we expect to understand the current
                          trends between gamers and non-gamers, detect any potential market niche and predict
                          if a person is willing to download and play the Nintendo Marios Bros. videogame on their 
                          mobile devices.")
                                       
                              ),
                              
                              
                              tabPanel("Graphs", plotOutput("bingQ1",  width = "500", height = "300"),
                                       plotOutput("nrcQ"), 
                                       plotOutput("afinn"),
                                       plotOutput("ngram")),
                              tabPanel("Word counts", plotOutput("counts",  width = "500", height = "300")),
                              
                              
                              
                              tabPanel("Business Insights",
                                       h4("Question 1"),
                                       
                                       p("Women respondents stated they don't and won't likely play video games
                  because they are a waste of time, boring and only for guys who like to 
                  kill time"),
                                       h4("Question 2"),
                                       p("Respondents who are video game players already have preferred games 
                 but for non-video game players prefer active sports or do leisure activities"),
                                       h4("Question 3"),
                                       p("Majority of the respondents prefer to play on their mobile phones regardless
                 of what game and video game players have preferred consoles (e.g. XBox, PC and PS4)"),
                                       h4("Question 4"),
                                       p("Super Mario Brothers is the most popular game for all the respondents 
                 followed closely by Super Smash"),
                                       h4("Question 5"),
                                       p("Respondents will consider downloading a new game if it's free, interesting,
                 suitable for their schedule and has a similar interface as Super Mario"),
                                       
                              ),
                              tabPanel("Contact US",
                                       h4("For further question feel free to reach us at support@nintendo.com"),
                                       br(),
                                       h3("Team 10:", color="yellow"),
                                       br(),
                                       h4(" Bhavya Garg | Evans Imade | Han Zhang | Jose Pereira |
                                  Kamille Andrea Galura | Keita Eriawan"))
                              
                          ),
                          
                          
                      )
                      
                      
                  )
                  
                  
)
)