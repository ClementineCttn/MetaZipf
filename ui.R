library(shiny)

shinyUI(fluidPage(
 # tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("MetaZipf"),
  titlePanel(h4("Meta Analysis of Zipf's law for cities")),
  
  navlistPanel(
    
    tabPanel("Presentation", 
             h3("Zipf's law in empirical estimations, an interactive meta-analysis"),
             "This application aims at presenting a meta-analysis of zipf's law estimations from the literature in an interactive way. Following the meta-analysis proposed by V. Nitsch in 2005, 
                  we extend the pool of papers reviewed and give access to the database and specifications used.",  br(),   br(),  
             "The idea is to query and represent the variation of empiricial estimations of zipf's 
                  law in the literature and to relate them with urban characteristics (age of the system, economic development) and with the specifications of the regression used () to unveil
                systematic variations and deviations from the iconic -1 slope.", br(),   br(),  
             "The current database cover 750 estimations from 50 studies, from 1600 to 2011 in more than 80 countries", br(),   br(),  
             fluidRow(column(6,selectInput("alpha", "I prefer to express regressions like as:", choices=c("Lotka", "Pareto"), multiple=FALSE))),
             h5("N.B.:"), 
             h6("the Lotka form : log(Pi) ~ alpha * log(Ri) + b + e(i)"),
             h6("the Pareto form : log(Ri) ~ alpha' * log(Pi) + b' + e'(i)"),
             h6("with: Pi the population of city i, Ri its rank in the urban hierarchy and alpha' = (1 / alpha)")),
    
     tabPanel("Literature Summary", 
                 "Summarise estimations by:",
                 fluidRow(
                   column(4,selectInput("territorys", "Territory", choices=c("ALL","USA", "France", "India", "China", "USSR", "Russia", "Brazil", 
                                                                           "Israel", "Japan", "GB", "Europe", "S. Africa", "Spain", "Austria",
                                                                           "Germany", "Sweden", "Malaysia", "Netherlands", "Turkey", "Ukraine",
                                                                           "Canada", "Egypt", "Mexico", "world", "Italy", "UK", "Argentina", "Denmark",
                                                                           "Nigeria", "Poland", "Austrialia", "Balkans", "Colombia", "Greece", "Hungary", "Indonesia",
                                                                           "Morocco", "Norway", "Romania", "Switzerland", "Venezuela"), multiple=FALSE)),
                   column(4,selectInput("scales", "Urban Definition", choices=c("ALL","metro", "agglo", "local", "mixed"), multiple=FALSE)),
                   column(4,selectInput("decades", "Decade", choices=c("ALL","2010s", "2000s", "1990s", "1980s", "1970s", "1960s", "1950s",
                                                                     "1940s", "1930s", "1920s", "1910s", "1900s", "1890s", "1880s",
                                                                     "1870s", "1860s", "1850s", "1840s", "1830s", "1820s", "1810s", 
                                                                     "1800s", "1790s", "1780s", "1770s", "1760s", "1750s", 
                                                                     "1700s", "1600s"), multiple=FALSE)),
                   dataTableOutput('summary')
                   )),
              
              tabPanel("Literature Review", 
                       "Subset Table by:",
                       fluidRow(
                         column(4,selectInput("territory", "Territory", choices=c("ALL","USA", "France", "India", "China", "USSR", "Russia", "Brazil", 
                                                                                 "Israel", "Japan", "GB", "Europe", "S. Africa", "Spain", "Austria",
                                                                                 "Germany", "Sweden", "Malaysia", "Netherlands", "Turkey", "Ukraine",
                                                                                 "Canada", "Egypt", "Mexico", "world", "Italy", "UK", "Argentina", "Denmark",
                                                                                 "Nigeria", "Poland", "Austrialia", "Balkans", "Colombia", "Greece", "Hungary", "Indonesia",
                                                                                 "Morocco", "Norway", "Romania", "Switzerland", "Venezuela"), multiple=FALSE)),
                         column(4,selectInput("scale", "Urban Definition", choices=c("ALL","metro", "agglo", "local", "mixed"), multiple=FALSE)),
                         column(4,selectInput("decade", "Decade", choices=c("ALL","2010s", "2000s", "1990s", "1980s", "1970s", "1960s", "1950s",
                                                                           "1940s", "1930s", "1920s", "1910s", "1900s", "1890s", "1880s",
                                                                           "1870s", "1860s", "1850s", "1840s", "1830s", "1820s", "1810s", 
                                                                           "1800s", "1790s", "1780s", "1770s", "1760s", "1750s", 
                                                                           "1700s", "1600s"), multiple=FALSE))),
                       dataTableOutput('review')),
              tabPanel("Plots",
                       "Plot alpha estimations by:",
                       fluidRow(
                         column(4,selectInput("quanti", "Continuous variable", choices=c("N", "TRUNCATION_POINT", "DATE"), multiple=FALSE)),
                         column(4,checkboxInput("log", "Log variable", value=TRUE)),
                         column(4,selectInput("quali", "Discrete variable", choices=c("URBANSCALE", "COUNTRY", "DECADE"), multiple=FALSE)),
                         plotOutput('plot')
                         )),
               tabPanel("Models",
             "Run the meta-analysis",
             fluidRow(
               column(4,checkboxInput("year4model", "Year", value=TRUE)),
               column(4,checkboxInput("truncation4model", "Truncation point", value=F)),
               column(4,checkboxInput("country4model", "Country?", value=F)),
               tableOutput('model')
             ))
    
                       
              
         )
      )
    )
