library(shiny)

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("MetaZipf"),
  titlePanel(h4("Meta Analysis of Zipf's law for cities")),
  
  navlistPanel(
    
    tabPanel("Presentation",
             column(3, img(src = "favicon.png",class="img-responsive")),
             column(9, h1("Interactive meta-analysis of empirical Zipf's laws")),
            tags$p(class="text-justify",
            "This application aims at presenting a meta-analysis of Zipf's law estimations from the literature in an interactive way. Following the meta-analysis proposed by V. Nitsch in 2005, 
                  and extending the pool of papers reviewed, it gives access to the database and specifications used.",  br(),   br(),  
             "The idea is to allow interactive queries and to represent the variation of empiricial estimations of Zipf's 
                  law in the literature. This meta-analysis relates the variation of Zipf's estimated coefficients with 
                urban characteristics (age of the system, economic development) and with the specifications of the regression used (urban definitions,
              truncation points, number of cities) to unveil systematic deviations from the iconic -1 slope.", br(),   br(),
             "The current database cover 1005 estimations from 50 studies, 
            spanning from 1600 to 2011 in more than 80 countries."),
             fluidRow(column(6,selectInput("alpha", "I prefer results to be expressed in the regression form of:", choices=c("Lotka", "Pareto"), multiple=FALSE)),
            column(6,
             h6("The Lotka form : log(Pi) ~ alpha * log(Ri) + b + e(i)"),
             h6("The Pareto form : log(Ri) ~ alpha' * log(Pi) + b' + e'(i)"),
             h6("with: Pi the population of city i, Ri its rank in the urban hierarchy and alpha' = (1 / alpha)"))),
            h6("N.B. 'Old' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. America, Oceania, Africa and central Asia are considered 'New' in that respect."),
            h5("Literature covered:"), dataTableOutput('references'),
            h6("Nitsch, V. (2005). Zipf zipped. Journal of Urban Economics, 57(1), 86-100. Total population in thousands, from UN estimates (1950-2015) <http://esa.un.org/unpd/wpp/DVD/Files/1_Excel%20(Standard)/EXCEL_FILES/1_Population/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS.>")
            ),
    
     tabPanel("Literature Summary", 
                 "Summarise estimations by:",
                 fluidRow(
                   column(4,selectInput("territorys", "Territory", choices=c("ALL","WORLD", "United States of America", "India", "China", "Russia", "Brazil", "South Africa", 
                                                                             "United Kingdom", "France", "USSR", "Israel", "Japan", "Europe", "Spain", "Austria",
                                                                           "Germany", "Sweden", "Malaysia", "Netherlands", "Turkey", "Ukraine",
                                                                           "Canada", "Egypt", "Mexico", "Italy", "Argentina", "Denmark",
                                                                           "Nigeria", "Poland", "Austrialia", "Balkans", "Colombia", "Greece", "Hungary", "Indonesia",
                                                                           "Morocco", "Norway", "Romania", "Switzerland", "Venezuela"), multiple=FALSE)),
                   column(4,selectInput("scales", "Urban Definition", choices=c("ALL","metro", "agglo", "local", "mixed"), multiple=FALSE)),
                   column(4,selectInput("decades", "Decade", choices=c("ALL","2010s", "2000s", "1990s", "1980s", "1970s", "1960s", "1950s",
                                                                     "1940s", "1930s", "1920s", "1910s", "1900s", "1890s", "1880s",
                                                                     "1870s", "1860s", "1850s", "1840s", "1830s", "1820s", "1810s", 
                                                                     "1800s", "1790s", "1780s", "1770s", "1760s", "1750s", 
                                                                     "1700s", "1600s"), multiple=FALSE)),
                   dataTableOutput('summary'),
                   plotOutput('histalpha')
                   )),
              
              tabPanel("Literature Review", 
                       "Subset Table by:",
                       fluidRow(
                         column(4,selectInput("territory", "Territory", choices=c("ALL","WORLD", "United States of America", "India", "China", "Russia", "Brazil", "South Africa", 
                                                                                  "United Kingdom", "France", "USSR", "Israel", "Japan", "Europe", "Spain", "Austria",
                                                                                  "Germany", "Sweden", "Malaysia", "Netherlands", "Turkey", "Ukraine",
                                                                                  "Canada", "Egypt", "Mexico", "Italy", "Argentina", "Denmark",
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
             h3("Select Features to Test in the Meta Analysis"),
             fluidRow(
               column(4,checkboxInput("urbanisation4model", "Age of Urbanisation", value=F)), 
               column(4,checkboxInput("truncation4model", "Truncation Level", value=F)),
                column(4,checkboxInput("year4model", "Year", value=F)),
               column(4,checkboxInput("N4model", "N Cities", value=F)),
               column(4,checkboxInput("scale4model", "City definition", value=F)),
               column(4,checkboxInput("countrySize", "Country Population", value=F)),
               br(), 
               h3("Results of the Regression of Alpha by the Selected Features."),
               tableOutput('modelparameters'),
               h3("Estimated Coefficients on the Variation of Alpha"),
               tableOutput('model'),
               textOutput('REFS')              
             ))
    
                       
              
         )
      )
    )
