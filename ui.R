library(shiny)

shinyUI(
  fluidPage(theme = "flatly_bootstrap.css",
            tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
            
            tags$head(
              tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Orbitron|Cabin:400,700');
    "))
            ),
            headerPanel("MetaZipf"),
            #   headerPanel(),
    
  titlePanel(
             h2("Interactive and Open Meta Analysis of Zipf's law for cities")),
  
  navlistPanel(
    
    tabPanel("Presentation",
             column(3, img(src = "favicon.png",class="img-responsive")),
             column(9, h1("Interactive Meta-analysis of Empirical Zipf's Estimates")), br(),
            tags$p(class="text-justify",
            "This application aims at presenting a meta-analysis of Zipf's law estimations from the literature in an interactive way. Following the meta-analysis proposed by V. Nitsch in 2005, 
                  and extending the pool of papers reviewed, it gives access to the database and specifications used.",  br(),   br(),  
             "The idea is to allow interactive queries and to represent the variation of empiricial estimations of Zipf's 
                  law in the literature. This meta-analysis relates the variation of Zipf's estimated coefficients with 
                urban characteristics (age of the system, economic development) and with the specifications of the regression used (urban definitions,
              truncation points, number of cities) to unveil systematic deviations from the iconic -1 slope.", br(),   br(),
             "The current database covers 1135 estimations from 61 studies, spanning more than 80 countries over 400 years. It is available here: https://github.com/ClementineCttn/MetaZipf."),
                      
            tags$hr(),
            
            fluidRow(column(6,selectInput("alpha", "I prefer results to be expressed in the regression form of:", choices=c("Lotka", "Pareto"), multiple=FALSE)),
            column(6,
             h6("The Lotka form : log(Pi) ~ alpha * log(Ri) + b + e(i)"),
             h6("The Pareto form : log(Ri) ~ alpha' * log(Pi) + b' + e'(i)"),
             h6("with: Pi the population of city i, Ri its rank in the urban hierarchy and alpha' = (1 / alpha)"))),
            tags$hr(),
           h5("Clementine Cottineau, 2016, University College London (CASA)."),
          h5("For any information / comment / suggestion, contact: c.cottineau@ucl.ac.uk"),
           tags$hr(),
           h6("Credits: T. Park from bootswatch.com for Flatly css file")),
    
    tabPanel("Meta-Analysis",
             tabsetPanel(
      tabPanel("Literature Overview",
             h4('TOP journals where the estimations* are drawn from:'),  
            dataTableOutput('topjournals'),
            '*Each reference count as one, irrespective of the number of estimations',  tags$hr(),
            h4('TOP authors* providing estimations:'), 
            dataTableOutput('topauthors'),
            '*the estimations published by the same author(s) in different publications are not added.',   tags$hr(),
            h4('TOP countries for the dispersion of results*:'),
            dataTableOutput('topcountries'),
            '*measured by the standard deviation of alpha for countries with more than 5 estimations.',  tags$hr(),
            h4("Complete list of references:"), dataTableOutput('references'),
            h6("Nitsch, V. (2005). Zipf zipped. Journal of Urban Economics, 57(1), 86-100. Total population in thousands, from UN estimates (1950-2015) <http://esa.un.org/unpd/wpp/DVD/Files/1_Excel%20(Standard)/EXCEL_FILES/1_Population/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS.>")
            ),
    
     tabPanel("Estimates Summary", 
              h4("Summarise estimations by:"),
                 fluidRow(
                   column(4,selectInput("territorys", "Country", choices=c("ALL",
                        "Algeria", "Argentina", "Australia", "Austria", "Bangladesh", "Belgium", "Brazil", "Bulgaria",
                        "Canada", "Chile", "China", "Colombia", "Cuba", "Denmark", "Egypt", "Finland", "France", "Germany",
                        "Greece", "Hungary", "India", "Indonesia", "Iran", "Israel", "Italy", "Japan", "Kazakhstan", "Malaysia",
                        "Mexico", "Morocco", "Netherlands","Nigeria", "Norway", "Pakistan", "Peru", "Philippines", "Poland",
                        "Portugal", "Romania", "Russia", "South Africa", "South Korea", "Spain", "Sweden", "Switzerland", "Taiwan",
                        "Thailand", "Turkey", "Ukraine", "United Kingdom", "United States of America", "Venezuela", "Vietnam"), multiple=FALSE)),
                   column(4,selectInput("scales", "Urban Definition", choices=c("ALL", "Local Units" = "1_Local", "Agglomerations" = "2_Agglo", 
                                                                               "Metropolitan Areas" = "3_Metro", "Mixed Definitions" = "4_Mixed"), multiple=FALSE)),
                   column(4,selectInput("decades", "Decade", choices=c("ALL","2010s", "2000s", "1990s", "1980s", "1970s", "1960s", "1950s",
                                                                     "1940s", "1930s", "1920s", "1910s", "1900s", "1890s", "1880s",
                                                                     "1870s", "1860s", "1850s", "1840s", "1830s", "1820s", "1810s", 
                                                                     "1800s", "1790s", "1780s", "1770s", "1760s", "1750s", 
                                                                     "1700s", "1600s"), multiple=FALSE)),
                   dataTableOutput('summary'),
                   tags$hr(),
                   plotOutput('histalpha')
                   ),
              tags$hr(),
              h4("Visualise variation of alpha with:"),
              fluidRow(
                column(4,selectInput("quanti", "Continuous variable (y)", choices=c("N", "TRUNCATION_POINT", "DATE"), multiple=FALSE)),
                column(4,checkboxInput("log", "Log(y)", value=TRUE)),
                column(4,selectInput("quali", "Categorical variable (colour)", choices=c("URBANSCALE", "COUNTRY", "DECADE", "ECO", "SOC", "PHYS"), multiple=FALSE)),
                plotOutput('plot')
              )),
              
             
               tabPanel("Meta Analysis",
             h4("Select Features to Test in the Meta Analysis"),
             fluidRow(
               column(4,checkboxGroupInput("technicalSpecs", "Technical Specifications", 
                                           c("All" = "alltech",
                                             "City definition" = "scale4model", 
                                              "Truncation Level" = "truncation4model",
                                              "N Cities" = "N4model"), selected = NULL, inline = FALSE)),
               column(4,checkboxGroupInput("topicalSpecs", "Topical Specifications", 
                                           c("All" = "alltop",
                                             "Age of Urbanisation" = "urbanisation4model",
                                             "Date (relative to 1950)" = "year4model",
                                             "Country Population" = "countrySize"), selected = NULL, inline = FALSE)),
               column(4,checkboxGroupInput("otherSpecs", "Other Specifications", 
                                           c("All" = "allother",
                                             "Journal Subject" = "discipline", "", ""), selected = NULL, inline = FALSE)),
               tags$hr(),
               h4("Results of the Regression of Alpha by the Selected Features."),
               tableOutput('modelparameters'),
               tags$hr(), h4("Estimated Coefficients on the Variation of Alpha"),
               tableOutput('model'),
               column(4,conditionalPanel(
                 condition = 'input.technicalSpecs.indexOf("truncation4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1', 
                 sliderInput("truncVal", "Truncation points (to define high, medium and low truncatures)",
                             min = 0, max = 1000000, value = c(10000, 100000)))),
                 column(4,conditionalPanel(
                 condition = 'input.technicalSpecs.indexOf("N4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1',
                 sliderInput("NVal", "Number of cities (to define large, medium and small samples)",
                             min = 1, max = 1000, value = c(30, 300)))),
                 column(4,conditionalPanel(
                   condition = 'input.topicalSpecs.indexOf("countrySize") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                 sliderInput("PopVal", "Thousands of Residents (to define large, medium and small countries)",
                             min = 1, max = 1000000, value = c(10000, 100000)))),
             
               textOutput('REFS'),
               h6("N.B. 'Old' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. America, Oceania, Africa and central Asia are considered 'New' in that respect.")
               
             )),
    
    tabPanel("Raw Meta Data", 
             h4("Subset Table by:"),
             fluidRow(
               column(4,selectInput("territory", "Country", choices=c("ALL","Algeria", "Argentina", "Australia", "Austria", "Bangladesh", "Belgium", "Brazil", "Bulgaria",
                                                                        "Canada", "Chile", "China", "Colombia", "Cuba", "Denmark", "Egypt", "Finland", "France", "Germany",
                                                                        "Greece", "Hungary", "India", "Indonesia", "Iran", "Israel", "Italy", "Japan", "Kazakhstan", "Malaysia",
                                                                        "Mexico", "Morocco", "Netherlands","Nigeria", "Norway", "Pakistan", "Peru", "Philippines", "Poland",
                                                                        "Portugal", "Romania", "Russia", "South Africa", "South Korea", "Spain", "Sweden", "Switzerland", "Taiwan",
                                                                        "Thailand", "Turkey", "Ukraine", "United Kingdom", "United States of America", "Venezuela", "Vietnam"), multiple=FALSE)),
               column(4,selectInput("scale", "Urban Definition", choices=c("ALL", "Local Units" = "1_Local", "Agglomerations" = "2_Agglo", 
                                                                           "Metropolitan Areas" = "3_Metro", "Mixed Definitions" = "4_Mixed"), multiple=FALSE)),
               column(4,selectInput("decade", "Decade", choices=c("ALL","2010s", "2000s", "1990s", "1980s", "1970s", "1960s", "1950s",
                                                                  "1940s", "1930s", "1920s", "1910s", "1900s", "1890s", "1880s",
                                                                  "1870s", "1860s", "1850s", "1840s", "1830s", "1820s", "1810s", 
                                                                  "1800s", "1790s", "1780s", "1770s", "1760s", "1750s", 
                                                                  "1700s", "1600s"), multiple=FALSE))),
             dataTableOutput('review')))),    
    tabPanel("Contribute !",
             h1("Add your own reviewed estimates:"), 
             h5("Please remember to press buttons to add the reference and the estimates, 
             and to send this data to the moderator for them to be added to the open database."),
             wellPanel(
             column(4,selectInput("type", "Document type",
                                  choices=c("Journal Article", "Book", "Thesis"),
                                  multiple=FALSE)),
             column(4,textInput("author", "Author(s)",
                                value = "Ex: Lotka A. J.")),
             column(4,numericInput("year", "Publication Year", value = "2016")),
             column(4,textInput("journal", "Journal / Book Title", value = "")),
             column(4,numericInput("page", "Page of Results", value = "1")),
             column(4,selectInput("regression", "Regression Form*",
                                  choices=c("LOTKA", "PARETO"), multiple=FALSE)), br(),
             h6("*Regression forms: LOTKA = log(Pi) ~ alpha * log(Ri) + b + e(i) or PARETO = log(Ri) ~ alpha' * log(Pi) + b' + e'(i)"),
             h6("with: Pi the population of city i, Ri its rank in the urban hierarchy and alpha' = (1 / alpha)")),
          
             fluidRow(column(6,sliderInput("nestimates", "Number of estimates",
                                           min = 1, max = 100, value = 1)), 
             column(6,textInput("url", "URL of document", value = ""))),
             tags$hr(),
             uiOutput(outputId = "nestimateRows"),
             tags$hr(),
             column(4,textInput("comment", NULL, value = "Any Comment?")),
             column(4,textInput("from", NULL, value = "Your E-mail")),
             column(4,actionButton("sendMail", "Report your additions"))
     )
)
      )
    )
