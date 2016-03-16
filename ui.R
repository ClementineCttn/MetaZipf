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
             column(9, h1("Interactive and Open Meta-analysis of Empirical Zipf's Estimates")), br(),
            tags$p(class="text-justify",
            "This application aims at presenting a crowdsourced meta-analysis of Zipf's law estimations from the literature in an interactive way. ",  br(), br(), 
"Following the work of V. Nitsch (2005), it extends the pool of papers reviewed (initially and through crowdsourcing) and shares access 
            to the database to enhance its represenjtativity across dates, areas and study fields.",  br(),   br(),  
             "The idea is to allow interactive queries into this pool of papers, to represent and to model the variation of empiricial estimations of Zipf's 
                  law's exponent in the literature, with respect to the systems of cities studied. Indeed, this meta-analysis 
        considers urban characteristics (age of the system, total population), the specifications of the regression used (urban definitions,
              truncation points, number of cities) and meta-informations (discipline of the journal publishing the paper) to unveil systematic deviations from the iconic -1 value.", br(),   br(),
             HTML('The current database covers 1151 estimations from 59 studies, spanning over more than 80 countries over 400 years. 
            It is available for download at this address (<a href="https://github.com/ClementineCttn/MetaZipf">https://github.com/ClementineCttn/MetaZipf</a>) and the user of this application
            is strongly invited to enrich this database by submitting their own empirical estimates using the \'Contribute !\' tab on the left.'), br(),   br(),
            "Before starting, choose a regression form into which all subsequent results will be expressed."),
                      
            tags$hr(),
            
            fluidRow(column(6,selectInput("alpha", "I prefer results to be expressed in the regression form of:", 
                                          choices=c("Lotka", "Pareto"), multiple=FALSE)),
            column(6,
                   withMathJax(h6("The Lotka form : $$\\log(P_i) = \\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$")),
                   withMathJax(h6("The Pareto form : $$\\log(R_i) = \\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$")),
             withMathJax(h6("with: \\(P_i\\) the population of city \\(i\\),
                            \\(R_i\\) its rank in the urban hierarchy,
                            \\(\\alpha' = \\frac{1}{\\alpha}\\) and
                            \\(\\beta' = -\\frac{\\beta}{\\alpha}\\)")))),
            tags$hr(),
           h5("Clementine Cottineau, 2016, University College London (CASA)."),
          h6("For any information / comment / suggestion, contact: c.cottineau@ucl.ac.uk"),
h6("Credits: T. Park from bootswatch.com for Flatly css file. R. Cura for technical help. E. Arcaute and M. Batty for overall design."),
tags$hr(),
h3("References:"), dataTableOutput('references')),
    
    tabPanel("Meta-Analysis",
             tabsetPanel(
               
               tabPanel("1. Data", 
                        h4("What is the Data for a MetaAnalysis?"),
                        "In a MetaAnalysis of Zipf for cities, we do not work directly with the cities' data.
  Instead, we use the estimations made by other researchers in published papers. The data is thus made by the result of their 
                        analysis along with the description of how they made the analysis. Each observation in our case is thus
                        composed of a single estimation of alpha (if you chose the Lotka form) or alpha' (if you chose the Pareto form) and is characterized by a dozen other variables.", 
                         withMathJax(h6("$$\\log(P_i) = \\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$ $$\\log(R_i) = \\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$ ")),  
                        h2("TERRITORY"), "The geographical extent from which cities were selected. For example: The Netherlands in Brakman et al. (1999). When available (i.e. between 1950 and 2015 for countries), the population of the territory was added.", br(),
                        h2("DATE & DECADE"), "The date (and decade) to which the cities' population refer.  For example: 1600 in Bretagnolle et al. (2000).", br(), 
                        h2("URBAN AGE"), "An indication whether urbanisation is a relatively recent or ancient phenomenon in the territory.  For example: 'OLD' for China.", 
                        " 'OLD' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. America, Oceania, Africa and central Asia are considered 'NEW' in that respect.",
                        h2("NUMBER OF CITIES"), "The number of cities used to estimate Zipf's coefficient. For example: 60 cities in Lepetit (1990).", br(),  
                        h2("CITY DEFINITION"), "The criteria used to identify cities. For example: MorphoCity in Guerin-Pace (1995).", br(), 
                        "LocalUnits correspond to administrative units. MorphoCities are aggregations of Local units based on density orthe built-up area. 
                        MetroAreas correspond to functional aggregations of Local units based on flows (typically commuters). 
                        VariaMixed indicate that the definition is either not uniform either uncommon.", br(), 
                        h2("POPULATION CUTOFF"), "The minimum population of the cities selected. For example: 5000 residents in Parr (1985).", br(),  
                        h2("DISCIPLINE"), "The disciplines in which the journal is recognised, according to the Chicago Journal Ranking SJR. For example: 'ECO' for the Quarterly Journal of Economics.", br(),  
                        "'ECO' refers to estimations published in journals classified in Economics, 'SOC' stands for Social Science and 'PHYS' for environmental and physical sciences journals. A journal can belong to one or more categories.",br(), 
                        h2("R2"), "The coefficient of Determination of the regression, indicating the quality of the fit. For example: 99% in Bretagnolle et al. (2008).", br(),  br(), 
                        #h2("REFERENCE"), "The reference from which the estimation is taken. For example: Singer (1936).", br(),  
                        h4("Data"),
                        h2("Subset Table by:"),
                        fluidRow(
                          column(4,selectizeInput("territory", "Country", "", multiple=T)),
                          column(4,selectizeInput("scale", "Urban Definition*", "", multiple=T)),
                          column(4,selectizeInput("decade", "Decade", "", multiple=T)),
                          column(3,downloadButton("downloadData", "Download")), 
                                 column(9, HTML('N.B. This table is a simplified version. You can find the full version here <a href="here">https://github.com/ClementineCttn/MetaZipf</a>'))),
                        dataTableOutput('review')
                                     
                        
               ),    

               
      tabPanel("2. Literature Overview",
             h4('TOP journals where the estimations* are drawn from:'),  
            dataTableOutput('topjournals'),
            '*Each reference count as one, irrespective of the number of estimations',  tags$hr(),
            h4('TOP authors* providing estimations:'), 
            dataTableOutput('topauthors'),
            '*the estimations published by the same author(s) in different publications are not added.',   tags$hr(),
            h4('TOP countries for the dispersion of results*:'),
            dataTableOutput('topcountries'),
            '*measured by the standard deviation of alpha for countries with more than 5 estimations.',  tags$hr(),
            h6("Nitsch, V. (2005). Zipf zipped. Journal of Urban Economics, 57(1), 86-100. Total population in thousands, from UN estimates (1950-2015) <http://esa.un.org/unpd/wpp/DVD/Files/1_Excel%20(Standard)/EXCEL_FILES/1_Population/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS.>")
            ),
    
     tabPanel("3. Estimates Summary", 
              h4("Summarise estimations by:"),
                 fluidRow(
                   column(4,selectizeInput("territorys", "Country", "", multiple=T)),
                   column(4,selectizeInput("scales", "Urban Definition*", "", multiple=T)),
                   column(4,selectizeInput("decades", "Decade", "", multiple=T)),
                   dataTableOutput('summary'),
                   h6('* The Urban Definition refers to the way cities are defined: locally as political units, morphologically as built-up agglomeration, 
                   functionally as metropolitan areas or a mix of these.'),
                   
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
              
             
               tabPanel("4. Meta Analysis",
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
                                             "Discipline" = "discipline", "", ""), selected = NULL, inline = FALSE)),
               tags$hr(),h4("Results of the Regression of Alpha by the Selected Features."),
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
               h6("N.B. 'Old' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. America, Oceania, Africa and central Asia are considered 'New' in that respect.
                  \n 'ECO' refers to estimations published in journals classified in Economics according to the Chicago Journal Ranking. 'SOC' stands for Social Science and 'PHYS' for environmental and physical sciences journals. A journal can belong to one or more categories.")
               
             ))
     )),
    
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
             column(6,textInput("contributor", "Your Name", value = ""))),
             tags$hr(),
             uiOutput(outputId = "nestimateRows")#,
          #   tags$hr(),
          #   column(4,textInput("comment", NULL, value = "Any Comment?")),
          #   column(4,textInput("from", NULL, value = "Your E-mail")),
           #  column(4,actionButton("sendMail", "Report your additions"))
     )
)
      )
    )
