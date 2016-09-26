library(shiny)
library(leaflet)
shinyUI(
  fluidPage(theme = "flatly_bootstrap.css",
            tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
            tags$head(includeScript("www/google_analytics.js")),
            tags$head(
              tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Orbitron|Cabin:400,700');
    "))
            ),
            headerPanel("MetaZipf"),
            #   headerPanel(),
    
  titlePanel(
             h2("Open Meta Review of Zipf's law with open urban data")),
  
  navlistPanel(
   
      
    tabPanel("About",
      tabsetPanel(
      tabPanel("1. The Project",
            
             column(3, img(src = "favicon.png",class="img-responsive")),
             column(9, h1("Open Review and Meta Analysis of Empirical Zipf's Estimates")), 
             tags$hr(), 
            tags$p(class="text-justify",
            "This application aims at presenting a crowdsourced review and meta analysis of Zipf's law estimations from the literature. ",  br(), br(),
            
            "Indeed, this famous 'urban mystery' (Krugman, 1996) of a regular distribution of city sizes, found in many city systems at various points in history,
            has been an on-going subject of scientific discussion for a century. Despite hundreds of empirical evidences, it is still not clear whether there is a universal
            Zipf's law (i.e. a power law relating city population to their rank in the system, with a exponent equal to -1) because in many cases, a value significantly different
from 1 is measured. Comparative papers (Singer, 1936; Rosen, Resnick, 1980; Parr, 1985; Soo, 2005) and meta analyses (Nitsch, 2005) provide a first idea of the magnitude of this variation and they try to
understand why deviations are observed: is it because this law simply does not apply in some countries (where controls over migrations are strong for example)? 
            Is it because the system is not consistently defined (Cristelli et al., 2012), in terms of territory, city definition, etc.? 
            Is it because the city size inequality tends to increase over time (Pumain, 1997)?", br(), br(),
            
            "Building on the work of V. Nitsch (2005), we bring it further by:",
            tags$ul(
              tags$li("creating a much larger review of empirical papers, 
            in particular through crowdsourcing to enhance representativity across dates, areas and study fields."),
              tags$li("making this database open."),
              tags$li("testing a broader pool of hypotheses regarding the variability of the exponent measured,
to distinguish topical differences from technical biases affecting the measurement."),
              tags$li("making this meta analysis transparent, interactive and adaptable to future developments.")),
        
            tags$b("The present application allows interactive queries into a pool of empirical papers to represent the variation of empiricial estimations of Zipf's 
                  law's exponent in the literature, with respect to the systems of cities studied and meta-information about the publication. The meta analysis 
        considers urban characteristics (age of the system, total population), the specifications of the regression used (urban definitions,
              truncation points, number of cities) and the discipline of the journal publishing the paper to unveil systematic deviations from the iconic 1 value."), br(),   br(),
             HTML('The current database covers 1702 estimations from 81 studies, spanning over more than 80 countries over 400 years. 
            It is open for download (<a href="https://github.com/ClementineCttn/MetaZipf">https://github.com/ClementineCttn/MetaZipf</a>) and you are
            strongly invited to contribute by submitting your own empirical estimates using the \'Contribute !\' form.'), br(),   br(),
            h5("Before starting, choose the way results will be expressed.")),
                      
            tags$hr(),
            
            fluidRow(column(6,selectInput("alpha", "I prefer results to be expressed in the regression form of:", 
                                          choices=c("Lotka", "Pareto"), multiple=FALSE)),
            column(6,
                   withMathJax(h6("The Lotka form : $$\\log(P_i) = -\\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$")),
                   withMathJax(h6("The Pareto form : $$\\log(R_i) = -\\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$")),
             withMathJax(h6("with: \\(P_i\\) the population of city \\(i\\),
                            \\(R_i\\) its rank in the urban hierarchy,
                            \\(\\alpha' = \\frac{1}{\\alpha}\\) and
                            \\(\\beta' = -\\frac{\\beta}{\\alpha}\\)")))
            ),
            tags$hr(),
            fluidRow(column(6,checkboxInput("Arxiv","Reproduce the analyses of the arXiv paper (reference collection ends in June 2016)",value = TRUE)),
                     column(6,HTML('Cottineau C. , 2016, « MetaZipf. (Re)producing knowledge about city size distributions », Arxiv.org, <a href="https://arxiv.org/abs/1606.06162">https://arxiv.org/abs/1606.06162</a>'))),
           
      
   tags$hr(),
   h5("Clémentine Cottineau, 2016, University College London (CASA)."),
   h6("For any information / comment / suggestion, contact: c.cottineau@ucl.ac.uk"),
   h6("Credits: T. Park from bootswatch.com for Flatly css file."),
   h6('Nitsch, V. (2005). Zipf zipped. Journal of Urban Economics, 57(1), 86-100')
),

tabPanel("2. An Example of Zipf's law for cities",
         h6("Empirical studies are based on urban databases, and the measurement of Zipf's coefficient
            may be affected by the sample size, date and definition of cities used, as in the example
            of Soviet cities below. "),
         tags$hr(),  h3("Explore variations of Zipf estimations with..."),
         fluidRow(
           
         column(4,selectInput("dariusyear", "Year", choice=c(2010, 2002, 1989, 1979, 1970, 1959, 1939, 1926, 1897), multiple=F)),
         column(4,sliderInput("dariuscutoff", "Minimum Population", min = 10000, max = 1000000, value = 10000)),
         column(4,selectInput("dariusdef", "Urban Definition", choices = c("Local", "Morpho"), selected = "Morpho")),
         column(12,dataTableOutput('DARIUSestim')),column(12, plotOutput('DARIUSgraph')), 
         
         column(12,    leafletOutput("DARIUS")),#plotOutput('DARIUSmap')),
         column(12, h6(HTML('DARIUS Dataset, 
                               Cottineau C. (2014), figshare.
<a href=https://figshare.com/articles/DARIUS_Database/1108081/1">https://figshare.com/articles/DARIUS_Database/1108081/1</a>')
         ))
))
)),
    
    tabPanel("100 years of publication",
             tabsetPanel(
               
               tabPanel("1. Literature Overview",
                        h6("This tab summarises the extent of studies of Zipf's law for cities in the literature."),
                        tags$hr(),   h4('Coverage by Continent:'),
                        dataTableOutput('continent'),
                       
                        h4('TOP journals where the estimations* are drawn from:'),  
                        '*Each reference count as one, irrespective of the number of estimations',  tags$hr(),
                        dataTableOutput('topjournals'),
                        h4('TOP authors* providing estimations:'), 
                        '*the estimations published by the same author(s) in different publications are not added.',   tags$hr(),
                        dataTableOutput('topauthors'),
                        
                        h4('TOP countries for the dispersion of results*:'),
                        '*measured by the standard deviation of alpha for countries with more than 5 estimations.',  tags$hr(),
                        dataTableOutput('topcountries')
                        
               ),
    
     tabPanel("2. Where & When",
              h6("This law has been estimated for a wide array of countries and periods, with particular
                 focuses on cases with reliable available data (i.e. developed countries in recent years)."),
              tags$hr(),  fluidRow(
                column(9,h4("Geographical Distribution of estimations"),
                       h6('!! It might take a few seconds to load and update !!')),
             
                column(3,selectInput('alphaVarToMap', 'Alpha Statistics', 
                                     choices = c("Mean Alpha" = 'meanAlpha',
                                                 "Standard Deviation" = 'diversity',
                                                 "Number of Estimations" = 'n'),
                                     selected = "meanAlpha", multiple = F))),
              
              tags$hr(), 
              leafletOutput('worldmap'),
              h6('Click on the countries to see all the estimated values'),
              tags$hr(), 
              h4('Temporal Coverage'),
              plotOutput('temporal')
            
                   ),
     tabPanel("3. Distribution",
              h6("Although Zipf's Law states that alpha should be equal to 1,
              empirical estimations are found to be distributed widely around this value.
                 The meta analysis precisely looks for explanations for this diversity, by relating the value
                 of alpha to some characteristics of the urban system (territory, population, age, type of cities)
                 and to potential biases (city definition, discipline, etc.)."),
              tags$hr(),  h4("Statistical Distribution of estimations"),
             
              h2("Subset by:"),
              fluidRow(
                column(4,selectizeInput("territorys", "Continent", "", multiple=T)),
                column(4,selectizeInput("scales", "City Definition", "", multiple=T)),
                column(4,selectizeInput("decades", "Decade", "", multiple=T)),
                plotOutput('histalpha')), br(), 
              tags$hr(),
              tags$hr(),
              h4("Summary Statistics"),
              fluidRow( 
                column(6,dataTableOutput('summaryAlpha')),
                column(6,dataTableOutput('summaryMeta')))
            
              
     ),
   
     tabPanel("4. References",
              h6("The analysis is based on estimations found in the following references."),
              tags$hr(),  h4("Bibliographical References"), dataTableOutput('references')
     )

     )),
     
     
     
     tabPanel("Static Meta-Analysis",
              tabsetPanel(
                
                tabPanel("1. Hypotheses",
                         h6("'Almost no data set corresponds exactly to the rank-size rule, so interpretations are based on how the data set diverges from the expected results' - Savage, 1997"),
                         
                         tags$hr(), 
                         h4("Hypotheses to explain deviations from Zipf's Law"),
                         h2("1. Recently urbanised systems are more unequal than ancient ones"),
                        h6("Because the transport networks available at the time of urbanisation were slower in old systems, a larger amount of small cities were necessary. In areas urbanised with railroads and highways, these small cities tend to have been short-circuited."),
                        h6("References: Moriconi-Ebrard, 1993; Pumain, 1997"),
                        h2("2. Small territories are more unequal than large ones"),
                        h6("Because the concentration of power in the largest cities is not balanced by a sufficiently large set of secondary cities."),
                        h6("References: Morrill, 1970; Rosen & Resnick, 1980"),
                        h2("3. Metropolitan areas (and built-up areas to a lesser extent) are more unevenly populated than 'city propers'"),
                        h6("Because the inclusion of suburbs causes a larger proportional increase in large cities than for small city populations, increasing the unevenness measured."),
                        h6("References: Auerbach, 1913; Rosen & Resnick, 1980; Soo, 2005; Nitsch, 2005"),
                        h2("4. Systems of cities tend to be more uneven with time"),
                        h6("Because larger cities grow faster on average, due to first-mover advantages in innovations."),
                        h6("References: Pumain, 1997; Nitsch, 2005"),
                        h2("5. Loosely-integrated systems deviate more than integrated ones"),
                        h6("Because they duplicate small to medium-sized cities and/or lack a primate city."),
                        h6("References: Harris, 1970; Johnson, 1977; Rosen & Resnick, 1980"),
                        h2("6. Smaller sets of cities are more likely to deviate from Zipf's law"),
                        h6("Because they do not represent sufficiently well the complete distribution of cities."),
                        h6("References: Rosen & Resnick, 1980; Critelli et al, 2012"),
                        h2("7. The minimum population used to define cities affects the measure"),
                        h6("Because empirical rank-size distributions can be convex or concave."),
                        h6("References: Rosen & Resnick, 1980; Moriconi-Ebrard, 1993; Savage, 1997; Soo, 2005")#,
                        # h2("8. "),
                        # h6("Because ."),
                        # h6("References: ")
                        
                         ),
     tabPanel("2. Data", 
              h6("In a meta-analysis, we do not work directly with the cities' data: we use the estimations published by other researchers. The data is thus made by the result of their 
              analysis along with the description of how they made the analysis. Each observation is 
                 composed of a single estimation of alpha and half a dozen other variables."), 
              tags$hr(), h4("Data for a MetaAnalysis of Zipf"),
              #  withMathJax(h6("$$\\log(P_i) = -\\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$ $$\\log(R_i) = -\\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$ ")),  
               
              h2("DATE"),  h6("For example: 1600 in Bretagnolle et al. (2000)."), "The date (and decade) to which the cities' 
              population refer. This information can be used to test the hypothesis according to which systems of cities increase
              their level of hierarchy over time, everything else being equal.", br(), 
              h2("URBANISATION AGE"), h6("For example: 'OLD' for China."), 
              "An indication whether urbanisation is a relatively recent or ancient phenomenon in the territory.
              'OLD' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. 
              America, Oceania, Africa and central Asia are considered 'NEW' in that respect.",
              h2("NUMBER OF CITIES"), h6("For example: 60 cities in Lepetit (1990)."),
              "The number of cities used to estimate Zipf's coefficient.", br(),  
              h2("CITY DEFINITION"),  h6("For example: MorphoCity in Guerin-Pace (1995)."),
              "The criteria used to identify cities. LocalUnits correspond to administrative units. 
              MorphoCities are aggregations of Local units based on density orthe built-up area. 
              MetroAreas correspond to functional aggregations of Local units based on flows (typically commuters). 
              VariaMixed indicate that the definition is heterogenous or uncommon.", br(), 
              h2("POPULATION CUTOFF"),  h6("For example: 5000 residents in Parr (1985)."),
              "The minimum population of the cities selected.", br(),  
              # h2("DISCIPLINE"),  h6("For example: 'ECO' for the Quarterly Journal of Economics."),
              # "The disciplines in which the journal is recognised, according to the Chicago Journal Ranking SJR. ", br(),  
              # "'ECO' refers to estimations published in journals classified in Economics, 
              # 'SOC' stands for Social Science and 'PHYS' for environmental and physical sciences journals. 
              # A journal can belong to one or more categories. We include this information as a test for disciplinary biases.",br(), 
              h2("TERRITORY"), 
             "We characterize different territories based on their population and type (national boundaries, subnational regions or supra-national macroRegions).", 
             h2("COUNTRY POPULATION"), 
             HTML('Total population in thousands, from UN estimates (1950-2015) 
                      <a href=http://esa.un.org/unpd/wpp/Download/Standard/Population/">http://esa.un.org/unpd/wpp/Download/Standard/Population/</a>'),
             h2("COUNTRY GDP"), 
             HTML('GDP per capita in current US $, from World Bank estimates (1960-2015) 
                     <a href=http://data.worldbank.org/indicator/NY.GDP.PCAP.CD">http://data.worldbank.org/indicator/NY.GDP.PCAP.CD</a>'),
             
             #h2("R2"),  h6("For example: 99% in Bretagnolle et al. (2008)."),
              #"The coefficient of Determination of the regression, indicating the quality of the fit. ",
             br(),  br(), 
              #h2("REFERENCE"), "The reference from which the estimation is taken. For example: Singer (1936).", br(),  
              h4("Data"),
              h2("Subset Table by:"),
              fluidRow(
                column(4,selectizeInput("territory", "Territory", "", multiple=T)),
                column(4,selectizeInput("scale", "City Definition", "", multiple=T)),
                column(4,selectizeInput("decade", "Decade", "", multiple=T)),
                column(3,downloadButton("downloadData", "Download")), 
                column(9, HTML('N.B. This table is a simplified version. You can find the full version here <a href="here">https://github.com/ClementineCttn/MetaZipf</a>'))),
             dataTableOutput('review'),
          
             tags$hr()
             
             
              
              ),    
     
               tabPanel("3. Results",
                       h6("We use a multiple linear regression to test our hypotheses about the relation between 
                          estimated alphas and some characteristics of the urban system 
                        (territory, population, age, type of cities) and
                         potential biases (city definition, discipline, etc.)."), tags$hr(), 
                       "Technically, we regress 
                        the value of alpha by the value of other 
                        characteristics Y of i, 
                        resulting in the estimation of an intercept and a vector of 
                        coefficients b, one for each characteristic Y, indicating the
                        intensity with which the value of alpha varies following a variation
                        of the value of Y.",
                        withMathJax(h6("$$\\alpha_i = Intercept + b * Y_i $$")),
                      "For comparability reasons, most of the characteristics Y 
                        that we consider have been discretised into three ordinal categories.
                        You can modify the parameters of each discretisation, using
                        the sliders corresponding to the quantitative variables (when activated). 
                        The first value in blue indicates the upper bound of the first category and
                        the second value indicates the lower bound of the third category.", 
                       tags$hr(),  h6("For example, if you click on 'Population Cutoff', a slider appears.
                           By default, it is set to distinguish three types of estimations from the literature 
                           with respect to the minimum population they consider for cities: "),
                        tags$ul(
                          tags$li(h6("Estimations where the 
                          full spectrum of city sizes are considered, even cities with a population of 10,000 residents
                                     (or less).")), 
                          tags$li(h6("Estimations where the minimum population for cities is comprised between 10,000 and 100,000 
                          residents.")), 
                          tags$li(h6("Estimations where the rank-size relation is applied to large cities only 
                            (with a population cutoff over 100,000)."))),
                        
                        tags$hr(),
                       
                        
             h4("Select Features for the Meta Analysis"),
             fluidRow(
               column(4,checkboxGroupInput("technicalSpecs", "Regression Specifications", 
                                           c("All" = "alltech",
                                              "Population Cutoff" = "truncation4model",
                                              "Number of Cities" = "N4model",
                                             "City definition" = "scale4model", 
                                             "Regression Form" = "regForm"), selected = NULL, inline = FALSE)),
                 column(4,checkboxGroupInput("otherSpecs", "Study Specifications", 
                                           c("All" = "allother",
                                             "Year of Publication" = "yearOfPubli", 
                                             "Study Size" = "studySize", 
                                             "Length of Period Analyzed" = "studyPeriod",
                                             #"Discipline" = "discipline",
                                             "Coverage of territories" = "studyCoverage"), selected = NULL, inline = FALSE)),
               column(4,checkboxGroupInput("topicalSpecs", "Territorial Variables", 
                                           c("All" = "alltop",
                                             "Type of territory" = "country",
                                             #  "Date of Observation" = "year4model",
                                             "Age of Urbanisation" = "urbanisation4model",
                                             "Country Population" = "countrySize",
                                             "Country GDP per capita" = "countryGDP"), selected = NULL, inline = FALSE)),
               
               column(6,checkboxInput("fixedEffects", "Fixed Study Effects", value = F)),
               column(6,checkboxInput("sameSample", "Compare models with the same observations", value = F)),
               
                column(4,conditionalPanel(
                 condition = 'input.technicalSpecs.indexOf("truncation4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1', 
                 sliderInput("truncVal", "Population Cutoff (bounds of the medium reference class)",
                             min = 0, max = 1000000, value = c(10000, 100000)))),
               column(4,conditionalPanel(
                 condition = 'input.technicalSpecs.indexOf("N4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1',
                 sliderInput("NVal", "Number of cities (bounds of the medium reference class)",
                             min = 1, max = 1000, value = c(30, 300)))),
               column(4,conditionalPanel(
                 condition = 'input.topicalSpecs.indexOf("countrySize") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                 sliderInput("PopVal", "Country Population (x 1000, bounds of the medium reference class)",
                             min = 1, max = 1000000, value = c(10000, 100000)))),
             column(4,conditionalPanel(
               condition = 'input.topicalSpecs.indexOf("countryGDP") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
               sliderInput("GDPVal", "GDP per Capita (current US$, bounds of the medium reference class)",
                           min = 1, max = 100000, value = c(1000, 10000)))),
             column(4,conditionalPanel(
               condition = 'input.otherSpecs.indexOf("yearOfPubli") != -1 || input.otherSpecs.indexOf("allother") != -1',
               sliderInput("yearOfP", "Year of Publication (bounds of the medium reference class)",
                           min = 1925, max = 2015, value = c(1975, 2000)))),
             column(4,conditionalPanel(
               condition = 'input.otherSpecs.indexOf("studyPeriod") != -1 || input.otherSpecs.indexOf("allother") != -1',
               sliderInput("s_period", "Length of the study period (boundary between short and long periods)",
                           min = 2, max = 200, value = 50))),
             column(4,conditionalPanel(
               condition = 'input.otherSpecs.indexOf("studyCoverage") != -1 || input.otherSpecs.indexOf("allother") != -1',
               sliderInput("n_territories", "Territories covered by the study (boundary between few and many)",
                           min = 2, max = 200, value = 5))),
             column(4,conditionalPanel(
               condition = 'input.otherSpecs.indexOf("studySize") != -1 || input.otherSpecs.indexOf("allother") != -1',
               sliderInput("n_estim", "Number of estimations in the study (boundary between small and large studies)",
                           min = 2, max = 100, value = 10)))),
     
             fluidRow(
               tags$hr(),
               h4("Model Fit"),
               "The accuracy of the model is given by the R2 value, but it also depends on the the number of observations (here, the estimations from the literature). 
               Because a consistent information is not fully available in all the papers published, 
               the number of observations decreases when the number of explaning variables increases.", br(), br(),
               tableOutput('modelparameters'),
             #  tags$b("Are fixed study effects needed? (based on pFtest)"),
              # textOutput('pFtest'),
               tags$hr(), h4("Results"),
               tags$b("Intercept and Time Coefficient"),
               tableOutput('model_temporal'),
               tags$b("Significant Coefficients"),
               tableOutput('model_significant'), 
               sliderInput("significance", "Significance level (%)", min = 0.1, max = 10, value = 5),
               tags$b("Non-significant Coefficients"),
               tableOutput('model_non_significant'),tags$hr(), 
               "To interpret the results, please note that:", br(), br(),
               tags$ul(
                  tags$li("The ", tags$b("Intercept"), " gives the average value of alpha predicted for 
                         an urban system characterized by the reference categories selected. If no
                         characteristics are selected, this value is the mean alpha measured over all studies.",
                        
                         htmlOutput('REFS'),  h6(
                   "For example, if 'Population Cutoff' is selected, it will give the average
                   value of alpha expected for studies using a high population cutoff, i.e. studies
                   where only large cities are considered."
                 )), 
                 tags$li("The coefficient for ", tags$b("Discrete Variables"), " (population cutoff, 
                          city definition, etc.), when added to the 
                         intercept, gives the average value of alpha for studies belonging to the 
                         category under consideration (compared to studies using the reference category).",
                        h6(
                           "For example and by default, if 'Population Cutoff' is selected, 
                           the estimate for 'Low' indicates that, on average, studies covering a wider range of city sizes
                           find Zipf coefficients indicating a larger size disparity than studies using data on
                           large cities only (Population Cutoff > 100,000 residents)."
                         )), 
                 tags$li("The coefficient associated with the '", tags$b("Date of observation"), "' indicates how much alpha varies
                         on average for each year added to the date of observation, these dates being centred on 1950. 
                         If the rank-size properties of a system do not change over time, this coefficient should be
                         equal to 0. On the contrary, significant non-zero coefficients indicate tendencies towards hierarchisation or equalisation
                         of city sizes over time.",
                         h6(
                           "For example, alpha estimations for cities in 1950 exhibit, on average, the value
                           of the intercept plus the coefficients of the categories to which they belong. 
                          Alpha estimations for cities in 1970 exhibit, on average, the value
                           of the intercept plus twenty times the value of the 'Date of observaton' coefficient,
                           plus the coefficients of the categories to which they belong."
                         )), 
                 tags$li("The coefficients associated with the", tags$b("Disciplines"), "(ECO, SOC & PHYS) indicate the quantity to 
                         be added to the average alpha for studies published in journals of the given category,
                         compared to studies published in journals non categorised as such.")
                 )
               )
             
              
                # ,
               # h4("Visualise variation of alpha with:"),
               # fluidRow(
               #   column(4,selectInput("quanti", "Continuous variable (y)", choices=c("N", "TRUNCATION_POINT", "DATE"), multiple=FALSE)),
               #   column(4,checkboxInput("log", "Log(y)", value=TRUE)),
               #   column(4,selectInput("quali", "Categorical variable (colour)", choices=c("URBANSCALE", "COUNTRY", "DECADE", "ECO", "SOC", "PHYS"), multiple=FALSE)),
               #   plotOutput('plot')
               # )
             )
     )),
tabPanel("Dynamic Analysis",
         tabsetPanel(
           tabPanel("1. Trajectories of urban hierarchy",
                    h2("Subset Table by:"),
                    fluidRow(
                      column(4,selectizeInput("territory_3", "Territory", "", multiple=T)),
                    plotOutput('trajectories')),   
                    br(), 
                    tags$hr(),
                    tags$hr(),
                    fluidRow(
                      column(6,h4("Growth of Alpha"),
                           h6('!! It might take a few seconds to load and update !!')),
                    column(6,h4("Contextual Growth"),
                           h6('!! It might take a few seconds to load and update !!')),
                    column(4,selectInput('dynVarToMap', 'Annual Average growth of Alpha (%)', 
                                         choices = c("Mean Value" = 'meanDynAlpha',
                                                     "Standard Deviation" = 'sdDynAlpha',
                                                     "Number of Observations" = 'nDynAlpha'),
                                         selected = "meanDynAlpha", multiple = F)),
                    column(4,selectInput("decade_3", "Decade", choices = c("1950s", "1960s","1970s","1980s","1990s","2000s", "2010s"),
                                         selected = c("1980s"), multiple=F)),
                      column(4,selectInput('contextToMap', 'Annual Average Growth context (%)', 
                                         choices = c("GDP per Capita" = 'g_GDP',
                                                     "Population" = 'g_pop'),
                                         selected = "g_GDP", multiple = F)),
                  
                   column(6,leafletOutput('mapectories')),
                   column(6,leafletOutput('mapcontext'))),
                   
                   
                    tags$hr()
           ),
           tabPanel("2. Projections"
           )
           )),

      tabPanel("Contribute !",
              
               h1("Add your estimates"), 
               "No-one can read all the literature and this one is particularly vast and fast-moving. 
              You might have published or reviewed estimates which are absent from the database in its current form.
                  Please add them here to improve the quality and representativity of this open database 
                  and meta analysis.",
             h5("Please remember to press the button 'Save' to save the reference and the estimates. When you do, you will be able
                to download the resulting formatted file. Do not forget to send this data with your potential comments
                to the moderator if you want them to be integrated to the meta analysis."),
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
                                  choices=c("LOTKA", "PARETO"), multiple=FALSE)), br(),br(),
             h6("*LOTKA: Pi ~ Ri. PARETO: Ri ~ Pi.")),
          
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
