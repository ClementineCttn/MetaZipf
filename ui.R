library(shiny)
library(leaflet)
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
             h2("Open Meta Review of Zipf's law with open urban data")),
  
  navlistPanel(
   
      
    tabPanel("Presentation",
      tabsetPanel(
      tabPanel("1. The Project",
            
             column(3, img(src = "favicon.png",class="img-responsive")),
             column(9, h1("Open Review and Meta Analysis of Empirical Zipf's Estimates")), 
             tags$hr(), 
            tags$p(class="text-justify",
            "This application aims at presenting a crowdsourced review and meta analysis of Zipf's law estimations from the literature. ",  br(), br(),
            
            "Indeed, this famous 'urban mystery' (Krugman, 1996) of a regular distribution of city sizes, found in many city systems at various points in history,
            has been an on-going subject of scientific discussion for a century. Despite hundreds of empirical evidences, it is still not clear if there is a universal
            Zipf's law,  - i.e. a power law relating city population to their rank in the system, with a exponent equal to 1 -, because in many cases, a value significantly different
from 1 is measured. Comparative papers (Singer, 1936; Rosen, Resnick, 1980; Parr, 1985; Soo, 2005) and meta analyses (Nitsch, 2005) provide a first idea of the magnitude of this variation and they try to
understand why deviations are observed: is it because this law simply does not apply in some countries (where control over migration is strong for example)? 
            Is it because the system is not consistently defined (Cristelli et al., 2012), in terms of territory, city definition, etc.? 
            Is it because the city size inequality tends to evolve over time (Pumain, 1997)?", br(), br(),
            
            "Building on the work of V. Nitsch (2005), we bring it further by:",
            tags$ul(
              tags$li("creating a much larger review of empirical papers, 
            in particular through crowdsourcing to enhance its representativity across dates, areas and study fields."),
              tags$li("making this database open for other researchers."),
              tags$li("testing a broader pool of hypotheses regarding the variability of the exponent measured,
so that eventually we could distinguish between the theoretical reasons for variations from the technical biases affecting the measurement of this power law."),
              tags$li("making this meta analysis transparent, interactive and adaptable to future additions and hypotheses.")),
        
            tags$b("The present application allows interactive queries into a pool of empirical papers to represent the variation of empiricial estimations of Zipf's 
                  law's exponent in the literature, with respect to the systems of cities studied and meta-information about the publication. The meta analysis 
        considers urban characteristics (age of the system, total population), the specifications of the regression used (urban definitions,
              truncation points, number of cities) and the discipline of the journal publishing the paper to unveil systematic deviations from the iconic 1 value
                   (more precision in tab 
                   'Meta-Analysis / Data')."), br(),   br(),
             HTML('The current database covers 1661 estimations from 79 studies, spanning over more than 80 countries over 400 years. 
            It is open for download (<a href="https://github.com/ClementineCttn/MetaZipf">https://github.com/ClementineCttn/MetaZipf</a>) and you are
            strongly invited to contribute by submitting your own empirical estimates using the \'Contribute !\' tab on the left.'), br(),   br(),
            h5("Before starting, choose a regression form into which all subsequent results will be expressed.")),
                      
            tags$hr(),
            
            fluidRow(column(6,selectInput("alpha", "I prefer results to be expressed in the regression form of:", 
                                          choices=c("Lotka", "Pareto"), multiple=FALSE)),
            column(6,
                   withMathJax(h6("The Lotka form : $$\\log(P_i) = -\\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$")),
                   withMathJax(h6("The Pareto form : $$\\log(R_i) = -\\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$")),
             withMathJax(h6("with: \\(P_i\\) the population of city \\(i\\),
                            \\(R_i\\) its rank in the urban hierarchy,
                            \\(\\alpha' = \\frac{1}{\\alpha}\\) and
                            \\(\\beta' = -\\frac{\\beta}{\\alpha}\\)")))),
           
   tags$hr(),
   h5("Clémentine Cottineau, 2016, University College London (CASA)."),
   h6("For any information / comment / suggestion, contact: c.cottineau@ucl.ac.uk"),
   h6("Credits: T. Park from bootswatch.com for Flatly css file."),
   h6('Nitsch, V. (2005). Zipf zipped. Journal of Urban Economics, 57(1), 86-100')
),

tabPanel("2. An Example of Zipf's law for cities",
         h3("Explore variations of Zipf estimations with..."),
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
                        h4('Coverage by Continent:'),
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
    
     tabPanel("2. Estimates distribution",
              br(),
             
              fluidRow(
                column(9,"Although Zipf's Law states that alpha should be equal to 1,
              empirical estimations are found to be distributed widely around this value. 
              Our meta analysis precisely looks for explanations for this diversity, by relating the value
              of alpha to some characteristics of the urban system (territory, population, age, type of cities) 
              and to potential biases (city definition, discipline, etc.)."),
                column(3,selectInput('alphaVarToMap', 'Alpha Statistics', 
                                     choices = c("Mean Alpha" = 'meanAlpha',
                                                 "Standard Deviation" = 'diversity',
                                                 "Number of Estimations" = 'n'),
                                     selected = "Mean Alpha", multiple = F))),
              
              tags$hr(), 
              h6('!! It might take a few seconds to load and update !!'),
              leafletOutput('worldmap') ,  tags$hr(),
              h4("Distribution of estimations"),
              h2("Subset by:"),
              fluidRow(
                column(4,selectizeInput("territorys", "Territory", "", multiple=T)),
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
     tabPanel("3. The References",
              h4("Bibliographical References"), dataTableOutput('references')
     )

     )),
     
     
     
     tabPanel("Meta-Analysis",
              tabsetPanel(
                
                
                tabPanel("1. Hypotheses"),
     tabPanel("2. Data", 
              h4("Data and Hypotheses for a MetaAnalysis of Zipf"),
              "In a meta-analysis of Zipf for cities, we do not work directly with the cities' data.
              Instead, we use the estimations made by other researchers in published papers. The data is thus made by the result of their 
              analysis along with the description of how they made the analysis. Each observation in our case is thus
              composed of a single estimation of alpha (if you chose the Lotka form) 
              or alpha' (if you chose the Pareto form) and is characterized by half a dozen other variables.", 
              withMathJax(h6("$$\\log(P_i) = -\\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$ $$\\log(R_i) = -\\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$ ")),  
              h2("TERRITORY"), 
              h6("For example: The Netherlands in Brakman et al. (1999)."), "The geographical extent from which cities were selected. When available (i.e. between 1950 and 2015 for countries), the population of the territory was added.
              Given the diversity of countries with respect to economic development, culture and size, one might expect alpha to vary accordingly.", 
              
              h2("DATE & DECADE"),  h6("For example: 1600 in Bretagnolle et al. (2000)."), "The date (and decade) to which the cities' 
              population refer. This information can be used to test the hypothesis according to which systems of cities increase
              their level of hierarchy over time, everything else being equal.", br(), 
              h2("URBANISATION AGE"), h6("For example: 'OLD' for China."), 
              "An indication whether urbanisation is a relatively recent or ancient phenomenon in the territory.
              'OLD' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. 
              America, Oceania, Africa and central Asia are considered 'NEW' in that respect. It has been found in previous studies 
              (Moriconi-Ebrard, 1993) that countries of more recent urbanisation tend to generate a steeper hierarchy of cities.",
              h2("NUMBER OF CITIES"), h6("For example: 60 cities in Lepetit (1990)."),
              "The number of cities used to estimate Zipf's coefficient.", br(),  
              h2("CITY DEFINITION"),  h6("For example: MorphoCity in Guerin-Pace (1995)."),
              "The criteria used to identify cities. LocalUnits correspond to administrative units. 
              MorphoCities are aggregations of Local units based on density orthe built-up area. 
              MetroAreas correspond to functional aggregations of Local units based on flows (typically commuters). 
              VariaMixed indicate that the definition is heterogenous or uncommon.", br(), 
              h2("POPULATION CUTOFF"),  h6("For example: 5000 residents in Parr (1985)."),
              "The minimum population of the cities selected. This cutoff is know to affect other properties of cities such 
              as scaling behaviours.", br(),  
              h2("DISCIPLINE"),  h6("For example: 'ECO' for the Quarterly Journal of Economics."),
              "The disciplines in which the journal is recognised, according to the Chicago Journal Ranking SJR. ", br(),  
              "'ECO' refers to estimations published in journals classified in Economics, 
              'SOC' stands for Social Science and 'PHYS' for environmental and physical sciences journals. 
              A journal can belong to one or more categories. We include this information as a test for disciplinary biases.
              It could be possible for example that some journals want to validate Zipf's Law with an exponent alpha of 1, whereas
              another journal would be biased towards publishing more refutations of this law, etc.",br(), 
              h2("R2"),  h6("For example: 99% in Bretagnolle et al. (2008)."),
              "The coefficient of Determination of the regression, indicating the quality of the fit. ", br(),  br(), 
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
              tags$hr(),
              h6(HTML('Total population in thousands, from UN estimates (1950-2015) 
                      <a href=http://esa.un.org/unpd/wpp/Download/Standard/Population/">http://esa.un.org/unpd/wpp/Download/Standard/Population/</a>')
              )
              
              ),    
     
               tabPanel("3. Results",
                        br(),
                        "Let's test quantitatively some assumptions about the relation between 
                          estimated alphas and some characteristics of the urban system 
                        (territory, population, age, type of cities) as well as
                         potential biases (city definition, discipline, etc.).",
                        
                        tags$hr(),
                        "To this end, we use a multiple linear regression. We regress 
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
                        the second value indicates the lower bound of the third category.", br(),br(),
                        h6("For example, if you click on 'Population Cutoff', a slider appears.
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
               column(4,checkboxGroupInput("technicalSpecs", "Technical Specifications", 
                                           c("All" = "alltech",
                                             "City definition" = "scale4model", 
                                              "Population Cutoff" = "truncation4model",
                                              "Number of Cities" = "N4model"), selected = NULL, inline = FALSE)),
               column(4,checkboxGroupInput("topicalSpecs", "Topical Specifications", 
                                           c("All" = "alltop",
                                             "Age of Urbanisation" = "urbanisation4model",
                                             "Date of Observation" = "year4model",
                                             "Country Population" = "countrySize"), selected = NULL, inline = FALSE)),
               column(4,checkboxGroupInput("otherSpecs", "Other Specifications", 
                                           c("All" = "allother",
                                             "Discipline" = "discipline", 
                                             "Type of territory" = "country",""), selected = NULL, inline = FALSE)),
               
               column(4,conditionalPanel(
                 condition = 'input.technicalSpecs.indexOf("truncation4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1', 
                 sliderInput("truncVal", "Population Cutoff (to define high, medium and low truncatures)",
                             min = 0, max = 1000000, value = c(10000, 100000)))),
               column(4,conditionalPanel(
                 condition = 'input.technicalSpecs.indexOf("N4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1',
                 sliderInput("NVal", "Number of cities (to define large, medium and small samples)",
                             min = 1, max = 1000, value = c(30, 300)))),
               column(4,conditionalPanel(
                 condition = 'input.topicalSpecs.indexOf("countrySize") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                 sliderInput("PopVal", "Country Population (x 1000, to define large, medium and small countries)",
                             min = 1, max = 1000000, value = c(10000, 100000))))),
             fluidRow(
               tags$hr(),
               h4("Model Fit"),
               "The accuracy of the model is given by the R2 value, but it also depends on the the number of observations (here, the estimations from the literature). 
               Because a consistent information is not fully available in all the papers published, 
               the number of observations decreases when the number of explaning variables increases.", br(), br(),
               tableOutput('modelparameters'),
               tags$hr(), h4("Results"),
               tableOutput('model'),
               "To interpret the results, please note that:", br(), br(),
               tags$ul(
                 tags$li("You should start by looking at the significance values ", tags$b("'Pr(>|t|)'"), " and interpret only
                         the value of the estimates for which the probability of error is the lowest."), 
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
    
      tabPanel("Contribute !",
             h1("Add your estimates"), 
             "Noone can read all the literature and this one is particularly vast and fast-moving. 
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
