##########################################
# MetaZipf | ui file
# Clementine Cottineau, 2016
# UCL - CASA - UDL
##########################################


library(shiny)
library(leaflet)



shinyUI(
  fluidPage(
    theme = "flatly_bootstrap.css",
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
    tags$head(includeScript("www/google_analytics.js")),
    tags$head(tags$style(
      HTML(
        "
        @import url('//fonts.googleapis.com/css?family=Orbitron|Cabin:400,700');
        "
      )
      )),
    headerPanel("MetaZipf"),
    titlePanel(h2(
      "A Dynamic and Open Meta Review of Zipf's law for cities"
    )),
    
    navlistPanel(
      tabPanel("A. About",
               tabsetPanel(
                 tabPanel(
                   "1. The Project",
                   
                   column(3, img(src = "favicon.png", class = "img-responsive")),
                   column(
                     9,
                     h1("Open Review and Meta Analysis of Empirical Zipf's Estimates")
                   ),
                   tags$hr(),
                   tags$p(
                     class = "text-justify",
                     "This application presents a dynamic and open meta analysis of Zipf's law, based on a large scale interactive review of urban literature.",
                     br(),
                     br(),
                     
                     "Indeed, this famous 'urban mystery' (Krugman, 1996) of a regular distribution of city sizes, found in many city systems at various points in history,
                     has been an on-going subject of scientific discussion for a century. Despite hundreds of empirical evidences, it is still not clear whether there is a universal
                     Zipf's law (i.e. a power law relating city population to their rank in the system, with a exponent equal to -1) because in many cases, a value significantly different
                     from 1 is measured. Comparative papers (Singer, 1936; Rosen, Resnick, 1980; Parr, 1985; Soo, 2005) and meta analyses (Nitsch, 2005) provide a first idea of the magnitude of this variation and they try to
                     understand why deviations are observed: is it because this law simply does not apply in some countries (where controls over migrations are strong for example)?
                     Is it because the system is not consistently defined (Cristelli et al., 2012), in terms of territory, city definition, etc.?
                     Is it because the city size inequality tends to increase over time (Pumain, 1997)?",
                     br(),
                     br(),
                     
                     "Building on the work of V. Nitsch (2005), we bring it further by:",
                     tags$ul(
                       tags$li(
                         "creating a much larger review of empirical papers,
                         in particular through crowdsourcing to enhance representativity across dates, areas and study fields."
                       ),
                       tags$li(
                         "testing a broader pool of hypotheses regarding the variability of the exponent measured. In particular,
                         we plug additional data to the metadatabase to describe the structure and evolution of the territories for which Zipf's law is estimated."
                       ),
                       tags$li(
                         "focusing on the evolution of city size unevenness measured by the alpha parameter of Zipf's law. We therefore
                         introduce the concept of a dynamic meta analysis."
                       ),
                       
                       tags$li(
                         "making this meta analysis transparent, open, interactive and adaptable to future developments."
                       )
                       ),
                     
                     tags$b(
                       "The present application allows interactive queries into a pool of empirical papers formatted in a comparable way. It provides a summary of the
                       numeric, spatial and temporal distribution of alpha found in this vast literature. It provides a static and dynamic meta analysis of Zipf's law and its evolution over time.
                       It disentangles the statistical effects of the regression specification, of fixed study effects, of characteristics of territories and their evolution,
                       as well as of historical events and public policies."
                     ),
                     br(),
                     br(),
                     HTML(
                       'The current database covers 1692 estimations from 81 studies, spanning over more than 80 countries over 400 years.
                       It is open for download (<a href="https://github.com/ClementineCttn/MetaZipf">https://github.com/ClementineCttn/MetaZipf</a>) and you are
                       strongly invited to contribute by submitting your own empirical estimates using the \'Contribute !\' form.'
                     ),
                     br(),
                     br(),
                     h5("Before starting, choose the way results will be expressed.")
                     ),
                   
                   tags$hr(),
                   
                   fluidRow(
                     column(
                       6,
                       selectInput(
                         "alpha",
                         "I prefer results to be expressed in the regression form of:",
                         choices = c("Lotka", "Pareto"),
                         multiple = FALSE
                       )
                     ),
                     column(6,
                            withMathJax(
                              h6(
                                "The Lotka form : $$\\log(P_i) = -\\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$"
                              )
                            ),
                            withMathJax(
                              h6(
                                "The Pareto form : $$\\log(R_i) = -\\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$"
                              )
                            ),
                            withMathJax(
                              h6(
                                "with: \\(P_i\\) the population of city \\(i\\),
                                \\(R_i\\) its rank in the urban hierarchy,
                                \\(\\alpha' = \\frac{1}{\\alpha}\\) and
                                \\(\\beta' = -\\frac{\\beta}{\\alpha}\\)"
                                )
                                ))
                                ),
                   tags$hr(),
                   fluidRow(column(
                     6,
                     checkboxInput(
                       "Arxiv",
                       "Reproduce the analyses of the arXiv paper (reference collection ends in June 2016)",
                       value = FALSE
                     )
                   ),
                   column(
                     6,
                     HTML(
                       'Cottineau C. , 2016, « MetaZipf. (Re)producing knowledge about city size distributions », Arxiv.org, <a href="https://arxiv.org/abs/1606.06162">https://arxiv.org/abs/1606.06162</a>'
                     )
                   )),
                   
                   
                   tags$hr(),
                   h5("Clémentine Cottineau, 2016, University College London (CASA)."),
                   h6(
                     "For any information / comment / suggestion, contact: c.cottineau@ucl.ac.uk"
                   ),
                   h6("Credits: T. Park from bootswatch.com for Flatly css file."),
                   h6(
                     'Nitsch, V. (2005). Zipf zipped. Journal of Urban Economics, 57(1), 86-100'
                   )
                                ),
                 
                 tabPanel(
                   "2. An Example of Zipf's law for cities",
                   h6(
                     "Empirical studies are based on urban databases, and the measurement of Zipf's coefficient
                     may be affected by the sample size, date and definition of cities used, as in the example
                     of Soviet cities below. "
                   ),
                   tags$hr(),
                   h3("Explore variations of Zipf estimations with..."),
                   fluidRow(
                     column(4, selectInput(
                       "dariusyear",
                       "Year",
                       choice = c(2010, 2002, 1989, 1979, 1970, 1959, 1939, 1926, 1897),
                       multiple = F
                     )),
                     column(
                       4,
                       sliderInput(
                         "dariuscutoff",
                         "Minimum Population",
                         min = 10000,
                         max = 1000000,
                         value = 10000
                       )
                     ),
                     column(
                       4,
                       selectInput(
                         "dariusdef",
                         "Urban Definition",
                         choices = c("Local", "Morpho"),
                         selected = "Morpho"
                       )
                     ),
                     column(12, dataTableOutput('DARIUSestim')),
                     column(12, plotOutput('DARIUSgraph')),
                     
                     column(12,    leafletOutput("DARIUS")),
                     #plotOutput('DARIUSmap')),
                     column(12, h6(
                       HTML(
                         'DARIUS Dataset,
                         Cottineau C. (2014), figshare.
                         <a href=https://figshare.com/articles/DARIUS_Database/1108081/1">https://figshare.com/articles/DARIUS_Database/1108081/1</a>'
                       )
                       ))
                       )
                     )
                   )),
      
      tabPanel(
        "B. 100 years of publication",
        tabsetPanel(
          tabPanel(
            "1. Literature Overview",
            h6(
              "This tab summarises the extent of studies of Zipf's law for cities in the literature."
            ),
            tags$hr(),
            h4('Coverage by Continent:'),
            dataTableOutput('continent'),
            
            h4('TOP journals where the estimations* are drawn from:'),
            '*Each reference count as one, irrespective of the number of estimations',
            tags$hr(),
            dataTableOutput('topjournals'),
            h4('TOP authors* providing estimations:'),
            '*the estimations published by the same author(s) in different publications are not added.',
            tags$hr(),
            dataTableOutput('topauthors'),
            
            h4('TOP countries for the dispersion of results*:'),
            '*measured by the standard deviation of alpha for countries with more than 5 estimations.',
            tags$hr(),
            dataTableOutput('topcountries')
            
          ),
          
          tabPanel(
            "2. Where & When",
            h6(
              "This law has been estimated for a wide array of countries and periods, with particular
              focuses on cases with reliable available data (i.e. developed countries in recent years)."
            ),
            tags$hr(),
            fluidRow(column(
              9,
              h4("Geographical Distribution of estimations"),
              h6('!! It might take a few seconds to load and update !!')
            ),
            
            column(
              3,
              selectInput(
                'alphaVarToMap',
                'Alpha Statistics',
                choices = c(
                  "Mean Alpha" = 'meanAlpha',
                  "Standard Deviation" = 'diversity',
                  "Number of Estimations" = 'n'
                ),
                selected = "meanAlpha",
                multiple = F
              )
            )),
            
            tags$hr(),
            leafletOutput('worldmap'),
            h6('Click on the countries to see all the estimated values'),
            tags$hr(),
            h4('Temporal Coverage'),
            plotOutput('temporal')
            
            ),
          tabPanel(
            "3. Distribution of Alpha",
            h6(
              "Although Zipf's Law states that alpha should be equal to 1,
              empirical estimations are found to be distributed widely around this value.
              The meta analysis precisely looks for explanations for this diversity, by relating the value
              of alpha to some characteristics of the urban system (territory, population, age, type of cities)
              and to potential biases (city definition, regression form, etc.)."
            ),
            tags$hr(),
            h4("Statistical Distribution of estimations"),
            
            h2("Subset by:"),
            fluidRow(
              column(4, selectizeInput("territorys", "Continent", "", multiple = T)),
              column(4, selectizeInput("scales", "City Definition", "", multiple =
                                         T)),
              column(4, selectizeInput("decades", "Decade", "", multiple =
                                         T)),
              plotOutput('histalpha')
            ),
            br(),
            tags$hr(),
            tags$hr(),
            h4("Summary Statistics"),
            fluidRow(column(6, dataTableOutput('summaryAlpha')),
                     column(6, dataTableOutput('summaryMeta')))
            
            
            ),
          
          tabPanel(
            "4. References",
            h6(
              "The analysis is based on estimations found in the following references."
            ),
            tags$hr(),
            h4("Bibliographical References"),
            dataTableOutput('references')
          )
          
          )
        ),
      
      
      
      tabPanel(
        "C. Static Meta-Analysis",
        tabsetPanel(
          tabPanel(
            "1. Hypotheses",
            h6(
              "'Almost no data set corresponds exactly to the rank-size rule, so interpretations are based on how the data set diverges from the expected results' - Savage, 1997"
            ),
            
            tags$hr(),
            h4("Hypotheses to explain deviations from Zipf's Law"),
            h2("1. Recently urbanised systems are more unequal than ancient ones"),
            h6(
              "Because the transport networks available at the time of urbanisation were slower in old systems, a larger amount of small cities were necessary. In areas urbanised with railroads and highways, these small cities tend to have been short-circuited."
            ),
            h6("References: Moriconi-Ebrard, 1993; Pumain, 1997"),
            h2("2. Small territories are more unequal than large ones"),
            h6(
              "Because the concentration of power in the largest cities is not balanced by a sufficiently large set of secondary cities."
            ),
            h6("References: Morrill, 1970; Rosen & Resnick, 1980"),
            h2(
              "3. Metropolitan areas (and built-up areas to a lesser extent) are more unevenly populated than 'city propers'"
            ),
            h6(
              "Because the inclusion of suburbs causes a larger proportional increase in large cities than for small city populations, increasing the unevenness measured."
            ),
            h6(
              "References: Auerbach, 1913; Rosen & Resnick, 1980; Soo, 2005; Nitsch, 2005"
            ),
            h2("4. Systems of cities tend to be more uneven with time"),
            h6(
              "Because larger cities grow faster on average, due to first-mover advantages in innovations."
            ),
            h6("References: Pumain, 1997; Nitsch, 2005"),
            h2("5. Loosely-integrated systems deviate more than integrated ones"),
            h6(
              "Because they duplicate small to medium-sized cities and/or lack a primate city."
            ),
            h6("References: Harris, 1970; Johnson, 1977; Rosen & Resnick, 1980"),
            h2("6. Smaller sets of cities are more likely to deviate from Zipf's law"),
            h6(
              "Because they do not represent sufficiently well the complete distribution of cities."
            ),
            h6("References: Rosen & Resnick, 1980; Critelli et al, 2012"),
            h2(
              "7. The minimum population used to define cities affects the measure"
            ),
            h6(
              "Because empirical rank-size distributions can be convex or concave."
            ),
            h6(
              "References: Rosen & Resnick, 1980; Moriconi-Ebrard, 1993; Savage, 1997; Soo, 2005"
            )#,
            # h2("8. "),
            # h6("Because ."),
            # h6("References: ")
            
          ),
          tabPanel(
            "2. Data",
            h6(
              "In a meta-analysis, we do not work directly with the cities' data: we use the estimations published by other researchers. The data is thus made by the result of their
              analysis along with the description of how they made the analysis. Each observation is thus
              composed of a single estimation of alpha, of variables describing the regression performed,
              of variables describing the study scope and of additional variables describing the territory within which alpha is estimated."
            ),
            tags$hr(),
            h4("Default Variables"),
            #  withMathJax(h6("$$\\log(P_i) = -\\alpha \\times \\log(R_i) + \\beta + \\epsilon_i$$ $$\\log(R_i) = -\\alpha' \\times \\log(P_i) + \\beta' + \\epsilon'_i$$ ")),
            
            h2("ALPHA"),
            "The scaling coefficient reported in the study or its transformed value, depending on the form chosen in the first tab (Lotka or Pareto).",
            br(),
            h2("DATE"),
            "The date of observation used in the meta analysis is the date to which the cities' population refer.",
            br(),
            h2("TERRITORY"),
            "The territory to which the cities' population refer.",
            br(),
            
            h4("Estimation variables"),
            h2("CITY DEFINITION"),
            "City definitions are a categorization of original definitions reported in the papers. LocalUnits correspond to all types of local administrative units. MorphoCities are aggregations of Local units based on density or the built-up area. MetroAreas correspond to functional aggregations of Local units based on flows (typically commuters as in the American SMAs). VariaMixed indicate that the definition is heterogenous or uncommon.
            ",
            br(),
            h2("POPULATION CUTOFF"),
            "The population cutoff of an estimation corresponds to the minimum population (if any) of the cities selected to estimate Zipf's coefficient.
            ",
            br(),
            h2("NUMBER OF CITIES"),
            "The number of cities refers to the number of observations (cities) used to estimate Zipf's coefficient.
            ",
            br(),
            h2("ESTIMATION METHOD"),
            "The estimation method is a categorical variable which can take the value 'OLS' if the estimation was performed
            using the Ordinal Least Squares Method on logged varibles, 'GI' if it used Gabaix & Ibragimov's trick of replacing
            ranks by <ranks - 1/2>, 'MC' if it used Markov Chains and 'ML' if it used Maximum Likelihood Estimation.
            ",
            br(),
            h2("REGRESSION FORM"),
            "The regression form is a categorical variable which can take the value 'Lotka' if the estimation was performed
            using the regression form log(Pi) = β - α log(i) or the value 'Pareto' if the regression form chosen was: log(i) = β' - α' log(Pi). In any case, the values of α and α' were made comparable by expressing all results in the Lotka form: transforming α' in 1/α'.
            ",
            br(),
            
            
            h4("Study variables"),
            h2("YEAR OF PUBLICATION"),
            "The year of publication is that of the study.
            ",
            br(),
            h2("NUMBER OF ESTIMATES"),
            "The number of estimates refers to the number of Zipf's coefficients reported in the study and included in this meta-analysis (i.e. those with sufficient specification details).
            ",
            br(),
            h2("PERIOD COVERED"),
            "The period covered corresponds to the difference between the date of observation of the latest estimation and the date of observation of the earliest estimation of the study.
            ",
            br(),
            h2("NUMBER OF COUNTRIES COVERED"),
            " The number of countries covered is the number of different territories for which an estimate is reported in the study.
            ",
            br(),
            
            
            h4("Territorial variables"),
            h2("URBANISATION LEVEL"),
            "The urbanisation level of the territory for which Zipf's coefficient is estimated corresponds to percentage of population urban according to the 2014 World Urbanization Prospects UN estimates for the years 1950-2050 .
            ",
            HTML(
              '<a href=https://esa.un.org/unpd/wup/CD-ROM">https://esa.un.org/unpd/wup/CD-ROM/</a>'
            ),
            br(),
            h2("AGE OF URBANISATION"),
            "The age of urbanisation is an indication whether urbanisation is a relatively recent or ancient phenomenon in the territory. 'OLD' continents refer to zones of early urbanisation, in Europe, South-East Asia and the Middle East. America, Oceania, Africa and central Asia are considered 'NEW' in that respect.
            ",
            br(),
            h2("TOTAL POPULATION"),
            "The total population of the territory for which Zipf's coefficient is estimated corresponds to the UN estimates for the years 1950-2015 .
            ",
            HTML(
              '<a href=http://esa.un.org/unpd/wpp/Download/Standard/Population/">http://esa.un.org/unpd/wpp/Download/Standard/Population/</a>'
            ),
            br(),
            h2("GDP PER CAPITA"),
            " The GDP per Capita in current $ of the territory for which Zipf's coefficient is estimated corresponds to the World Bank estimates for the years 1960-2015 .
            ",
            HTML(
              '<a href=http://data.worldbank.org/indicator/NY.GDP.PCAP.CD">http://data.worldbank.org/indicator/NY.GDP.PCAP.CD</a>'
            ),
            br(),
            
            
            h4("Dynamic variables"),
            h2("POPULATION GROWTH"),
            "The population growth of the territory for which Zipf's coefficient is estimated corresponds to the annual average growth rates of its total population (C3).
            ",
            br(),
            h2("GDP GROWTH"),
            "The GDP growth of the territory for which Zipf's coefficient is estimated corresponds to the annual average growth rates of its GDP per Capita (C4).
            ",
            br(),
            h2("URBANISATION GROWTH"),
            "The urbanisation growth of the territory for which Zipf's coefficient is estimated corresponds to the annual average growth rates of its urbanisation level (C1).
            ",
            br(),
            h2("GROWTH OF THE NUMBER OF CITIES"),
            "The growth of the number of cities of the territory for which Zipf's coefficient is estimated corresponds to the annual average growth rates of the number of cities used in the estimation (A3).
            ",
            br(),
            
            
            h4("Event variables"),
            h2("INTERNATIONAL WARS"),
            "International Wars are recorded from the Wikipedia list of wars by date . Only the 196 international wars involving national States between 1700 and 2014 were selected and geocoded.
            ",
            HTML(
              '<a https://en.wikipedia.org/wiki/Outline_of_war#Wars">https://en.wikipedia.org/wiki/Outline_of_war#Wars</a>'
            ),
            br(),
            h2("CIVIL WARS"),
            "Civil Wars are recorded from the Wikipedia list of civil wars . Only the 106 civil wars which happened at a national scale between 1775 and 2015 were selected and geocoded.
            ",
            HTML(
              '<a https://en.wikipedia.org/wiki/List_of_civil_wars">https://en.wikipedia.org/wiki/List_of_civil_wars</a>'
            ),
            br(),
            h2("REVOLUTIONS"),
            "Revolutions are recorded from the Wikipedia list of revolutions and rebellions . Only the 107 revolutions which happened at a national scale between 1642 and 2014 were selected and geocoded.
            ",
            HTML(
              '<a https://en.wikipedia.org/wiki/List_of_revolutions_and_rebellions">https://en.wikipedia.org/wiki/List_of_revolutions_and_rebellions</a>'
            ),
            br(),
            h2("WARS OF INDEPENDENCE"),
            "Wars of independence are recorded from the Wikipedia list of national independence days  and geocoded.
            ",
            HTML(
              '<a https://en.wikipedia.org/wiki/List_of_national_independence_days"> https://en.wikipedia.org/wiki/List_of_national_independence_days</a>'
            ),
            br(),
            
            
            br(),
            h4("Data"),
            h2("Subset Table by:"),
            fluidRow(
              column(4, selectizeInput("territory", "Territory", "", multiple = T)),
              column(4, selectizeInput("scale", "City Definition", "", multiple =
                                         T)),
              column(4, selectizeInput("decade", "Decade", "", multiple =
                                         T)),
              column(3, downloadButton("downloadData", "Download")),
              column(
                9,
                HTML(
                  'N.B. This table is a simplified version. You can find the full version here <a href="here">https://github.com/ClementineCttn/MetaZipf</a>'
                )
              )
            ),
            dataTableOutput('review'),
            
            tags$hr()
            
            
            
            ),
          
          tabPanel(
            "3. Results",
            h6(
              "We use a multiple regression to test our hypotheses about the relation between
              estimated alphas and the characteristics of the estimation, of the study and of the urban system considered."
            ),
            tags$hr(),
            "Technically, we regress the value of alpha reported for the estimation k of a study s regarding the territory m at time t as follow:",
            withMathJax(
              h6(
                "$$\\alpha_{k,s,m} = Intercept + b1 * A_k + b2 * B_s + b3 * C_m + e_s + e_c + e_k $$"
              )
              ),
            
            "where Ak represents a set of variables describing the estimation k,
            Bs represents variables describing the study s,
            Cm represents static variables relating to the territory m,
            es and ec represent respectively a fixed-study and fixed-country effect if the model selected includes fixed-effects,
            ek represents normally distributed errors.",
            br(),
            
            "For comparability reasons, most of the characteristics A, B and C
            that we consider have been discretised into ordinal categories.
            You can modify the bounds of each discretisation using
            the sliders appearing when the variable is included in the model.",
            
            tags$hr(),
            
            
            h4("Select Features for the Meta Analysis"),
            fluidRow(
              column(
                4,
                checkboxGroupInput(
                  "technicalSpecs",
                  "Ak. Estimation Variables",
                  c(
                    "All" = "alltech",
                    "Population Cutoff" = "truncation4model",
                    "Number of Cities" = "N4model",
                    "City definition" = "scale4model",
                    "Regression form" = "regForm",
                    "Estimation method" = "olsOrnot"
                  ),
                  selected = NULL,
                  inline = FALSE
                )
              ),
              column(
                4,
                checkboxGroupInput(
                  "otherSpecs",
                  "Bs. Study Variables",
                  c(
                    "All" = "allother",
                    "Year of Publication" = "yearOfPubli",
                    "Study Size" = "studySize",
                    "Length of Period Analyzed" = "studyPeriod",
                    "Coverage of territories" = "studyCoverage"
                  ),
                  selected = NULL,
                  inline = FALSE
                )
              ),
              column(
                4,
                checkboxGroupInput(
                  "topicalSpecs",
                  "Cm. Territorial Variables",
                  c(
                    "All" = "alltop",
                    "Date of observation" = "time",
                    "Age of Urbanisation" = "urbanisation4model",
                    "Country Population" = "countrySize",
                    "Urbanization Level" = "countryUrb",
                    "Country GDP per capita" = "countryGDP"
                  ),
                  selected = NULL,
                  inline = FALSE
                )
              ),
              
              column(
                4,
                checkboxInput("fixedEffects", "Fixed Study Effects", value = F)
              ),
              column(
                4,
                checkboxInput("fixedCountryEffects", "Fixed Country Effects", value = F)
              ),
              column(
                4,
                checkboxInput("sameSample", "Compare models with the same observations", value = F)
              ),
              # column(
              #   6,
              #   checkboxInput("standardize", "Standardise variables: (x - mean(x)) / sd(x)", value = F)
              # ),
              # 
              column(
                4,
                conditionalPanel(
                  condition = 'input.technicalSpecs.indexOf("truncation4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1',
                  sliderInput(
                    "truncVal",
                    "Population Cutoff (bounds of the medium reference class)",
                    min = 0,
                    max = 1000000,
                    value = c(10000, 100000)
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.technicalSpecs.indexOf("N4model") != -1 || input.technicalSpecs.indexOf("alltech") != -1',
                  sliderInput(
                    "NVal",
                    "Number of cities (bounds of the medium reference class)",
                    min = 1,
                    max = 1000,
                    value = c(30, 300)
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.otherSpecs.indexOf("yearOfPubli") != -1 || input.otherSpecs.indexOf("allother") != -1',
                  sliderInput(
                    "yearOfP",
                    "Year of Publication (bounds of the medium reference class)",
                    min = 1925,
                    max = 2015,
                    value = c(1975, 2000)
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.otherSpecs.indexOf("studyPeriod") != -1 || input.otherSpecs.indexOf("allother") != -1',
                  sliderInput(
                    "s_period",
                    "Length of the study period (boundary between short and long periods)",
                    min = 2,
                    max = 200,
                    value = 50
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.otherSpecs.indexOf("studyCoverage") != -1 || input.otherSpecs.indexOf("allother") != -1',
                  sliderInput(
                    "n_territories",
                    "Territories covered by the study (boundary between few and many)",
                    min = 2,
                    max = 200,
                    value = 5
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.otherSpecs.indexOf("studySize") != -1 || input.otherSpecs.indexOf("allother") != -1',
                  sliderInput(
                    "n_estim",
                    "Number of estimations in the study (boundary between small and large studies)",
                    min = 2,
                    max = 100,
                    value = 10
                  )
                )
              )
            ),
            column(
              4,
               conditionalPanel(
                condition = 'input.topicalSpecs.indexOf("countrySize") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                sliderInput(
                  "PopVal",
                  "Country Population (x 1000, bounds of the medium reference class)",
                  min = 1,
                  max = 1000000,
                  value = c(10000, 100000)
                )
              )
            ),
            column(
              4,
              conditionalPanel(
                condition = 'input.topicalSpecs.indexOf("countryUrb") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                sliderInput(
                  "UrbVal",
                  "Urbanization Level (%, bounds of the medium reference class)",
                  min = 0,
                  max = 100,
                  value = c(20, 60)
                )
              )
            ),
            column(
              4,
              conditionalPanel(
                condition = 'input.topicalSpecs.indexOf("countryGDP") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                sliderInput(
                  "GDPVal",
                  "GDP per Capita (current US$, bounds of the medium reference class)",
                  min = 1,
                  max = 100000,
                  value = c(1000, 10000)
                )
              )
            ),
            column(
              4,
              conditionalPanel(
                condition = 'input.topicalSpecs.indexOf("time") != -1 || input.topicalSpecs.indexOf("alltop") != -1',
                sliderInput(
                  "nTime",
                  "Date of observation (bounds of the medium reference class)",
                  min = 1700,
                  max = 2010,
                  value = c(1940, 2000)
                )
              )
            ),
            
            
            
            fluidRow(
              tags$hr(),
              h4("Model Fit"),
              "The accuracy of the model is given by the R2 value, but it also depends on the the number of observations (here, the estimations from the literature),
              because of missing information in the reported specifications.",
              br(),
              br(),
              tableOutput('modelparameters'),
              # tags$b("Are fixed study effects needed? (based on pFtest)"),
              #  textOutput('pFtest'),
              tags$hr(),
              h4("Results"),
              tags$b("Intercept Coefficient"),
              tableOutput('model_temporal'),
              tags$b("Significant Coefficients"),
              tableOutput('model_significant'),
              br(),"Adjust the level of significance according to your requirements:",
              sliderInput(
                "significance",
                "Significance level (%)",
                min = 0.1,
                max = 10,
                value = 5
              ),
              tags$b("Non-significant Coefficients"),
              tableOutput('model_non_significant'),
              tags$hr(),
              "To interpret the results, please note that:",
              br(),
              br(),
              tags$ul(
                tags$li(
                  "The ",
                  tags$b("Intercept"),
                  " gives the average value of alpha predicted for
                  an urban system characterized by the reference categories of the selected variables",
                  htmlOutput('REFS')
                )
                )
                )
            
                )
              )
              ),
      tabPanel(
        "D. Dynamic Analysis",
        tabsetPanel(
          tabPanel(
            "1. Meta Dynamic Analysis",
            
            h6(
              "We use a multiple regression to test our hypotheses about the relation between
              the evolution of alpha values and the characteristics of the territory, of its evolution and of some events happening within the time frame considered."
            ),
            tags$hr(),
            "Technically, we regress the average annual growth rate of alpha between t1 and t2 (when two or more values of alpha were reported in a study with the same specification) as follow:",
            withMathJax(
              h6(
                "$$G_{\\alpha, l, m, t1, t2} = Intercept + b1 * C_{m, t1} + b2 * D_{m, t1, t2} + b3 * E_{m, t1, t2} + e_p + e_l $$"
              )
              ),
            
            "where Cm represents a set of variables describing the territory m at time t1,
            Dm represents the evolution of territorial variables between t1 and t2,
            Em represents events which have happened in the territory m between t1 and t2,
ep represents a fixed effect in the panel models,
            el represents normally distributed errors.",
            br(),
            
            "For comparability reasons, most of the characteristics C and D
            that we consider have been discretised into ordinal categories.
            You can modify the bounds of each discretisation using
            the sliders appearing when the variable is included in the model.",
            
            tags$hr(),
            
            
            
            h4("Select Features for the Dynamic Meta Analysis"),
            fluidRow(
              
              column(
                4,
                checkboxGroupInput(
                  "var_static_meta_analysis",
                  "Cm. Territorial Variables",
                  c("Initial Alpha" = "alpha",
                    "Initial Population" = "pop",
                    "Initial GDP per capita" = "gdp",
                    "Initial Urbanization level" = "urb"
                  ),
                  selected = c('alpha'),
                  inline = FALSE
                )
              ),
              column(
                4,
                checkboxGroupInput(
                  "var_dyn_meta_analysis",
                  "Dm. Dynamic Variables",
                  c("Date" = "t",
                    "Population Growth" = "tcam_pop",
                    "GDP per capita Growth" = "tcam_gdp",
                    "Urbanization Growth" = "tcam_urb",
                    "Number of Cities" = "n"
                  ),
                  selected = NULL,
                  inline = FALSE
                )
              ),
              column(
                4,
                checkboxGroupInput(
                  "meta_events_meta_analysis",
                  "Em. Event Variables",
                  c(
                    "Revolution" = "rv",
                    "War of Independence" = "wi",
                    "Civil War" = "cw",
                    "International War" = "iw"
                  ),
                  selected = NULL,
                  inline = FALSE
                )
              ),
              
              column(
                6,
                checkboxInput("fixedEffects2", "Fixed Effect Panel Model", value = F)
              ),
              column(
                6,
                checkboxInput(
                  "sameSample2",
                  "Compare models with the same observations",
                  value = F
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_static_meta_analysis.indexOf("alpha") != -1',
                  sliderInput(
                    "alphaVal",
                    "Alpha at the beginning of the period (bounds of the medium reference class)",
                    min = 0,
                    max = 5,
                    value = c(0.75, 1.25),
                    step = 0.05
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_static_meta_analysis.indexOf("pop") != -1',
                  sliderInput(
                    "PopVal2",
                    "Country Population (x 1000, bounds of the medium reference class)",
                    min = 1,
                    max = 1000000,
                    value = c(10000, 100000)
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_static_meta_analysis.indexOf("gdp") != -1',
                  sliderInput(
                    "GDPVal2",
                    "GDP per Capita (current US$, bounds of the medium reference class)",
                    min = 1,
                    max = 100000,
                    value = c(1000, 10000)
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_static_meta_analysis.indexOf("urb") != -1',
                  sliderInput(
                    "UrbVal2",
                    "Urbanization Level (%, bounds of the medium reference class)",
                    min = 0,
                    max = 100,
                    value = c(20, 60)
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_dyn_meta_analysis.indexOf("tcam_pop") != -1 ',
                  sliderInput(
                    "ratespop",
                    "Rates of Population Growth (%, bounds of the medium class)",
                    min = -1,
                    max = 5,
                    value = c(1, 2),
                    step = 0.5
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_dyn_meta_analysis.indexOf("tcam_gdp") != -1 ',
                  sliderInput(
                    "ratesgdp",
                    "Rates of GDP Growth (%, bounds of the medium class)",
                    min = -5,
                    max = 10,
                    value = c(1, 5),
                    step = 0.5
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_dyn_meta_analysis.indexOf("tcam_urb") != -1 ',
                  sliderInput(
                    "ratesurb",
                    "Rates of Urbanisation Growth (%, bounds of the medium class)",
                    min = -1,
                    max = 5,
                    value = c(1, 2),
                    step = 0.5
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.var_dyn_meta_analysis.indexOf("n") != -1 ',
                  sliderInput(
                    "ratesn",
                    "Rates of Growth of the number of cities (%, bounds of the medium class)",
                    min = -1,
                    max = 5,
                    value = c(1, 5),
                    step = 0.5
                  )
                )
              )
       
              
            ),
            
            fluidRow(
              tags$hr(),
              h4("Model Fit"),
              tableOutput('modeldyn_fit'),
              tags$hr(),
              h4("Results"),
              tags$b("Intercept"),
              tableOutput('model_temporal_dyn'),
              tags$b("Significant Coefficients"),
              tableOutput('model_significant_dyn'),
              sliderInput(
                "significance_dyn",
                "Significance level (%)",
                min = 0.1,
                max = 10,
                value = 5
              ),
              tags$b("Non-significant Coefficients"),
              tableOutput('model_non_significant_dyn'),
              tags$hr()
            )
            ),
          tabPanel(
            "2. Residuals",
            
            
            h6(
              "In this tab, you can explore the residual growth rates of alpha to search for other historical factors absent from the dynamic model."
            ),
            tags$hr(),
            "Given the low predictive power of the dynamic models, the residuals still encapsulate most of the empirical information.
            Looking overestimated and underestimated observations can suggest other factors affecting the evolution of the city size unevenness.
            For example, rebalancing policies can be evaluated if a country at a certain period systematically appear in the overestimated residuals.",
            tags$hr(),
            
            h4("Select the type of residuals to display:"),
            fluidRow(
              column(
                4,
                selectInput(
                  "criteriaSubset",
                  "Criteria",
                  choices = c(
                    "Underestimated growth of alpha" = "increasing",
                    "Overestimated growth of alpha" = "decreasing"
                  )
                  ,
                  selected = c("increasing"),
                  multiple = F
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.criteriaSubset.indexOf("increasing") != -1',
                  sliderInput(
                    "threshold_growthrate_increasing",
                    "Minimum residual for display",
                    min = 0,
                    max = 10,
                    value = 0,
                    step = 0.5
                  )
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.criteriaSubset.indexOf("decreasing") != -1',
                  sliderInput(
                    "threshold_growthrate_decreasing",
                    "Maximum residual for display",
                    min = -10,
                    max = 0,
                    value = 0,
                    step = 0.5
                  )
                )
              ),
              
              dataTableOutput('residual_trajectories'),
              column(12, "* AAGR = Average Annual Growth Rate") #,
            )
          ),
          tabPanel(
            "3. Trajectories",
            
            h6(
              "In this tab, you can explore the trajectories of alpha values over time for a given set of estimation specifications."
            ),
            tags$hr(),
            "Represent your intuitions of policy and events effects on the value of alpha by considering its evolution over for all specification sets reported longitudinally in the literature.",
            tags$hr(),
            
            h4("Select territories for which to display the trajectories:"),
            fluidRow(
              column(4, selectizeInput("territory_3", "Territory", "", multiple = T)),
              column(
                4,
                checkboxGroupInput(
                  "eventsToPlot",
                  "Display Events",
                  choices = c(
                    "International Wars" = "iw",
                    "Civil Wars" = "cw",
                    "Revolutions" = "rv",
                    "Wars of Independence" = "wi",
                    " "=""
                  ),
                  inline = F
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = 'input.eventsToPlot.indexOf("iw") != -1 ||
                  input.eventsToPlot.indexOf("cw") != -1 || input.eventsToPlot.indexOf("wi") != -1 ',
                  sliderInput(
                    "duration",
                    "Min. Duration for Wars to be displayed on the graph",
                    min = 0,
                    max = 100,
                    value = 2
                  )
                )
            ),
            tags$hr(),
            
            fluidRow(
              column(
                12,
                conditionalPanel(
                  condition = 'input.eventsToPlot.indexOf("iw") != -1  ',
                  h6(
                    "Blue vertical lines = beginning of international wars involving the territory considered."
                  )
                )
                
              ),
              column(
                12,
                conditionalPanel(
                  condition = 'input.eventsToPlot.indexOf("cw") != -1 ',
                  h6(
                    "Black vertical lines = beginning of civil wars involving the territory considered."
                  )
                )
              ),
              column(
                12,
                conditionalPanel(
                  condition = 'input.eventsToPlot.indexOf("rv") != -1',
                  h6(
                    "Green vertical lines = beginning of revolutions involving the territory considered."
                  )
                )
              ),
              column(
                12,
                conditionalPanel(
                  condition = 'input.eventsToPlot.indexOf("wi") != -1',
                  h6(
                    "Red vertical lines = beginning of independence wars involving the territory considered."
                  )
                )
              )
            ),
            plotOutput('trajectories')
            
            )
          )
          )
      ),
      
      tabPanel(
        "E. Contribute !",
        
        h1("Add your estimates"),
        "No-one can read all the literature and this one is particularly vast and fast-moving.
        You might have published or reviewed estimates which are absent from the database in its current form.
        Please add them here to improve the quality and representativity of this open database
        and meta analysis.",
        h5(
          "Please remember to press the button 'Save' to save the reference and the estimates. When you do, you will be able
          to download the resulting formatted file. Do not forget to send this data with your potential comments
          to the moderator if you want them to be integrated to the meta analysis."
        ),
        wellPanel(
          column(
            4,
            selectInput(
              "type",
              "Document type",
              choices = c("Journal Article", "Book", "Thesis"),
              multiple = FALSE
            )
          ),
          column(4, textInput("author", "Author(s)",
                              value = "Ex: Lotka A. J.")),
          column(4, numericInput("year", "Publication Year", value = "2016")),
          column(4, textInput("journal", "Journal / Book Title", value = "")),
          column(4, numericInput("page", "Page of Results", value = "1")),
          column(
            4,
            selectInput(
              "regression",
              "Regression Form*",
              choices = c("LOTKA", "PARETO"),
              multiple = FALSE
            )
          ),
          br(),
          br(),
          h6("*LOTKA: Pi ~ Ri. PARETO: Ri ~ Pi.")
        ),
        
        fluidRow(column(
          6,
          sliderInput(
            "nestimates",
            "Number of estimates",
            min = 1,
            max = 100,
            value = 1
          )
        ),
        column(
          6, textInput("contributor", "Your Name", value = "")
        )),
        tags$hr(),
        uiOutput(outputId = "nestimateRows")
        )
            )
                   )
    )
