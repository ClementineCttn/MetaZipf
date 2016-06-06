library(ggplot2)
library(RColorBrewer)
library(plyr)
library(shiny)
library(rgdal) 
library(rgeos) 
library(leaflet)
library(plotly)
library(RColorBrewer)

meta = read.csv("data/zipf_meta.csv", sep=",", dec=".")
meta$TOTAL_POP = as.numeric(meta$TOTAL_POP)

lkp = meta[,c("TERRITORY", "CNTR_ID")]
lookupCNTR = lkp[lkp$CNTR_ID  != "",]
lookupCNTR_ID = lookupCNTR[!duplicated(lookupCNTR), ]

refs = read.csv("data/zipf_refs.csv", sep=",", dec=".")
head(meta)
metaToAdd = data.frame()
refsToAdd = data.frame()

meta = data.frame(meta, refs[match(meta$REFID,refs$REFID),])
rownames(meta) = 1:dim(meta)[1]
meta$REFID.1 = NULL
meta$REGRESSIONFORM = NULL
meta$AUTHOR = NULL
meta$YEAR = NULL
meta$PAGE = NULL
meta$SOURCE = NULL

meta[meta$ECO == 1 & meta$SOC == 0 & meta$PHYS == 0, "DISCIPLINE"] = "ECO"
meta[meta$ECO == 1 & meta$SOC == 1 & meta$PHYS == 0, "DISCIPLINE"] = "ECO & SOC"
meta[meta$ECO == 1 & meta$SOC == 0 & meta$PHYS == 1, "DISCIPLINE"] = "ECO & PHYS"
meta[meta$ECO == 1 & meta$SOC == 1 & meta$PHYS == 1, "DISCIPLINE"] = "ECO, SOC & PHYS"
meta[meta$ECO == 0 & meta$SOC == 1 & meta$PHYS == 0, "DISCIPLINE"] = "SOC"
meta[meta$ECO == 0 & meta$SOC == 1 & meta$PHYS == 1, "DISCIPLINE"] = "SOC & PHYS"
meta[meta$ECO == 0 & meta$SOC == 0 & meta$PHYS == 1, "DISCIPLINE"] = "PHYS"

DARIUS_A<-read.csv("data/DARIUS_A.csv", sep=",", dec=".")
DARIUS_L<-read.csv("data/DARIUS_L.csv", sep=",", dec=".")
full_countries <- readOGR(dsn='data/world_SimplifiedGeom.shp', layer = "world_SimplifiedGeom", 
                          verbose = F,encoding = "utf8")

SummaryMetaAlpha = function(table, regression = "Lotka"){
  tab = table
  meanAlpha = round(mean(tab$ALPHA),3)
  medianAlpha = round(median(tab$ALPHA),3)
  sdAlpha = round(sd(tab$ALPHA),3)
  minAlpha = round(min(tab$ALPHA),3)
  maxAlpha = round(max(tab$ALPHA),3)
  estimations = dim(tab)[[1]]
  names = c("Number of estimations", 
            "Mean Alpha", "Median Alpha", "Standard Deviation Alpha", 
            "Mininimum Alpha", "Maximum Alpha")
  summ = data.frame(estimations,meanAlpha, medianAlpha, sdAlpha, minAlpha, maxAlpha)
  Summary = data.frame(cbind(names,t(summ)))
  colnames(Summary) = c("Statistics for Alpha", "Value")
  return(Summary)
}

SummaryMetaMeta = function(table, regression = "Lotka"){
  tab = table
  references = length(list(unique(tab$REFERENCE))[[1]])
  duration = max(tab$DATE) - min(tab$DATE)
  estimations = dim(tab)[[1]]
  pct_Agglo = round(dim(subset(tab, URBANSCALE == "MorphoCity"))[[1]] / estimations * 100,1)
  pct_Metro = round(dim(subset(tab, URBANSCALE == "MetroArea"))[[1]] / estimations * 100,1)
  t1 = subset(tab, !is.na(N))
  t2 = subset(tab, !is.na(TRUNCATION_POINT))
  medianN = median(t1$N)
  medianTruncation = median(t2$TRUNCATION_POINT)
  
  names = c("Number of references", "Number of years covered", 
             "% of estimations with MorphoCity", 
            "% of estimations with MetroArea",
            "Median Number of Cities", 
            "Median Population Cutoff")
  summ = data.frame(references, duration, 
                    pct_Agglo, pct_Metro,
                    medianN, medianTruncation)
  Summary = data.frame(cbind(names,t(summ)))
  colnames(Summary) = c("Meta Statistics", "Value")
  return(Summary)
}
x=meta$ALPHALOTKA
sumNum = function(x){sum(as.numeric(x), na.rm=TRUE)}
stdDev = function(x){sd(as.numeric(x), na.rm=TRUE)}
collapseRefs = function(x){paste(as.list(as.character(x), na.rm=TRUE), sep=" ", collapse = " | ")}

generateEstimRows <- function(i){
  list(
    fluidRow(
      column(2, h5(paste("Estimate ", i, sep = ""))),
      column(2,numericInput(paste("alphaestim", i, sep="_") , paste("Alpha ", i, sep = " "), value = "1")),
      column(4,textInput(paste("territoryestim", i, sep="_"), paste("Territory", i, sep = " "), value = "Ex: France")),
      column(4,textInput(paste("urbandefestim", i, sep="_"), paste("City Def.", i, sep = " "), value = "Ex: SMA, Boroughs, UN agglomerations...")),
      column(2, " "),
      column(4,numericInput(paste("truncestim", i, sep="_"), paste("Pop. Cutoff", i, sep = " "), value = "10000")),
      column(2,numericInput(paste("dateestim", i, sep="_"), paste("Date", i, sep = " "), value = "2000")),
      column(2,numericInput(paste("nCitiesestim", i, sep="_"), paste("# of cities", i, sep = " "), value = "100")),
      column(2,numericInput(paste("r2estim", i, sep="_"), paste("R2", i, sep = " "), value = "100"))
    ),
    tags$hr()
  )
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


shinyServer(function(input, output, session) {
   
  output$references = renderDataTable({
    d = refs[refs$IN_HERE == 1,c("AUTHOR", "YEAR", "JOURNAL", "PAGE", "N_ESTIM", "REGRESSIONFORM")]
    colnames(d) = c("Author", "Year", "Journal", "Page", "Estimations", "Regression")
    return(d)
  }, options = list(pageLength = 10, paging = FALSE))
  
  
  
  DARIUSSubset <- reactive({
    if (input$dariusdef == "Local") DARIUS = DARIUS_L
    if (input$dariusdef == "Morpho") DARIUS = DARIUS_A
    DARIUSsub = DARIUS
    year4darius = paste0("Pop",input$dariusyear)
    DARIUSsub$Population = DARIUSsub[,year4darius]
    dariuscutoff = input$dariuscutoff / 1000
    DARIUSsub$Population = ifelse(DARIUSsub$Population >= dariuscutoff, DARIUSsub$Population, NA)
    return(DARIUSsub)
  })
  
  DARIUSzipf <- reactive({
    if (input$dariusdef == "Morpho") {
    DARIUSdf= DARIUSSubset()
      }
    if (input$dariusdef == "Local") {
      DARIUSdf = LocalUnits
      DARIUSdf$Population = DARIUSdf[,paste0("Pop",input$dariusyear)]
      DARIUSdf$Population = ifelse(DARIUSdf$Population >= input$dariuscutoff / 1000, DARIUSdf$Population, NA)
    }
    size = DARIUSdf[order(-DARIUSdf$Population) , "Population"]
    rank = 1:length(size)
    zipf = data.frame(size, rank)
  })
  
  
  alphaSummaryByCountry = reactive({
    m = meta[meta$TERRITORY_TYPE == "Country",]
    if (input$alpha == "Lotka") m$ALPHA = m$ALPHALOTKA
    if (input$alpha == "Pareto") m$ALPHA = m$ALPHAPARETO
    m$REFERENCE = as.character(m$REFERENCE)
    m$count = 1
     keep = aggregate(m[, "count"], unique(list(m$CNTR_ID)), FUN = sumNum)
    keep = subset(keep, x >= 5)
    keep = subset(keep, Group.1 != "")
    m = m[m$CNTR_ID %in% keep[,1],]
    d = aggregate(m[, "ALPHA"], unique(list(m$CNTR_ID)), FUN = stdDev)
    ds = d[order(-d$x),]
    top = as.data.frame(ds)
    top$x = round(top$x, 3)
    mn = aggregate(m[, "ALPHA"], unique(list(m$CNTR_ID)), FUN = mean)
    mean = as.data.frame(mn)
    mean$x = round(mean$x, 3)
    topC = join( top,mean, by = "Group.1")
    topC = join( topC,keep, by = "Group.1")
    
    m$ALPHA_REFERENCE = paste(m$REFERENCE,m$ALPHA,  sep=": ")
    mRefs = aggregate(m[,"ALPHA_REFERENCE"], unique(list(m$CNTR_ID)), FUN = collapseRefs)
    mRefs = as.data.frame(mRefs)
    str(mRefs)
    topC = join(topC,mRefs, by = "Group.1")
   
      rownames(topC) = topC$Group.1
    colnames(topC) = c("CNTR_ID","a", "b", "Estimations", "vals")
    topCN = merge(topC, lookupCNTR_ID, by = "CNTR_ID", all.x=T, all.y=F)
    #colnames(topCN) = c("CNTR_ID","Country", "Alpha Diversity*", "Mean Alpha", "Estimations")
    topCN$Estimations = as.integer(topCN$Estimations)
    topCN = topCN[order(-topCN[,2]),]
    finalTop = topCN[,c("CNTR_ID", "TERRITORY", "a", "b", "Estimations", "vals")]
    colnames(finalTop) = c("CNTR_ID","Country", "Alpha Diversity*", "Mean Alpha","Estimations", "Values")
    finalTop= finalTop[finalTop$Country != "Taiwan",]
    return(finalTop)
  })
  
  output$DARIUSgraph = renderPlot({
    zipf = DARIUSzipf()
    par(mar = c(2,2,3,2))
    p <-ggplot(zipf, aes(x=rank, y=size)) 
    p + scale_y_log10(breaks=c(10, 100, 1000, 10000)) +
      scale_x_log10(breaks=c(1, 10, 100, 1000)) + 
      xlab("Rank") + ylab("Size (Population in thousands)") +
      geom_point(color = "#1e90ff" ) + geom_line(color = "#1e90ff" ) +
      theme(axis.text=element_text(size=12) ,
            axis.title=element_text(size=14),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
    
  output$DARIUSestim = renderDataTable({
    zipf = DARIUSzipf()
    if (input$alpha == "Lotka") {
      model = lm(log(size) ~ log(rank), data = zipf)
    }
    if (input$alpha  == "Pareto") {
      model = lm(log(rank) ~ log(size), data = zipf)
    }
    m = summary(model)
    alpha = round(-m$coefficients[2,1],3)
    sd = round(m$coefficients[2,2],3)
    r2 = round(m$r.squared,3) * 100
    ci9low = round(-confint(model)[2,2],3)
    ci9high = round(-confint(model)[2,1],3)
    n = m$df[1] + m$df[2]
    summary = data.frame(alpha, sd, r2, n)
    colnames(summary) = c("Alpha", "Standard Deviation","R2", "N Cities")
    return(summary)
    }, options = list(pageLength = 10, paging = FALSE, searching = FALSE))
  
  
  output$topjournals= renderDataTable({
     d = refs[refs$JOURNAL !="Dissertation",]
    d$count = 1
    ds = aggregate(d[, "count"], unique(list(d$JOURNAL)), FUN = sumNum)
    ds = subset(ds, x >= 1)  
    ds = ds[order(-ds$x),]
    top = as.data.frame(ds)
    colnames(top) = c("Journal","References")
    return(top)
  }, options = list(pageLength = 10,searching = FALSE))
  
  output$topauthors= renderDataTable({
     d = refs[,c("AUTHOR", "YEAR", "JOURNAL", "N_ESTIM")]
    ds = d[order(-d$N_ESTIM),]
    top = as.data.frame(ds)
         colnames(top) = c("Author", "Year", "Journal/Book","Estimations")
    return(top)
  }, options = list(pageLength = 10))
  
  output$topcountries= renderDataTable({
    df = alphaSummaryByCountry()
    df$CNTR_ID = NULL
    df$Values = NULL
    return(df)
  }, options = list(pageLength = 10))
  
  output$topextremes= renderDataTable({
    m = meta
    if (input$alpha == "Lotka") m$ALPHA = m$ALPHALOTKA
    if (input$alpha == "Pareto") m$ALPHA = m$ALPHAPARETO
    
    d = m[,c("ALPHA", "TERRITORY", "DATE","URBANDEF", "R2", "N", "TRUNCATION", "REFERENCE")]
    
    ds1 = d[order(-d$ALPHA),]
    rownames(ds1) = 1:dim(ds1)[[1]]
    top = as.data.frame(ds1)
    colnames(top) = c("Alpha", "Territory", "Date","Cities", "R2", "N", "Truncation", "Reference")
  
    return(top)
  })
  
  output$continent = renderDataTable({
    m = meta
    if (input$alpha == "Lotka") m$ALPHA = m$ALPHALOTKA
    if (input$alpha == "Pareto") m$ALPHA = m$ALPHAPARETO
    
    d = m[,c("ALPHA", "CONTINENT", "REFERENCE")]
    d$count = 1
    
    macro = aggregate(d[, "count"], unique(list(d$CONTINENT)), FUN = sumNum)
    colnames(macro) = c("Continent", "Estimations")
    meanalpha = aggregate(d[, "ALPHA"], unique(list(d$CONTINENT)), FUN = mean)
    
    refTable = as.data.frame(table(d$CONTINENT, d$REFERENCE))
    refTable[refTable == 0] <- NA
    r = refTable[!is.na(refTable$Freq),]
    r$Freq = 1
    r[,2] = NULL
    colnames(r) = c("CONTINENT", "count")
    studies = aggregate(r[, "count"], unique(list(r$CONTINENT)), FUN = sumNum)
    macro$Studies = studies$x
    macro$A = round(meanalpha$x,3)
    colnames(macro) = c("CONTINENT", "Estimations", "Studies", "Mean Alpha")
    macroO = macro[order(-macro$Estimations),]
    return(macroO)
  }, options = list(paging = FALSE, searching = FALSE))
  
    
    
    
    
metaTableSelected <- reactive({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha  == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    terr = input$territory
    dec = input$decade
    def = input$scale
    if(length(terr) >= 1) tab = tab[tab$TERRITORY %in% terr,]
    if(length(dec) >= 1) tab = tab[tab$DECADE %in% dec,]
    if(length(def) >= 1) tab = tab[tab$URBANSCALE %in% def,]
    tab = tab[order(tab$DATE),]
        tab$ALPHA = round(tab$ALPHA, 3)
     return(tab)
  })
  
metaTableSummary <- reactive({
  tab = meta
  if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
  if (input$alpha  == "Pareto") tab$ALPHA = tab$ALPHAPARETO
  terr = input$territorys
  dec = input$decades
  def = input$scales
  reg = input$alpha
  if(length(terr) >= 1) tab = tab[tab$TERRITORY %in% terr,]
  if(length(dec) >= 1) tab = tab[tab$DECADE %in% dec,]
  if(length(def) >= 1) tab = tab[tab$URBANSCALE %in% def,]
  return(tab)
})



  output$review = renderDataTable({
    tab = metaTableSelected()
    tab = tab[,c("ALPHA", "TERRITORY", "DATE", "URBANISATION",
                 "N", "URBANSCALE", "TRUNCATION", "DISCIPLINE", "R2", "REFERENCE")]
    colnames(tab) = c("Alpha", "Territory", "Date", "Urban Age", 
                      "Number of Cities", "City Definition", "Population Cutoff", 
                      "Discipline", "R2", "Reference")
    
     return(tab)
  }, options = list(paging = FALSE, searching = FALSE))
  
 
  
  output$DARIUS <- renderLeaflet({
    cities = DARIUSSubset()
    
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
    #  setView(lng=75, lat=58, zoom=3) %>% 
      addCircleMarkers(data=cities, radius = ~sqrt(0.1*Population), lat = ~lat,
                        color = "#1e90ff", stroke=FALSE, fillOpacity=0.5, layerId = ~AROKATO, lng = ~long)
  })

  
  
  output$summaryAlpha = renderDataTable({
    tab = metaTableSummary()
    summaryA = SummaryMetaAlpha(table = tab, regression = reg)
    return(summaryA)
  }, options = list(paging = FALSE, searching = FALSE))
  
  output$summaryMeta = renderDataTable({
    tab = metaTableSummary()
    summaryA = SummaryMetaMeta(table = tab, regression = reg)
    return(summaryA)
  }, options = list(paging = FALSE, searching = FALSE))
  
  output$histalpha = renderPlot({
    tab = metaTableSummary()
    
    histo = ggplot(tab, aes(x = ALPHA)) + 
      geom_histogram(binwidth = 0.05, color = "#18BC9C", fill = "#18BC9C") +  
      labs(x = "alpha", y = "frequency") +  
      geom_vline(xintercept=1, size=2, col="#2c3e50") +
      annotate("text", x = 1.01, y = dim(tab)[1] / 10, 
               label = "Zipf's Law (alpha = 1)", hjust=0, col="#2c3e50")
    return(histo)
  })
  
     ZipfCountries <- reactive({
    c_shp <- full_countries#[full_countries$CNTR_ID == input$Country_name, ]
    data = alphaSummaryByCountry()
    colnames(data) = c("CNTR_ID", "Name", "Diversity", "Alpha", "Estimation", "Values")
    c_shp@data = data.frame(c_shp@data, data[match(c_shp@data$CNTR_ID,data$CNTR_ID), ])
    return(c_shp)
    
  })
  
  output$worldmap <- renderLeaflet({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    countriesToMap = ZipfCountries()
    toMap = input$alphaVarToMap
    if (toMap == "meanAlpha") {
      countriesToMap@data$VarToCut = as.numeric(countriesToMap@data$Alpha)
      ColorRamp = "BrBG" #colorRampPalette(c("#18BC9C", "#e3e3e3", "#2c3e50"))(n = 299)
      #pal <- colorNumeric('Blues', NULL)
      Breaks = c(0, 0.9, 0.95, 1, 1.05, 1.1, 2)
      t = "Mean Alpha"
    }
    if (toMap == "diversity") {
      countriesToMap@data$VarToCut = as.numeric(countriesToMap@data$Diversity)
      ColorRamp = 'Blues'#  colorRampPalette(c("#e3e3e3","#2c3e50"))(n = 299)
      Breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1) 
      t = "Standard Deviation of Alpha"
    }
    if (toMap == "n") {
      countriesToMap@data$VarToCut = as.numeric(countriesToMap@data$Estimation)
      ColorRamp = 'Greys'# colorRampPalette(c("#e3e3e3","#18BC9C"))(n = 299)
      Breaks = c(5, 10, 20, 50, 100, 500, 1000) 
      t = "Number of Estimations"
    } 

      vPal6 <- brewer.pal(n = 6, name = ColorRamp)
      countriesToMap@data$VarToMap<- as.character(cut(countriesToMap@data$VarToCut,
                                                   breaks = Breaks,
                                                   labels = vPal6,
                                                   include.lowest = TRUE,
                                                   right = FALSE))
      vLegendBox <- as.character(levels(cut(countriesToMap@data$VarToCut,
                                            breaks = Breaks,
                                            include.lowest = TRUE,
                                            right = FALSE)))
      countriesToMap@data$VarToMap = ifelse( is.na(countriesToMap@data$VarToMap), "white", countriesToMap@data$VarToMap )
      
     #  countriesToMap@data$vals = paste(countriesToMap@data$Name,"  ",
     #                                   countriesToMap@data$Values,sep = " ", collapse="\n")
     #  
     # countriesToMap@data$popup = ifelse(is.na(countriesToMap@data$VarToMap), "Insufficient Data",
     #                                      paste(countriesToMap@data$Name,"  ",
     #                                     countriesToMap@data$Values,sep = " ", collapse="\n")
     #                                     )
     # 
     
     leaflet(countriesToMap) %>% addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>% setView(lng=10, lat=20, zoom=2) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0, 
                  fillColor = ~VarToMap, fillOpacity = 0.7, 
                  layerId = ~CNTR_ID, popup = ~Values) %>%
           addLegend("bottomright", colors= vPal6, labels=vLegendBox, title=t)
    
  })
  
 
  
  
  
  output$plot = renderPlot({
    tab = metaTableSummary()
    
    quanti = input$quanti
    quali = input$quali
    tab$quanti = tab[,quanti]
    tab$Category = as.character(tab[,quali])
    tab = subset(tab, !is.na(quanti))
    tab = subset(tab, !is.na(Category))
    
    p = ggplot(tab, aes(y = quanti, x = ALPHA, fill = Category, colour = Category)) +  geom_vline(xintercept=1, size=1, col="#2c3e50") +
      geom_point() +   labs(y = quanti, x = "alpha") 
    
    if(quali != "DECADE") {
      cols = c("#2c3e50", "#18BC9C", "#B91838", "#1e90ff")
     }
    
    if(quali == "DECADE") {
     cols = colorRampPalette(c("white", "#1e90ff", "#2c3e50"))(n = 25)
       }
   
    p = p + scale_fill_manual(values=cols)  + scale_colour_manual(values=cols)
    if (input$log == "TRUE") p = p + scale_y_log10()
    return(p)
  })
  
  
  
  
  metaModel <- reactive({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("discipline", "country")
    
    regressants = "ALPHA ~ 1"
    if ('year4model' %in% TopicalSpecs == "TRUE") {
      tab$Date_of_Observation = tab$DATE - 1950
      regressants = paste(regressants, " + Date_of_Observation", sep="")}
    if ('truncation4model' %in% TechnicalSpecs == "TRUE"){
      tab$Population_Cutoff_ = as.factor(ifelse(tab$TRUNCATION_POINT <= input$truncVal[[1]], "Low", ifelse(tab$TRUNCATION_POINT >= input$truncVal[[2]], "High", "Medium")))
      regressants = paste(regressants, " + Population_Cutoff_", sep="")}
    if ('scale4model' %in% TechnicalSpecs == "TRUE")  {
      tab$City_Definition_ = tab$URBANSCALE
      regressants = paste(regressants, " + City_Definition_", sep="")}
    if ('N4model' %in% TechnicalSpecs == "TRUE")  {
      tab$Number_Of_Cities_ = as.factor(ifelse(tab$N <= input$NVal[[1]], "Small", ifelse(tab$N >= input$NVal[[2]], "Large", "Medium")))
      regressants = paste(regressants, " + Number_Of_Cities_", sep="")}
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE"){
      tab = subset(tab, URBANISATION != "")
      tab$Urbanisation_Age_ = tab$URBANISATION
      regressants = paste(regressants, " + Urbanisation_Age_", sep="")}
    if ('countrySize' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, TOTAL_POP > 0)
      tab$Country_Size_ = as.factor(ifelse(tab$TOTAL_POP <= input$PopVal[[1]], "Small", ifelse(tab$TOTAL_POP >= input$PopVal[[2]], "Large", "Medium")))
      regressants = paste(regressants, " + Country_Size_", sep="")}
    if ('discipline' %in% OtherSpecs == "TRUE") {
      tab = subset(tab, ECO != "")
      tab$Discipline_ECO = tab$ECO
      tab$Discipline_SOC = tab$SOC
      tab$Discipline_PHYS = tab$PHYS
      regressants = paste(regressants, " + Discipline_ECO + Discipline_SOC + Discipline_PHYS", sep="")}
    if ('country' %in% OtherSpecs  == "TRUE") {
      tab = subset(tab, TERRITORY_TYPE != "")
      tab$Territory_ = tab$TERRITORY_TYPE
      regressants = paste(regressants, " + Territory_", sep="")}
    
    model = lm(regressants, data=tab, na.action = na.omit)
    return(model)
  })
  
  
  
  
  output$model = renderTable({
   model = metaModel()
     mod = summary(model)
     return(mod)
  })
  
  output$modelparameters = renderTable({
    model = metaModel()
    R2 = summary(model)$r.squared * 100
    Observations = summary(model)$df[[2]]
    summ = data.frame(R2, Observations)
    colnames(summ) = c("R2 of regression (%)", "Number of Estimations")
    return(summ)
  })
  
  output$REFS = renderUI({
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("discipline", "country")
    
    Reference = ""
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Age of Urbanisation: Old", sep="")
    if ('truncation4model' %in% TechnicalSpecs == "TRUE")Reference = paste(Reference, " | Population Cutoff: High", sep="")
    if ('N4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | Number of cities: Large", sep="")
    if ('year4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Date of Observation: 1950", sep="")
    if ('scale4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | City Definition: LocalUnit", sep="")
    if ('countrySize' %in% TopicalSpecs  == "TRUE") Reference = paste(Reference, " | Country Size: Large", sep="")
    if ('discipline' %in% OtherSpecs == "TRUE") Reference = paste(Reference, " | Discipline: none", sep="")
    if ('country' %in% OtherSpecs  == "TRUE") Reference = paste(Reference, " | Territory: National State", sep="")
      
    if (Reference != "") Reference = paste0("Reference Categories ", Reference)
    h5(Reference)
  })
    
 
 
  
  
  
  observe({
    req(input$nestimates)
    estRows <- lapply(1:input$nestimates,FUN = generateEstimRows)
      estbutton <- fluidRow(
       column(2,actionButton("addest", "Save")),
        conditionalPanel(
          condition = "input.addest == 1", 
           column(10,downloadButton("downloadTables", "Download and THEN PLEASE send the file to: c.cottineau@ucl.ac.uk"))))
     estRows <- list(estRows, estbutton)
    output$nestimateRows <- renderUI({
      do.call(fluidPage, estRows)
    })
    
  })
 #  
 # 
 # observe({
 #   if(is.null(input$send) || input$send==0) return(NULL)
 #   from <- isolate(input$from)
 #   to <- "c.cottineau@ucl.ac.uk"
 #   subject <- "My contribution to MetaZipf"
 #   comment <- isolate(input$comment)
 #   msg = paste("Dear Clementine, I have added some estimations to MetaZipf.", comment , sep = "\n")
 #   sendmail(from, to, subject, msg)
 # })
 # 
 # 
  
  datasetInput <- reactive({
    refsToAdd = refsToAdd
    if(is.null(input$addest) || input$addest==0) return(NULL)
    line = dim(refsToAdd)[1]
    refsToAdd[1, 1] = input$author
    refsToAdd[1, 2] = input$year
    refsToAdd[1, 3] = input$page
    if (input$type != "Thesis") refsToAdd[1, 4] = input$journal
    if (input$type == "Thesis") refsToAdd[1, 4] = "Dissertation"
    refsToAdd[1, 5] = input$nestimates
    refsToAdd[1, 6] = input$regression
    colnames(refsToAdd) = c("author", "year", "p", "journal", "n", "form")
    
    refName = paste(input$author, input$year, "p.", input$page, sep="")
    
    r = input$nestimates
    
    estimToAdd <- lapply(1:r, function(i) {c(input[[paste0("alphaestim_",i)]],
                                             input[[paste0("territoryestim_",i)]],input[[paste0("urbandefestim_",i)]],
                                             input[[paste0("truncestim_",i)]],input[[paste0("dateestim_",i)]],
                                             input[[paste0("nCitiesestim_",i)]],input[[paste0("r2estim_",i)]])})
    
    metaToAdd = as.data.frame(t(as.data.frame(estimToAdd)))
    if(input$alpha == "Lotka") {
    colnames(metaToAdd) = c("alphaLOTKA", "where", "what", "truncation", "when", "n", "r2")
    metaToAdd$alphaPARETO = 1/metaToAdd$alphaLOTKA
    } else {
      colnames(metaToAdd) = c("alphaPARETO", "where", "what", "truncation", "when", "n", "r2")
      metaToAdd$alphaLOTKA  = 1/metaToAdd$alphaPARETO
    }
    rownames(metaToAdd) = 1:r
    metaToAdd$ref = refName
    return(metaToAdd)
   # write.csv(refsToAdd, paste("data/ToAdd/refToAdd_session", s, ".csv", sep=""))
   # write.csv(metaToAdd, paste("data/ToAdd/metaToAdd_session", s, ".csv", sep=""))
    
  })
  
  
  observe({
    inFile<-metaTableSummary()
     if(is.null(inFile))
      return(NULL)
    updateSelectInput(session, "territory", choices = c(sort(unique(as.character(inFile$TERRITORY)))))
    updateSelectInput(session, "scale", choices = c(sort(unique(as.character(inFile$URBANSCALE)))))
    updateSelectInput(session, "decade", choices = c(sort(unique(as.character(inFile$DECADE)))))
  })
  
  observe({
    inFile<-metaTableSelected()
    if(is.null(inFile))
      return(NULL)
    updateSelectInput(session, "territorys", choices = c(sort(unique(as.character(inFile$TERRITORY)))))
    updateSelectInput(session, "scales", choices = c(sort(unique(as.character(inFile$URBANSCALE)))))
    updateSelectInput(session, "decades", choices = c(sort(unique(as.character(inFile$DECADE)))))
  })
#  observe({
#    refsToAdd = refsToAdd
#    if(is.null(input$addest) || input$addest==0) return(NULL)
#    line = dim(refsToAdd)[1]
#    refsToAdd[1, 1] = input$author
#    refsToAdd[1, 2] = input$year
#    refsToAdd[1, 3] = input$page
#    if (input$type != "Thesis") refsToAdd[1, 4] = input$journal
#    if (input$type == "Thesis") refsToAdd[1, 4] = "Dissertation"
#    refsToAdd[1, 5] = input$nestimates
#    refsToAdd[1, 6] = input$regression
#    refsToAdd[1, 7] = input$url
#    colnames(refsToAdd) = c("author", "year", "p", "journal", "n", "form", "url")
#    
#     refName = paste(input$author, input$year, "p.", input$page, sep="")
#     
#    r = input$nestimates
#    
#     estimToAdd <- lapply(1:r, function(i) {c(input[[paste0("alphaestim_",i)]],
#        input[[paste0("territoryestim_",i)]],input[[paste0("urbandefestim_",i)]],
#        input[[paste0("truncestim_",i)]],input[[paste0("dateestim_",i)]],
#        input[[paste0("nCitiesestim_",i)]],input[[paste0("r2estim_",i)]])})
#    
#     metaToAdd = as.data.frame(t(as.data.frame(estimToAdd)))
#      colnames(metaToAdd) = c("alpha", "where", "what", "truncation", "when", "n", "r2")
#     rownames(metaToAdd) = 1:r
#     metaToAdd$ref = refName
#      
#    s = as.character(Sys.time())
# #   gs_copy(refsToAdd, to = paste0("Ref", s))
# #   gs_copy(metaToAdd, to = paste0("Meta", s))   
#    write.csv(refsToAdd, paste("data/ToAdd/refToAdd_session", s, ".csv", sep=""))
#    write.csv(metaToAdd, paste("data/ToAdd/metaToAdd_session", s, ".csv", sep=""))
#  })
#  
#  
#  
#  
  
  output$downloadData <- downloadHandler(
    filename = "MetaZipf_Selection.csv",
    content = function(file) {
      tab = metaTableSelected()
        tab = tab[,c("ALPHA", "TERRITORY", "DATE", "URBANISATION",
                            "N", "URBANSCALE", "TRUNCATION", "DISCIPLINE", "R2", "REFERENCE")]
      colnames(tab) = c("Alpha", "Territory", "Date", "Urban Age", 
                        "Number of Cities", "City Definition", "Population Cutoff", 
                        "Discipline", "R2", "Reference")
      write.csv(tab, file)
    }
  )
  
  
 output$downloadTables <- downloadHandler(
   filename = function() {
     s = as.character(Sys.time())
     you = input$contributor
     paste("metaToAdd_", you, "_session", s, ".csv", sep='')
   },
   content = function(file) {
    write.csv(datasetInput(), file)
   }
 )

 })

