library(ggplot2)
library(RColorBrewer)
library(plyr)
library(shiny)
library(rgdal) 
library(rgeos) 


meta = read.csv("data/zipf_meta.csv", sep=",", dec=".")
meta$TOTAL_POP = as.numeric(meta$TOTAL_POP)
meta[meta$ECO == 1 & meta$SOC == 0 & meta$PHYS == 0, "DISCIPLINE"] = "ECO"
meta[meta$ECO == 1 & meta$SOC == 1 & meta$PHYS == 0, "DISCIPLINE"] = "ECO & SOC"
meta[meta$ECO == 1 & meta$SOC == 0 & meta$PHYS == 1, "DISCIPLINE"] = "ECO & PHYS"
meta[meta$ECO == 1 & meta$SOC == 1 & meta$PHYS == 1, "DISCIPLINE"] = "ECO, SOC & PHYS"
meta[meta$ECO == 0 & meta$SOC == 1 & meta$PHYS == 0, "DISCIPLINE"] = "SOC"
meta[meta$ECO == 0 & meta$SOC == 1 & meta$PHYS == 1, "DISCIPLINE"] = "SOC & PHYS"
meta[meta$ECO == 0 & meta$SOC == 0 & meta$PHYS == 1, "DISCIPLINE"] = "PHYS"

refs = read.csv("data/zipf_refs.csv", sep=",", dec=".")

metaToAdd = data.frame()
refsToAdd = data.frame()

DARIUS_A<-readOGR(dsn = "data/DARIUS_points.shp" , layer = "DARIUS_points", encoding = "utf8", stringsAsFactors = FALSE, verbose = FALSE)
DARIUSBack<-readOGR(dsn = "data/DARIUS_background2.shp" , layer = "DARIUS_background2", encoding = "utf8", stringsAsFactors = FALSE, verbose = FALSE)
DARIUS_L<-readOGR(dsn = "data/LocalDARIUS_points.shp" , layer = "LocalDARIUS_points", encoding = "utf8", stringsAsFactors = FALSE, verbose = FALSE)
LocalUnits = read.csv("data/DARIUS_LocalUnits.csv", sep=",", dec=".", header=T)

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
sumNum = function(x){sum(as.numeric(x), na.rm=TRUE)}
stdDev = function(x){sd(as.numeric(x), na.rm=TRUE)}
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
    DARIUSsub@data$Population = DARIUSsub@data[,year4darius]
    dariuscutoff = input$dariuscutoff / 1000
    DARIUSsub@data$Population = ifelse(DARIUSsub@data$Population >= dariuscutoff, DARIUSsub@data$Population, NA)
    return(DARIUSsub)
  })
  
  DARIUSzipf <- reactive({
    if (input$dariusdef == "Morpho") {
    DARIUSsub = DARIUSSubset()
    DARIUSdf = DARIUSsub@data
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
    
  output$DARIUSmap = renderPlot({
    DARIUSsub = DARIUSSubset()
    par(mar = c(0,0,1,0))
     plot(DARIUSBack, border="white", col="grey90")
    plot(DARIUSsub, pch=16, col="#1e90ff", add=T,
    cex=0.05 * sqrt(DARIUSsub@data$Population/ pi))
    leg <- c(15000, 1000, 100, 10)
    legend("topleft",legend = leg, pch = 21,
           col = "gray30", pt.bg = "dodgerblue",
           pt.cex = 0.05 * sqrt(leg / pi),
           bty = "n", cex=0.8, title = "Population in thousands")
    arrows(par()$usr[1] + 100000, 
           par()$usr[3] + 100000,
           par()$usr[1] + 1000000, 
           par()$usr[3] + 100000,
           lwd = 2, code = 3,
           angle = 90, length = 0.05)
    text(par()$usr[1] + 505000, 
         par()$usr[3] + 270000, 
         "1000 km", cex = 0.8)  
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
    m = meta[meta$COUNTRY == "YES",]
    if (input$alpha == "Lotka") m$ALPHA = m$ALPHALOTKA
    if (input$alpha == "Pareto") m$ALPHA = m$ALPHAPARETO
    m$count = 1
    keep = aggregate(m[, "count"], unique(list(m$TERRITORY)), FUN = sumNum)
    keep = subset(keep, x >= 5)
    m = m[m$TERRITORY %in% keep[,1],]
    d = aggregate(m[, "ALPHA"], unique(list(m$TERRITORY)), FUN = stdDev)
    ds = d[order(-d$x),]
    top = as.data.frame(ds)
    top$x = round(top$x, 3)
    mn = aggregate(m[, "ALPHA"], unique(list(m$TERRITORY)), FUN = mean)
    mean = as.data.frame(mn)
    mean$x = round(mean$x, 3)
    topC = join( top,mean, by = "Group.1")
    topC = join( topC,keep, by = "Group.1")
   
    rownames(topC) = topC$Group.1
 #  top$Group.1 = NULL
    colnames(topC) = c("Country", "Alpha Diversity*", "Mean Alpha", "Estimations")
    topC$Estimations = as.integer(topC$Estimations)
    topC = topC[order(-topC[,2]),]
    
    return(topC)
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

