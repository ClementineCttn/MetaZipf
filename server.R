library(ggplot2)
library(RColorBrewer)
library(plyr)
library(shiny)



meta = read.csv("data/zipf_meta.csv", sep=",", dec=".")
meta$TOTAL_POP = as.numeric(meta$TOTAL_POP)
refs = read.csv("data/zipf_refs.csv", sep=",", dec=".")

metaToAdd = data.frame()
refsToAdd = data.frame()


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
      column(2, h5(paste("Estimation ", i, sep = ""))),
      column(2,numericInput(paste("alphaestim", i, sep="_") , paste("Alpha ", i, sep = "_"), value = "1")),
      column(4,textInput(paste("territoryestim", i, sep="_"), paste("Territory", i, sep = "_"), value = "Ex: France")),
      column(4,textInput(paste("urbandefestim", i, sep="_"), paste("Urban Def.", i, sep = "_"), value = "Ex: SMA, Boroughs, UN agglomerations...")),
      column(2, " "),
      column(4,numericInput(paste("truncestim", i, sep="_"), paste("Min. pop of Cities", i, sep = "_"), value = "10000")),
      column(2,numericInput(paste("dateestim", i, sep="_"), paste("Date", i, sep = "_"), value = "2000")),
      column(2,numericInput(paste("nCitiesestim", i, sep="_"), paste("# of cities", i, sep = "_"), value = "100")),
      column(2,numericInput(paste("r2estim", i, sep="_"), paste("R2", i, sep = "_"), value = "100"))
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
  
  output$topjournals= renderDataTable({
    ntop = input$top
    d = refs[refs$JOURNAL !="Dissertation",]
    d$count = 1
    ds = aggregate(d[, "count"], unique(list(d$JOURNAL)), FUN = sumNum)
    ds = subset(ds, x > 1)  
    ds = ds[order(-ds$x),]
    top = as.data.frame(ds)
    colnames(top) = c("Journal","References")
    return(top)
  }, options = list(pageLength = 10, paging = FALSE, searching = FALSE))
  
  output$topauthors= renderDataTable({
    ntop = input$top
    d = refs[,c("AUTHOR", "YEAR", "JOURNAL", "N_ESTIM")]
    ds = d[order(-d$N_ESTIM),]
    top = as.data.frame(ds)
   colnames(top) = c("Author", "Year", "Journal/Book","Estimations")
    return(top)
  }, options = list(pageLength = 10))
  
  output$topcountries= renderDataTable({
    ntop = input$top
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
    top = join(top, keep, by = "Group.1")
    rownames(top) = top$Group.1
 #  top$Group.1 = NULL
    colnames(top) = c("Country", "Diversity* of Alpha", "Estimations")
    top$Estimations = as.integer(top$Estimations)
    return(top)
  }, options = list(pageLength = 10))
  
  output$topextremes= renderDataTable({
    ntop = input$top
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
    tab[tab$ECO == 1 & tab$SOC == 0 & tab$PHYS == 0, "DISCIPLINE"] = "ECO"
    tab[tab$ECO == 1 & tab$SOC == 1 & tab$PHYS == 0, "DISCIPLINE"] = "ECO & SOC"
    tab[tab$ECO == 1 & tab$SOC == 0 & tab$PHYS == 1, "DISCIPLINE"] = "ECO & PHYS"
    tab[tab$ECO == 1 & tab$SOC == 1 & tab$PHYS == 1, "DISCIPLINE"] = "ECO, SOC & PHYS"
    tab[tab$ECO == 0 & tab$SOC == 1 & tab$PHYS == 0, "DISCIPLINE"] = "SOC"
    tab[tab$ECO == 0 & tab$SOC == 1 & tab$PHYS == 1, "DISCIPLINE"] = "SOC & PHYS"
    tab[tab$ECO == 0 & tab$SOC == 0 & tab$PHYS == 1, "DISCIPLINE"] = "PHYS"
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
  
  
  output$model = renderTable({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("discipline")
    
   regressants = "ALPHA ~ 1"
   if ('year4model' %in% TopicalSpecs == "TRUE")  {
     tab$NORMALIZED_DATE = tab$DATE - 1950
     regressants = paste(regressants, " + NORMALIZED_DATE", sep="")}
   if ('truncation4model' %in% TechnicalSpecs == "TRUE") {
     tab$TRUNCATION_LEVEL = as.factor(ifelse(tab$TRUNCATION_POINT <= input$truncVal[[1]], " Low", ifelse(tab$TRUNCATION_POINT >= input$truncVal[[2]], " High", " Medium")))
     regressants = paste(regressants, " + TRUNCATION_LEVEL", sep="")}
   if ('scale4model' %in% TechnicalSpecs == "TRUE") {
     regressants = paste(regressants, " + URBANSCALE", sep="")}
   if ('N4model' %in% TechnicalSpecs == "TRUE") {
     tab$N_SAMPLE = as.factor(ifelse(tab$N <= input$NVal[[1]], " Small", ifelse(tab$N >= input$NVal[[2]], " Large", " Medium")))
     regressants = paste(regressants, " + N_SAMPLE", sep="")}
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE") {
     tab = subset(tab, URBANISATION != "")
     regressants = paste(regressants, " + URBANISATION", sep="")}
   if ('countrySize' %in% TopicalSpecs == "TRUE") {
     tab = subset(tab, !is.na(TOTAL_POP))
     tab = subset(tab, TOTAL_POP > 0)
     tab$COUNTRY_SIZE = as.factor(ifelse(tab$TOTAL_POP <= input$PopVal[[1]], " Small", ifelse(tab$TOTAL_POP >= input$PopVal[[2]], " Large", " Medium")))
     regressants = paste(regressants, " + COUNTRY_SIZE", sep="")}
   if ('discipline' %in% OtherSpecs == "TRUE") {
     tab = subset(tab, ECO != "")
     regressants = paste(regressants, " + ECO + SOC + PHYS", sep="")}
   
   model = lm(regressants, data=tab, na.action = na.omit)
     mod = summary(model)
     return(mod)
  })
  
  output$modelparameters = renderTable({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("discipline")
    
    regressants = "ALPHA ~ 1"
    if ('year4model' %in% TopicalSpecs == "TRUE") {
      tab$NORMALIZED_DATE = tab$DATE - 1950
      regressants = paste(regressants, " + NORMALIZED_DATE", sep="")}
    if ('truncation4model' %in% TechnicalSpecs == "TRUE"){
      tab$TRUNCATION_LEVEL = as.factor(ifelse(tab$TRUNCATION_POINT <= input$truncVal[[1]], " Low", ifelse(tab$TRUNCATION_POINT >= input$truncVal[[2]], " High", " Medium")))
      regressants = paste(regressants, " + TRUNCATION_LEVEL", sep="")}
    if ('scale4model' %in% TechnicalSpecs == "TRUE")  {
      regressants = paste(regressants, " + URBANSCALE", sep="")}
    if ('N4model' %in% TechnicalSpecs == "TRUE")  {
      tab$N_SAMPLE = as.factor(ifelse(tab$N <= input$NVal[[1]], " Small", ifelse(tab$N >= input$NVal[[2]], " Large", " Medium")))
      regressants = paste(regressants, " + N_SAMPLE", sep="")}
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE"){
      tab = subset(tab, URBANISATION != "")
      regressants = paste(regressants, " + URBANISATION", sep="")}
    if ('countrySize' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, TOTAL_POP > 0)
      tab$COUNTRY_SIZE = as.factor(ifelse(tab$TOTAL_POP <= input$PopVal[[1]], " Small", ifelse(tab$TOTAL_POP >= input$PopVal[[2]], " Large", " Medium")))
      regressants = paste(regressants, " + COUNTRY_SIZE", sep="")}
    if ('discipline' %in% OtherSpecs == "TRUE") {
      tab = subset(tab, ECO != "")
      regressants = paste(regressants, " + ECO + SOC + PHYS", sep="")}
    
    model = lm(regressants, data=tab, na.action = na.omit)
    
    R2 = summary(model)$r.squared * 100
    Observations = summary(model)$df[[2]]
    summ = data.frame(R2, Observations)
    colnames(summ) = c("R2 of regression (%)", "Number of Estimations")
    return(summ)
  })
  
  output$REFS = renderText({
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")

    Reference = "Reference Categories: \n"
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Age of Urbanisation: Old", sep="")
    if ('truncation4model' %in% TechnicalSpecs == "TRUE")Reference = paste(Reference, " | Truncation Level: High", sep="")
    if ('N4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | Sample Size: Large", sep="")
    if ('year4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Year: 1950", sep="")
    if ('scale4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | City Definition: 1. Local", sep="")
    if ('countrySize' %in% TopicalSpecs  == "TRUE") Reference = paste(Reference, " | Country Size: Large", sep="")
    
    return(Reference)
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
    inFile<-metaTableSelected()
     if(is.null(inFile))
      return(NULL)
    updateSelectInput(session, "territorys", choices = c(sort(unique(as.character(inFile$TERRITORY)))))
    updateSelectInput(session, "scales", choices = c(sort(unique(as.character(inFile$URBANSCALE)))))
    updateSelectInput(session, "decades", choices = c(sort(unique(as.character(inFile$DECADE)))))
    updateSelectInput(session, "territory", choices = c(sort(unique(as.character(inFile$TERRITORY)))))
    updateSelectInput(session, "scale", choices = c(sort(unique(as.character(inFile$URBANSCALE)))))
    updateSelectInput(session, "decade", choices = c(sort(unique(as.character(inFile$DECADE)))))
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

