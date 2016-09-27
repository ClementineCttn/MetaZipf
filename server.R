library(ggplot2)
library(RColorBrewer)
library(plyr)
library(shiny)
library(rgdal) 
library(rgeos) 
library(leaflet)
library(data.table)
library(plm)


meta = read.csv("data/zipf_meta.csv", sep=",", dec=".")
meta$REFERENCE = as.character(meta$REFERENCE)
meta$DECADE = as.factor(paste0(substr(meta$DATE, 1, 3), "0s"))

lkp = meta[,c("TERRITORY", "CNTR_ID")]
lookupCNTR = lkp[lkp$CNTR_ID  != "",]
lookupCNTR_ID = lookupCNTR[!duplicated(lookupCNTR), ]

refs = read.csv("data/zipf_refs.csv", sep=",", dec=".")
metaToAdd = data.frame()
refsToAdd = data.frame()

meta = data.frame(meta, refs[match(meta$REFID,refs$REFID),])
rownames(meta) = 1:dim(meta)[1]
meta$REFID.1 = NULL
meta$AUTHOR = NULL
meta$YEARPUB = meta$YEAR 
meta$YEAR = NULL
meta$PAGE = NULL
meta$SOURCE = NULL



  min_year_by_study = ddply(meta,~REFERENCE,summarise,min=min(DATE),max=max(DATE),
                            N_COUNTRIES=length(unique(TERRITORY)))
  min_year_by_study$StudyPeriod = min_year_by_study$max - min_year_by_study$min
  meta = data.frame(meta, min_year_by_study[match(meta$REFERENCE,min_year_by_study$REFERENCE),])
 

  
  pops = read.csv("data/UN_Population_1950_2015.csv", sep=",", dec=".")
colnames(pops) = c("CNTR_ID", "Name", "CC", paste0("POP", 1950:2015))
pops[pops == 0] <- NA

pops$Pct_POP_1950s = AAGR_pct(pops$POP1950, pops$POP1960, 10)
pops$Pct_POP_1960s = AAGR_pct(pops$POP1960, pops$POP1970, 10)
pops$Pct_POP_1970s = AAGR_pct(pops$POP1970, pops$POP1980, 10)
pops$Pct_POP_1980s = AAGR_pct(pops$POP1980, pops$POP1990, 10)
pops$Pct_POP_1990s = AAGR_pct(pops$POP1990, pops$POP2000, 10)
pops$Pct_POP_2000s = AAGR_pct(pops$POP2000, pops$POP2010, 10)
pops$Pct_POP_2010s = AAGR_pct(pops$POP2010, pops$POP2015, 5)

gdps = read.csv("data/WB_GDP_1960_2015.csv", sep=",", dec=".")
colnames(gdps) = c("CNTR_ID", "Name", "CC", paste0("GDP", 1960:2015))
gdps[gdps == 0] <- NA
  
  gdps$Pct_GDP_1960s = AAGR_pct(gdps$GDP1960, gdps$GDP1970, 10)
  gdps$Pct_GDP_1970s = AAGR_pct(gdps$GDP1970, gdps$GDP1980, 10)
  gdps$Pct_GDP_1980s = AAGR_pct(gdps$GDP1980, gdps$GDP1990, 10)
  gdps$Pct_GDP_1990s = AAGR_pct(gdps$GDP1990, gdps$GDP2000, 10)
  gdps$Pct_GDP_2000s = AAGR_pct(gdps$GDP2000, gdps$GDP2010, 10)
  gdps$Pct_GDP_2010s = AAGR_pct(gdps$GDP2010, gdps$GDP2015, 5)
 
  meta = data.frame(meta, pops[match(meta$CNTR_ID,pops$CNTR_ID),])
meta = data.frame(meta, gdps[match(meta$CNTR_ID,gdps$CNTR_ID),])
meta$TOTAL_POP = NA
for (i in 1:dim(meta)[1]) {
  if(meta[i,"DATE"] %in% 1950:2015) meta[i,"TOTAL_POP"] = round(as.numeric(meta[i,paste0("POP", meta[i,"DATE"])]),0)
}
meta$GDPPC = NA
for (i in 1:dim(meta)[1]) {
  if(meta[i,"DATE"] %in% 1960:2015) meta[i,"GDPPC"] = round(as.numeric(meta[i,paste0("GDP", meta[i,"DATE"])]),0)
}
rownames(meta) = 1:dim(meta)[1]
meta$REFID.1 = NULL
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



sumNum = function(x){sum(as.numeric(x), na.rm=TRUE)}
stdDev = function(x){sd(as.numeric(x), na.rm=TRUE)}
collapseRefs = function(x){paste(as.list(as.character(x), na.rm=TRUE), sep=" ", collapse = " | ")}
raiseXToPowerY = function(x,y){sign(x)*abs(x)^y}
AAGR_pct = function(initVal, finalVal, nPeriods){ (raiseXToPowerY((finalVal / initVal), (1 / nPeriods)) - 1 ) * 100 }
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}





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
    refs = refsArxiv()
    d = refs[refs$IN_HERE == 1,c("AUTHOR", "YEAR", "JOURNAL", "PAGE", "N_ESTIM", "REGRESSIONFORM")]
    colnames(d) = c("Author", "Year", "Journal", "Page", "Estimations", "Regression")
    return(d)
  }, options = list(pageLength = 10, paging = FALSE))
  
  
  
  DARIUSSubset <- reactive({
    if (input$dariusdef == "Local") {
      DARIUS = DARIUS_L
      DARIUS$ID = DARIUS$ROKATO
    }
    if (input$dariusdef == "Morpho") {
      DARIUS = DARIUS_A
      DARIUS$ID = DARIUS$AROKATO
    }
    DARIUSsub = DARIUS
    year4darius = paste0("Pop",input$dariusyear)
    DARIUSsub$Population = DARIUSsub[,year4darius]
    dariuscutoff = input$dariuscutoff / 1000
    DARIUSsub$Population = ifelse(DARIUSsub$Population >= dariuscutoff, DARIUSsub$Population, NA)
    return(DARIUSsub)
  })
  

  
  
  
  DARIUSzipf <- reactive({
    DARIUSdf= DARIUSSubset()
    size = DARIUSdf[order(-DARIUSdf$Population) , "Population"]
    rank = 1:length(size)
    zipf = data.frame(size, rank)
  })
  
  
  
  
  alphaSummaryByCountryForMap = reactive({
    m1 = metaTableSummary()
    m = m1[m1$TERRITORY_TYPE == "Country",]
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
    m$ALPHA_REFERENCE = paste(m$REFERENCE,round(m$ALPHA,3),  sep=": ")
    mRefs = aggregate(m[,"ALPHA_REFERENCE"], unique(list(m$CNTR_ID)), FUN = collapseRefs)
    mRefs = as.data.frame(mRefs)
     topC = join(topC,mRefs, by = "Group.1")
   
      rownames(topC) = topC$Group.1
    colnames(topC) = c("CNTR_ID","a", "b", "Estimations", "vals")
    topCN = merge(topC, lookupCNTR_ID, by = "CNTR_ID", all.x=T, all.y=F)
    topCN$Estimations = as.integer(topCN$Estimations)
    finalTop = topCN[,c("CNTR_ID", "TERRITORY", "a", "b", "Estimations", "vals")]
    colnames(finalTop) = c("CNTR_ID","Country", "Alpha Diversity*", "Mean Alpha","Estimations", "Values")
    finalTop= finalTop[finalTop$Country != "Taiwan",]
    return(finalTop)
  })
  
  
  
  alphaSummaryByCountryForTable = reactive({
    meta = metaArxiv()
    m = meta[meta$TERRITORY_TYPE == "Country",]
    if (input$alpha == "Lotka") m$ALPHA = m$ALPHALOTKA
    if (input$alpha == "Pareto") m$ALPHA = m$ALPHAPARETO
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
    
      rownames(topC) = topC$Group.1
    colnames(topC) = c("CNTR_ID","a", "b", "Estimations")
    topCN = merge(topC, lookupCNTR_ID, by = "CNTR_ID", all.x=T, all.y=F)
    topCN$Estimations = as.integer(topCN$Estimations)
    topCN = topCN[order(-topCN[,2]),]
    finalTop = topCN[,c("CNTR_ID", "TERRITORY", "a", "b", "Estimations")]
    colnames(finalTop) = c("CNTR_ID","Country", "Alpha Diversity*", "Mean Alpha","Estimations")
    finalTop= finalTop[finalTop$Country != "Taiwan",]
    finalTop$CNTR_ID = NULL
    return(finalTop)
  })

# output$plotTRAJ = renderPlot({
#   tab = metaTableSummary()
#   tab = tab[tab$StudyPeriod > 0,]
#   p <-ggplot(tab, aes(x=DATE, y=ALPHA, group=sameSpec, col=sameSpec) )
#   p +  geom_point() + geom_line()
# })


  output$DARIUSgraph = renderPlot({
    zipf = DARIUSzipf()
     maxsize = zipf[1,1]
    df = data.frame(size = c(maxsize, 10), rank = c(1, maxsize / 10))
     par(mar = c(2,2,3,2))
    p <-ggplot(zipf, aes(x=rank, y=size)) 
    p + scale_y_log10(breaks=c(10, 100, 1000, 10000), limits = c(10,15000)) +
      scale_x_log10(breaks=c(1, 10, 100, 1000), limits=c(1, 2000)) + 
      xlab("Rank") + ylab("Size (Population in thousands)") + 
     geom_smooth(method = "lm", se = FALSE, col = "#2c3e50", size = 0.5) + 
      geom_line(aes(rank, size), data=df, colour = "#B91838", size = 0.5) + 
         geom_point(color = "#1e90ff" ) + geom_line(color = "#1e90ff" ) +
      annotate("text", x = 5, y = 50, label = "Zipf's Law (alpha = 1)", colour="#B91838", size=5) +
      annotate("text", x = 5, y = 100, label = "Empirical Estimation", colour="#2c3e50", size=5) +
      
      theme(axis.text=element_text(size=12) ,
            axis.title=element_text(size=14),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
    
  DARIUSmodel <- reactive({
    zipf = DARIUSzipf()
    if (input$alpha == "Lotka") {
      model = lm(log(size) ~ log(rank), data = zipf)
    }
    if (input$alpha  == "Pareto") {
      model = lm(log(rank) ~ log(size), data = zipf)
    }
    m = summary(model)
    alpha = round(-m$coefficients[2,1],3)
    beta = round(-m$coefficients[1,1],3)
    
    sd = round(m$coefficients[2,2],3)
    r2 = round(m$r.squared,3) * 100
    ci9low = round(-confint(model)[2,2],3)
    ci9high = round(-confint(model)[2,1],3)
    n = m$df[1] + m$df[2]
    summary = data.frame(alpha, sd, r2, n, beta)
    colnames(summary) = c("Alpha", "Standard Deviation","R2", "N Cities", "beta")
    return(summary)
  })
  
  
  output$DARIUSestim = renderDataTable({
    summary = DARIUSmodel()
    summary$beta = NULL
    return(summary)
    }, options = list(pageLength = 10, paging = FALSE, searching = FALSE))
  
  
  
  
  output$topjournals= renderDataTable({
    refs = refsArxiv()
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
    refs = refsArxiv()
    
    d1 = refs[refs$IN_HERE == 1,]
    
     d = d1[,c("AUTHOR", "YEAR", "JOURNAL", "N_ESTIM")]
      
    ds = d[order(-d$N_ESTIM),]
    top = as.data.frame(ds)
         colnames(top) = c("Author", "Year", "Journal/Book","Estimations")
    return(top)
  }, options = list(pageLength = 10))
  
  
  
  
  output$topcountries= renderDataTable({
    df = alphaSummaryByCountryForTable()
    return(df)
  }, options = list(pageLength = 10))
  
  
  
  
  output$topextremes= renderDataTable({
    m = metaArxiv()
    if (input$alpha == "Lotka") m$ALPHA = m$ALPHALOTKA
    if (input$alpha == "Pareto") m$ALPHA = m$ALPHAPARETO
    
    d = m[,c("ALPHA", "TERRITORY", "DATE","URBANDEF", "R2", "N", "TRUNCATION", "REFERENCE")]
    
    ds1 = d[order(-d$ALPHA),]
    rownames(ds1) = 1:dim(ds1)[[1]]
    top = as.data.frame(ds1)
    colnames(top) = c("Alpha", "Territory", "Date","Cities", "R2", "N", "Truncation", "Reference")
  
    return(top)
  })
  
  
  
  
  
  
  output$temporal = renderPlot({
    m = metaArxiv()
  
    ggplot(m, aes(x = DATE)) + 
      geom_histogram(binwidth = 10,color = "#18BC9C", fill = "#18BC9C") +  
      labs(x = "Decade", y = "Number of Estimates")
  
  })
  
 
  output$continent = renderDataTable({
    m = metaArxiv()
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
    r$Var2 = NULL
    colnames(r) = c("CONTINENT", "count")
    studies = aggregate(r[, "count"], unique(list(r$CONTINENT)), FUN = sumNum)
    macro$Studies = studies$x
    macro$A = round(meanalpha$x,3)
    colnames(macro) = c("CONTINENT", "Estimations", "Studies", "Mean Alpha")
    macroO = macro[order(-macro$Estimations),]
    return(macroO)
  }, options = list(paging = FALSE, searching = FALSE))
  
    
    
    
    metaArxiv = reactive({
      tab = meta
      if (input$Arxiv == TRUE) tab = subset(tab, ARXIV == 1)
      return(tab)
    })
    
    refsArxiv = reactive({
      tab = refs
      if (input$Arxiv == TRUE) tab = subset(tab, ARXIV == 1)
      return(tab)
    })
    
metaTableSelected <- reactive({
    tab = metaArxiv()
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
  tab = metaArxiv()
  if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
  if (input$alpha  == "Pareto") tab$ALPHA = tab$ALPHAPARETO
  terr = input$territorys
  dec = input$decades
  def = input$scales
  if(length(terr) >= 1) tab = tab[tab$CONTINENT %in% terr,]
  if(length(dec) >= 1) tab = tab[tab$DECADE %in% dec,]
  if(length(def) >= 1) tab = tab[tab$URBANSCALE %in% def,]
  return(tab)
})


tableForTrajectories <- reactive({
  tab = metaArxiv()
  if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
  if (input$alpha  == "Pareto") tab$ALPHA = tab$ALPHAPARETO
  terr = input$territory_3
   if(length(terr) >= 1) {
     tab = tab[tab$TERRITORY %in% terr,]
       } else {
     terr = unique(as.character(tab$TERRITORY))
   }
  
  longitudinalAlphas = tab[tab$TRUNCATION != "" & tab$TERRITORY %in% terr, ]
  longitudinalAlphas$SAME_SPECIFICATIONS = ifelse(longitudinalAlphas$TRUNCATION == "sample size", 
                                      paste(longitudinalAlphas$REFID, longitudinalAlphas$TERRITORY, longitudinalAlphas$URBANSCALE, longitudinalAlphas$TRUNCATION, longitudinalAlphas$N, sep="_"),
                                      paste(longitudinalAlphas$REFID, longitudinalAlphas$TERRITORY, longitudinalAlphas$URBANSCALE, longitudinalAlphas$TRUNCATION_POINT,sep="_"))
  numberOfDates = ddply(longitudinalAlphas,~SAME_SPECIFICATIONS,summarise,N_DATES=length(unique(DATE)))
  longitudinalAlphas = data.frame(longitudinalAlphas, numberOfDates[match(longitudinalAlphas$SAME_SPECIFICATIONS,numberOfDates$SAME_SPECIFICATIONS),])
  longitudinalAlphas = longitudinalAlphas[!is.na(longitudinalAlphas$REGRESSION) & longitudinalAlphas$N_DATES > 1, ]
  
  n = dim(longitudinalAlphas)[1] - 1
  
  for (i in 1:n){
    if (longitudinalAlphas[i,"SAME_SPECIFICATIONS"] == longitudinalAlphas[i+1,"SAME_SPECIFICATIONS"]) {
      fv = longitudinalAlphas[i+1,"ALPHA"]
      iv = longitudinalAlphas[i,"ALPHA"]
      ye = 1 / (longitudinalAlphas[i+1,"DATE"] - longitudinalAlphas[i,"DATE"])
      median_date = (longitudinalAlphas[i,"DATE"] + longitudinalAlphas[i+1,"DATE"]) / 2
       longitudinalAlphas[i,"PCT_GROWTH_ALPHA"] =  AAGR_pct(initVal = iv, finalVal = fv, nPeriods = ye) 
       longitudinalAlphas[i,"GROWTH_DECADE"] = paste0(substr(as.character(median_date),1,3),'0s')
    } else {
      longitudinalAlphas[i,"PCT_GROWTH_ALPHA"] = NA
      longitudinalAlphas[i,"GROWTH_DECADE"] = NA
      }
  }
   return(longitudinalAlphas)
})





  output$review = renderDataTable({
   tab = metaTableSelected()
    tab = tab[,c("ALPHA", "TERRITORY", "DATE", "URBANISATION",
                 "N", "URBANSCALE", "TRUNCATION", "DISCIPLINE", "R2", "TERRITORY_TYPE","TOTAL_POP", "GDPPC","REFERENCE")]
    colnames(tab) = c("Alpha", "Territory", "Date", "Urban Age", 
                      "Number of Cities", "City Definition", "Population Cutoff", 
                      "Discipline", "R2", "Type of Territory", "Total Pop (x1000)", "GDP per Cap. (Current US$)","Reference")
    
     return(tab)
  }, options = list(paging = FALSE))
  
  output$trajectories = renderPlot({
    tab = tableForTrajectories()
    gp = ggplot(data = tab, aes(x=DATE, y=ALPHA, group=SAME_SPECIFICATIONS, col=SAME_SPECIFICATIONS))
   if (length(unique(tab$SAME_SPECIFICATIONS)) <= 20)  {
      gp + geom_point() + geom_line()
      } else {
      gp + geom_point() + geom_line() + guides(colour=FALSE)
      }
  })
  
  
    output$DARIUS <- renderLeaflet({
    cities = DARIUSSubset()
    
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
    #  setView(lng=75, lat=58, zoom=3) %>% 
      addCircleMarkers(data=cities, radius = ~sqrt(0.1*Population), lat = ~lat,
                        color = "#1e90ff", stroke=FALSE, fillOpacity=0.5, layerId = ~ID, lng = ~long)
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
  
  
  
  
  GrowthCountries <- reactive({
    c_shp <- full_countries#[full_countries$CNTR_ID == input$Country_name, ]
    tab = tableForTrajectories()
  #  periods = c("1950s", "1960s", "1970s")
    periods = as.character(input$decade_3)
    tab$GROWTH_DECADE = as.character(tab$GROWTH_DECADE)
    tab = tab[tab$TERRITORY_TYPE == "Country" & !is.na(tab$PCT_GROWTH_ALPHA) & tab$GROWTH_DECADE %in% periods,]
    tab$count = 1
    data = ddply(tab,~TERRITORY,summarise, 
                 MEAN_GROWTH_ALPHA=mean(PCT_GROWTH_ALPHA),
                 SD_GROWTH_ALPHA=sd(PCT_GROWTH_ALPHA),
                 N_GROWTH_ALPHA=sumNum(count))
    
    data = merge(data, lookupCNTR_ID, by = "TERRITORY", all.x=T, all.y=F)
    c_shp@data = data.frame(c_shp@data, data[match(c_shp@data$CNTR_ID,data$CNTR_ID), ])
    rate_gdp = gdps[, c("CNTR_ID", paste0('Pct_GDP_',c(1960, 1970, 1980, 1990, 2000, 2010),'s'))]
    c_shp@data = data.frame(c_shp@data, rate_gdp[match(c_shp@data$CNTR_ID,rate_gdp$CNTR_ID), ])
    rate_pop = pops[, c("CNTR_ID", paste0('Pct_POP_',c(1950,1960, 1970, 1980, 1990, 2000, 2010),'s'))]
    c_shp@data = data.frame(c_shp@data, rate_pop[match(c_shp@data$CNTR_ID,rate_pop$CNTR_ID), ])
    
     return(c_shp)
  })
  
  
  
     ZipfCountries <- reactive({
    c_shp <- full_countries#[full_countries$CNTR_ID == input$Country_name, ]
     data = alphaSummaryByCountryForMap()
    
    colnames(data) = c("CNTR_ID", "Name", "Diversity", "Alpha", "Estimation", "Values")
    c_shp@data = data.frame(c_shp@data, data[match(c_shp@data$CNTR_ID,data$CNTR_ID), ])
    
    c_shp@data = data.frame(c_shp@data, data[match(c_shp@data$CNTR_ID,data$CNTR_ID), ])
    c_shp@data = data.frame(c_shp@data, data[match(c_shp@data$CNTR_ID,data$CNTR_ID), ])
    return(c_shp)
  })
  
     
     
     
  output$worldmap <- renderLeaflet({
    tab = metaArxiv()
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
      
  
      leaflet(countriesToMap) %>% addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>% setView(lng=10, lat=20, zoom=2) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0, 
                  fillColor = ~VarToMap, fillOpacity = 0.7, 
                  layerId = ~CNTR_ID, popup = ~Values, options = popupOptions(maxWidth = 100)) %>% 
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
  
  
 
    
  metaModelOLS <- reactive({
    tab = metaArxiv()
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("truncation4model", "N4model", "scale4model", "regForm")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "countryGDP","country")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("yearOfPubli", "studySize", "studyPeriod", "studyCoverage") #"discipline"
    
    tab$Date_of_Observation = tab$DATE - 1950
    regressants = "ALPHA ~ 1 + Date_of_Observation"
    columnsToKeep = c("ALPHA", "Date_of_Observation")
    # if ('year4model' %in% TopicalSpecs == "TRUE") {
    #   tab$Date_of_Observation = tab$DATE - 1950
    #   regressants = paste(regressants, " + Date_of_Observation", sep="")}
     if ('truncation4model' %in% TechnicalSpecs == "TRUE"){
      tab$Population_Cutoff_ = as.factor(ifelse(tab$TRUNCATION_POINT <= input$truncVal[[1]], "Low", ifelse(tab$TRUNCATION_POINT >= input$truncVal[[2]], "High", " Medium")))
      regressants = paste0(regressants, " + Population_Cutoff_")
      columnsToKeep = c(columnsToKeep, "Population_Cutoff_")}
     if ('N4model' %in% TechnicalSpecs == "TRUE")  {
      tab$Number_Of_Cities_ = as.factor(ifelse(tab$N <= input$NVal[[1]], "Small", ifelse(tab$N >= input$NVal[[2]], "Large", " Medium")))
      regressants = paste0(regressants, " + Number_Of_Cities_")
      columnsToKeep = c(columnsToKeep, "Number_Of_Cities_")}
    
    if ('country' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, TERRITORY_TYPE != "")
      tab$Territory_ = tab$TERRITORY_TYPE
      regressants = paste0(regressants, " + Territory_") 
      columnsToKeep = c(columnsToKeep, "Territory_")}
    
    if ('regForm' %in%  TechnicalSpecs == "TRUE")  {
      tab$Regression_Form_ = tab$REGRESSIONFORM
      regressants = paste0(regressants, " + Regression_Form_")
      columnsToKeep = c(columnsToKeep, "Regression_Form_")}
    if ('scale4model' %in% TechnicalSpecs == "TRUE")  {
      tab$City_Definition_ = tab$URBANSCALE
      regressants = paste0(regressants, " + City_Definition_")
      columnsToKeep = c(columnsToKeep, "City_Definition_")}
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE"){
      tab = subset(tab, URBANISATION != "")
      tab$Urbanisation_Age_ = tab$URBANISATION
      regressants = paste0(regressants, " + Urbanisation_Age_")
      columnsToKeep = c(columnsToKeep, "Urbanisation_Age_")}
    if ('countrySize' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, TOTAL_POP > 0)
      tab$Country_Size_ = as.factor(ifelse(tab$TOTAL_POP <= input$PopVal[[1]], "Small", ifelse(tab$TOTAL_POP >= input$PopVal[[2]], "Large", " Medium")))
      regressants = paste0(regressants, " + Country_Size_")
      columnsToKeep = c(columnsToKeep, "Country_Size_")}
    if ('countryGDP' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, GDPPC > 0)
      tab$Country_GDP_ = as.factor(ifelse(tab$GDPPC <= input$GDPVal[[1]], "Low", ifelse(tab$GDPPC >= input$GDPVal[[2]], "High", " Medium")))
       regressants = paste0(regressants, " + Country_GDP_")
       columnsToKeep = c(columnsToKeep, "Country_GDP_")}
    # if ('discipline' %in% OtherSpecs == "TRUE") {
    #   tab = subset(tab, ECO != "")
    #   tab$Discipline_ECO = tab$ECO
    #   tab$Discipline_SOC = tab$SOC
    #   tab$Discipline_PHYS = tab$PHYS
    #   regressants = paste0(regressants, " + Discipline_ECO + Discipline_SOC + Discipline_PHYS")}
    if ('yearOfPubli' %in% OtherSpecs  == "TRUE") {
      tab$Year_Of_Publication_ = as.factor(ifelse(tab$YEARPUB <= input$yearOfP[[1]], "Early", ifelse(tab$YEARPUB >= input$yearOfP[[2]], "Recent", " Medium")))
       regressants = paste0(regressants, " + Year_Of_Publication_")
       columnsToKeep = c(columnsToKeep, "Year_Of_Publication_")}
    if ('studySize' %in% OtherSpecs  == "TRUE") {
      tab$Study_Size_ = as.factor(ifelse(tab$N_ESTIM == 1, "Single", ifelse(tab$N_ESTIM >= input$n_estim, "Large", " Small")))
      regressants = paste0(regressants, " + Study_Size_")
      columnsToKeep = c(columnsToKeep, "Study_Size_")}
    if ('studyCoverage' %in% OtherSpecs  == "TRUE") {
      tab$Territorial_Coverage_ = as.factor(ifelse(tab$N_COUNTRIES == 1, "Single", ifelse(tab$N_COUNTRIES >= input$n_territories, "Large", " Small")))
      regressants = paste0(regressants, " + Territorial_Coverage_")
      columnsToKeep = c(columnsToKeep, "Territorial_Coverage_")}
    if ('studyPeriod' %in% OtherSpecs  == "TRUE") {
        tab$Period_Analysed_ = as.factor(ifelse(tab$StudyPeriod == 0, "Cross_section", ifelse(tab$StudyPeriod >= input$s_period, "Long", " Short")))
      regressants = paste0(regressants, " + Period_Analysed_")
      columnsToKeep = c(columnsToKeep, "Period_Analysed_")}
   
    
    sameSample = input$sameSample
    if (sameSample == T) {
      tab = subset(tab, TRUNCATION_POINT >= 0)
      tab = subset(tab, N >= 0)
      tab = subset(tab, TERRITORY_TYPE != "")
      tab = subset(tab, URBANISATION != "")
      tab = subset(tab, TOTAL_POP > 0)
      tab = subset(tab, GDPPC > 0)
      tab = tab[,columnsToKeep]
      tab = tab[complete.cases(tab),]
    }
    
    
     model = lm(regressants, data=tab, na.action = na.omit)
    return(model)
  })
  
  metaModelFixed <- reactive({
    tab = metaArxiv()
     if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("truncation4model", "N4model", "scale4model", "regForm")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "countryGDP","country")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("yearOfPubli", "studySize", "studyPeriod", "studyCoverage") #"discipline"
    
    tab$Date_of_Observation = tab$DATE - 1950
    regressants = "ALPHA ~ Date_of_Observation"
    # if ('year4model' %in% TopicalSpecs == "TRUE") {
    #   tab$Date_of_Observation = tab$DATE - 1950
    #   regressants = paste(regressants, " + Date_of_Observation", sep="")}
    if ('truncation4model' %in% TechnicalSpecs == "TRUE"){
      tab$Population_Cutoff_ = as.factor(ifelse(tab$TRUNCATION_POINT <= input$truncVal[[1]], "Low", ifelse(tab$TRUNCATION_POINT >= input$truncVal[[2]], "High", " Medium")))
      regressants = paste0(regressants, " + Population_Cutoff_")}
    if ('N4model' %in% TechnicalSpecs == "TRUE")  {
      tab$Number_Of_Cities_ = as.factor(ifelse(tab$N <= input$NVal[[1]], "Small", ifelse(tab$N >= input$NVal[[2]], "Large", " Medium")))
      regressants = paste0(regressants, " + Number_Of_Cities_")}
    if ('country' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, TERRITORY_TYPE != "")
      tab$Territory_ = tab$TERRITORY_TYPE
      regressants = paste0(regressants, " + Territory_")}
    if ('regForm' %in%  TechnicalSpecs == "TRUE")  {
      tab$Regression_Form_ = tab$REGRESSIONFORM
      regressants = paste0(regressants, " + Regression_Form_")}
    if ('scale4model' %in% TechnicalSpecs == "TRUE")  {
      tab$City_Definition_ = tab$URBANSCALE
      regressants = paste0(regressants, " + City_Definition_")}
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE"){
      tab = subset(tab, URBANISATION != "")
      tab$Urbanisation_Age_ = tab$URBANISATION
      regressants = paste0(regressants, " + Urbanisation_Age_")}
    if ('countrySize' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, TOTAL_POP > 0)
      tab$Country_Size_ = as.factor(ifelse(tab$TOTAL_POP <= input$PopVal[[1]], "Small", ifelse(tab$TOTAL_POP >= input$PopVal[[2]], "Large", " Medium")))
      regressants = paste0(regressants, " + Country_Size_")}
    if ('countryGDP' %in% TopicalSpecs  == "TRUE") {
      tab = subset(tab, GDPPC > 0)
      tab$Country_GDP_ = as.factor(ifelse(tab$GDPPC <= input$GDPVal[[1]], "Low", ifelse(tab$GDPPC >= input$GDPVal[[2]], "High", " Medium")))
      regressants = paste0(regressants, " + Country_GDP_")}
    # if ('discipline' %in% OtherSpecs == "TRUE") {
    #   tab = subset(tab, ECO != "")
    #   tab$Discipline_ECO = tab$ECO
    #   tab$Discipline_SOC = tab$SOC
    #   tab$Discipline_PHYS = tab$PHYS
    #   regressants = paste0(regressants, " + Discipline_ECO + Discipline_SOC + Discipline_PHYS")}
     if ('yearOfPubli' %in% OtherSpecs  == "TRUE") {
      tab$Year_Of_Publication_ = as.factor(ifelse(tab$YEARPUB <= input$yearOfP[[1]], "Early", ifelse(tab$YEARPUB >= input$yearOfP[[2]], "Recent", " Medium")))
      regressants = paste0(regressants, " + Year_Of_Publication_")}
    if ('studySize' %in% OtherSpecs  == "TRUE") {
      tab$Study_Size_ = as.factor(ifelse(tab$N_ESTIM == 1, "Single", ifelse(tab$N_ESTIM >= input$n_estim, "Large", " Small")))
      regressants = paste0(regressants, " + Study_Size_")}
    if ('studyCoverage' %in% OtherSpecs  == "TRUE") {
      tab$Territorial_Coverage_ = as.factor(ifelse(tab$N_COUNTRIES == 1, "Single", ifelse(tab$N_COUNTRIES >= input$n_territories, "Large", " Small")))
      regressants = paste0(regressants, " + Territorial_Coverage_")}
    if ('studyPeriod' %in% OtherSpecs  == "TRUE") {
      tab$Period_Analysed_ = as.factor(ifelse(tab$StudyPeriod == 0, "Cross_section", ifelse(tab$StudyPeriod >= input$s_period, "Long", " Short")))
      regressants = paste0(regressants, " + Period_Analysed_")}
   
    
    sameSample = input$sameSample
    if (sameSample == T) {
      tab = subset(tab, TRUNCATION_POINT >= 0)
      tab = subset(tab, N >= 0)
      tab = subset(tab, TERRITORY_TYPE != "")
      tab = subset(tab, URBANISATION != "")
      tab = subset(tab, TOTAL_POP > 0)
      tab = subset(tab, GDPPC > 0)
      tab = tab[,columnsToKeep]
      tab = tab[complete.cases(tab),]
    }
    
    
     tab$SAME_SPECIFICATIONS = as.factor(ifelse(tab$TRUNCATION == "sample size" | is.na(tab$TRUNCATION_POINT), 
                                               paste(tab$REFID, tab$TERRITORY, tab$URBANDEF, tab$TRUNCATION, tab$N, sep="_"),
                                               paste(tab$REFID, tab$TERRITORY, tab$URBANDEF, tab$TRUNCATION_POINT,sep="_")))

     model = lmer(as.formula(paste0(regressants, " + ( 1 | REFID)")),data=tab, REML=F, na.action=na.omit)
      return(model)
  })
# 
# output$pFtest = renderText({
#   ols = metaModelOLS()
#   fixed = metaModelFixed()
# #  f = pFtest(fixed, ols)
#   yesOrNo = "No"
# #   if (f$p.value < 0.05) { yesOrNo = "yes"}
#   return(yesOrNo)
# })

  output$model_temporal = renderTable({
    fixedEffect = input$fixedEffects
    if (fixedEffect == T) {
      model = metaModelFixed()
   } else {
      model = metaModelOLS()
     }
    mod = as.data.frame(summary(model)$coefficients)
    temporal = mod[rownames(mod) %in% c("(Intercept)","Date_of_Observation"),]
    return(temporal)
  }, digits = 3)
 
   
  output$model_significant = renderTable({
    if (input$fixedEffects == T) {
      model = metaModelFixed()
       mod = as.data.frame(summary(model)$coefficients)
       sign = input$significance / 100
       if (sign <= 0.1) tTestValue = 1.282  
       if (sign < 0.05) tTestValue = 1.645
       if (sign < 0.025) tTestValue = 1.960
       if (sign < 0.01) tTestValue = 2.326
     significant = round(mod[mod$`t value` >= tTestValue,],3)
    } else {
      model = metaModelOLS()
      mod = as.data.frame(summary(model)$coefficients)
      sign = input$significance / 100
      significant = round(mod[mod$`Pr(>|t|)` <= sign,],3)
     }
     significant = significant[!rownames(significant) %in% c("(Intercept)","Date_of_Observation"),]
     return(significant)
  }, digits=3)
  
  output$model_non_significant = renderTable({
    if (input$fixedEffects == T) {
      model = metaModelFixed()
      mod = as.data.frame(summary(model)$coefficients)
      sign = input$significance / 100
      if (sign <= 0.1) tTestValue = 1.282  
      if (sign < 0.05) tTestValue = 1.645
      if (sign < 0.025) tTestValue = 1.960
      if (sign < 0.01) tTestValue = 2.326
      non_significant = mod[mod$`t value` < tTestValue,]
    } else {
      model = metaModelOLS()
      mod = as.data.frame(summary(model)$coefficients)
      sign = input$significance / 100
      non_significant = mod[mod$`Pr(>|t|)` > sign,]
      }
      return(non_significant)
  })
  
  output$modelparameters = renderTable({
    if (input$fixedEffects == T) {
      model = metaModelFixed()
      R2 = r2.corr.mer(model) * 100
      vc <- VarCorr(model) 
      vc.tab <- as.data.frame(vc)
      vc.tabinter <- vc.tab [1,]
      vc.tabintra <- vc.tab [2,]
      InterVarMnnull <- vc.tabinter$vcov
      IntraVarMnnull <- vc.tabintra$vcov
      TotalVarMnnull <- InterVarMnnull + IntraVarMnnull
      ShareInterVar <- InterVarMnnull / TotalVarMnnull
      R2between = ShareInterVar * R2
      R2within = R2 - R2between
      Observations = length(model@frame$ALPHA)
      summ = data.frame(R2within, R2between, Observations)
      colnames(summ) = c("R2 within (%)", "R2 between (%)", "Number of Estimations")
      return(summ)
     } else {
      model = metaModelOLS()
     R2 = summary(model)$r.squared * 100
    Observations = summary(model)$df[[2]] + summary(model)$df[[1]] 
    summ = data.frame(R2, Observations)
    colnames(summ) = c("R2 of regression (%)", "Number of Estimations")
    return(summ)}
  }, include.rownames = FALSE)
  
  output$REFS = renderUI({
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    OtherSpecs = input$otherSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("truncation4model", "N4model", "scale4model", "regForm")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "countryGDP","country")
    if ('allother' %in% OtherSpecs == "TRUE") OtherSpecs = c("yearOfPubli", "studySize", "studyPeriod", "studyCoverage") #"discipline"
    
    Reference = ""
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Age of Urbanisation: Old", sep="")
    if ('truncation4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | Population Cutoff: Medium", sep="")
    if ('N4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | Number of cities: Medium", sep="")
    if ('regForm' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | Regression Form: Lotka", sep="")
   # if ('year4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Date of Observation: 1950", sep="")
    if ('scale4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | City Definition: LocalUnit", sep="")
    if ('countrySize' %in% TopicalSpecs  == "TRUE") Reference = paste(Reference, " | Country Size: Medium", sep="")
    if ('countryGDP' %in% TopicalSpecs  == "TRUE") Reference = paste(Reference, " | Country GDP: Medium", sep="")
    if ('discipline' %in% OtherSpecs == "TRUE") Reference = paste(Reference, " | Discipline: none", sep="")
    if ('country' %in% TopicalSpecs  == "TRUE") Reference = paste(Reference, " | Territory: National State", sep="")
    if ('yearOfP' %in% OtherSpecs == "TRUE") Reference = paste(Reference, " | Year Of Publication: Medium", sep="")
    if ('studySize' %in% OtherSpecs == "TRUE") Reference = paste(Reference, " | Study Size: Small", sep="")
    if ('studyCoverage' %in% OtherSpecs == "TRUE") Reference = paste(Reference, " | Coverage of territories: Small", sep="")
    if ('studyPeriod' %in% OtherSpecs == "TRUE") Reference = paste(Reference, " | Study Period: Short", sep="")
    if (Reference != "") Reference = paste0("Reference Categories ", Reference)
    h5(Reference)
  })
    
 ###### ABCD
 
   output$mapcontext<- renderLeaflet({
    countriesToMap = GrowthCountries()
    
    toMap = input$contextToMap
    period = input$decade_3
    
      Breaks = c(-100, -5, -2, 0, 2, 5, 100)
    ColorRamp = "BrBG" #colorRampPalette(c("#18BC9C", "#e3e3e3", "#2c3e50"))(n = 299)
    
    if (toMap == "g_GDP") {
      var = "GDP" 
      t = "Annual Average Growth of GDP per Capita"
    }
    if (toMap == "g_pop") {
      var = "POP" 
      t = "Annual Average Growth of Population"
    }

    countriesToMap@data$VarToCut = countriesToMap@data[,paste0('Pct_', var, '_', period)]

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
    
    
    leaflet(countriesToMap) %>% addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>% setView(lng=10, lat=20, zoom=2) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0, 
                  fillColor = ~VarToMap, fillOpacity = 0.7, layerId = ~CNTR_ID#,
               #   popup = ~N_GROWTH_ALPHA, options = popupOptions(maxWidth = 100)
      ) %>% 
      addLegend("bottomright", colors= vPal6, labels=vLegendBox, title=t)
    
  })
  
  
  
  
  output$mapectories <- renderLeaflet({
    countriesToMap = GrowthCountries()
    toMap = input$dynVarToMap
    
       if (toMap == "meanDynAlpha") {
      countriesToMap@data$VarToCut = as.numeric(countriesToMap@data$MEAN_GROWTH_ALPHA)
      ColorRamp = "BrBG" #colorRampPalette(c("#18BC9C", "#e3e3e3", "#2c3e50"))(n = 299)
      #pal <- colorNumeric('Blues', NULL)
      Breaks = c(-100, -5, -2, 0, 2, 5, 100)
      t = "Mean Growth of Alpha"
    }
    if (toMap == "sdDynAlpha") {
      countriesToMap@data$VarToCut = as.numeric(countriesToMap@data$SD_GROWTH_ALPHA)
      ColorRamp = 'Blues'#  colorRampPalette(c("#e3e3e3","#2c3e50"))(n = 299)
      Breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1) 
      t = "Standard Deviation of Growth"
    }
    if (toMap == "nDynAlpha") {
      countriesToMap@data$VarToCut = as.numeric(countriesToMap@data$N_GROWTH_ALPHA)
      ColorRamp = 'Greys'# colorRampPalette(c("#e3e3e3","#18BC9C"))(n = 299)
      Breaks = c(1, 5, 10, 20, 50, 100, 500) 
      t = "Number of Observations"
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
    
    
    leaflet(countriesToMap) %>% addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>% setView(lng=10, lat=20, zoom=2) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0, 
                  fillColor = ~VarToMap, fillOpacity = 0.7, layerId = ~CNTR_ID,
                  popup = ~VarToCut, options = popupOptions(maxWidth = 100)
                  ) %>% 
      addLegend("bottomright", colors= vPal6, labels=vLegendBox, title=t)
    

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
    })
  
  
  
  observe({
    allTech <- input$technicalSpecs
    allOther = input$otherSpecs
    allTop = input$topicalSpecs
    if("alltech" %in% allTech) updateCheckboxGroupInput(session, "technicalSpecs", selected =  c("alltech","truncation4model","N4model", "scale4model", "regForm"))
    if("allother" %in% allOther) updateCheckboxGroupInput(session, "otherSpecs", selected =  c("allother","yearOfPubli",  "studySize",  "studyPeriod", "studyCoverage"))
    if("alltop" %in% allTop) updateCheckboxGroupInput(session, "topicalSpecs", selected =  c("alltop","country","urbanisation4model", "countrySize", "countryGDP"))
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
    updateSelectInput(session, "territorys", choices = c(sort(unique(as.character(inFile$CONTINENT)))))
    updateSelectInput(session, "scales", choices = c(sort(unique(as.character(inFile$URBANSCALE)))))
    updateSelectInput(session, "decades", choices = c(sort(unique(as.character(inFile$DECADE)))))
  })
  
  observe({
    tab = metaArxiv()
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha  == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    longitudinalAlphas = tab[tab$TRUNCATION != "", ]
    longitudinalAlphas$SAME_SPECIFICATIONS = ifelse(longitudinalAlphas$TRUNCATION == "sample size" | is.na(longitudinalAlphas$TRUNCATION_POINT), 
                                                    paste(longitudinalAlphas$REFID, longitudinalAlphas$TERRITORY, longitudinalAlphas$URBANSCALE, longitudinalAlphas$TRUNCATION, longitudinalAlphas$N, sep="_"),
                                                    paste(longitudinalAlphas$REFID, longitudinalAlphas$TERRITORY, longitudinalAlphas$URBANSCALE, longitudinalAlphas$TRUNCATION_POINT,sep="_"))
    numberOfDates = ddply(longitudinalAlphas,~SAME_SPECIFICATIONS,summarise,N_DATES=length(unique(DATE)))
    longitudinalAlphas = data.frame(longitudinalAlphas, numberOfDates[match(longitudinalAlphas$SAME_SPECIFICATIONS,numberOfDates$SAME_SPECIFICATIONS),])
    inFile = longitudinalAlphas[!is.na(longitudinalAlphas$REGRESSION) & longitudinalAlphas$N_DATES > 1, ]
    if(is.null(inFile))
      return(NULL)
    updateSelectInput(session, "territory_3", choices = c(sort(unique(as.character(inFile$TERRITORY)))))
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
        tab = tab[,c("ALPHA", "TERRITORY", "DATE", "URBANISATION", "CONTINENT",
                            "N", "URBANSCALE", "TRUNCATION", "DISCIPLINE", "R2", "REFERENCE")]
      colnames(tab) = c("Alpha", "Territory", "Date", "Urban Age", "Continent",
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

