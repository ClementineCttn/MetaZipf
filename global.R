
AAGR_pct = function(initVal, finalVal, nPeriods){ 
  (raiseXToPowerY((finalVal / initVal), (1 / nPeriods)) - 1 ) * 100 
  }
raiseXToPowerY = function(x,y){
  sign(x)*abs(x)^y
  }



sumNum = function(x){sum(as.numeric(x), na.rm=TRUE)}
stdDev = function(x){sd(as.numeric(x), na.rm=TRUE)}
collapseRefs = function(x){paste(as.list(as.character(x), na.rm=TRUE), sep=" ", collapse = " | ")}
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
  t1 = subset(tab, !is.na(N) & is.numeric(N))
  t2 = subset(tab, !is.na(TRUNCATION_POINT) & is.numeric(TRUNCATION_POINT))
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
