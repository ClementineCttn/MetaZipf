
library(ggplot2)
library(RColorBrewer)

SubsetMeta = function(table, attribute, value, operator="=="){
  tab = table
  tab$att = tab[,attribute]
  #tab = subset(tab, !is.na(att))
  if (operator=="==") sub = subset(tab, att == value)
  if (operator==">=") sub = subset(tab, att >= value)
  if (operator=="<=") sub = subset(tab, att <= value)
  return(sub)
}


SummaryMeta = function(table, regression = "Lotka"){
  tab = table
  references = length(list(unique(tab$REFERENCE))[[1]])
  duration = max(tab$DATE) - min(tab$DATE)
  meanAlpha = round(mean(tab$ALPHA),3)
  medianAlpha = round(median(tab$ALPHA),3)
  sdAlpha = round(sd(tab$ALPHA),3)
  minAlpha = round(min(tab$ALPHA),3)
  maxAlpha = round(max(tab$ALPHA),3)
  estimations = dim(tab)[[1]]
  pct_Local = round(dim(subset(tab, URBANSCALE == "local"))[[1]] / estimations * 100,1)
  pct_Agglo = round(dim(subset(tab, URBANSCALE == "agglo"))[[1]] / estimations * 100,1)
  pct_Metro = round(dim(subset(tab, URBANSCALE == "metro"))[[1]] / estimations * 100,1)
  t1 = subset(tab, !is.na(N))
  t2 = subset(tab, !is.na(TRUNCATION_POINT))
  medianN = median(t1$N)
  medianTruncation = median(t2$TRUNCATION_POINT)
  
  names = c("Number of estimations", "Number of references", "Number of years covered", 
            "Mean Alpha", "Median Alpha", "Standard Deviation Alpha", 
            "Mininimum Alpha", "Maximum Alpha", 
            "% of estimations with political Units (local)", 
            "% of estimations with Built-up areas (agglo)", 
            "% of estimations with Functional Areas (metro)",
            "Median Number of observations", 
            "Median Truncation Point for population")
  summ = data.frame(estimations, references, duration, 
                       meanAlpha, medianAlpha, sdAlpha, minAlpha, maxAlpha, 
                       pct_Local, pct_Agglo, pct_Metro,
                       medianN, medianTruncation)
  Summary = data.frame(cbind(names,t(summ)))
  colnames(Summary) = c("Descriptor", "Value")
  return(Summary)
}

meta = read.csv("data/zipf_meta.csv", sep=",", dec=".")


my_palette = colorRampPalette(c("seashell", "dodgerblue3"))(n = 299)


shinyServer(function(input, output) {

  
  output$review = renderDataTable({
    tab = meta
    
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha  == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    terr = input$territory
    dec = input$decade
    def = input$scale
 
    if(terr != "ALL") {
     tab = SubsetMeta(table = tab, attribute = "TERRITORY", value = terr)
    }
    if(dec != "ALL") {
      tab = SubsetMeta(table = tab, attribute = "DECADE", value = dec)
          }
    if(def != "ALL") {
      tab = SubsetMeta(table = tab, attribute = "URBANSCALE", value = def)
    }
    tab = tab[,c("ALPHA", "TERRITORY", "DATE", "N", "URBANSCALE",  "REFERENCE", "URBANDEF",  "TRUNCATION_POINT", "R2","DECADE", "COUNTRY", "REGRESSION", "NITSCH2005")]
    return(tab)
  })
  
  output$summary = renderDataTable({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    
    terr = input$territorys
    dec = input$decades
    def = input$scales
    reg = input$alpha
    
    if(terr != "ALL") {
      tab = SubsetMeta(table = tab, attribute = "TERRITORY", value = terr)
    }
    if(dec != "ALL") {
      tab = SubsetMeta(table = tab, attribute = "DECADE", value = dec)
    }
    if(def != "ALL") {
      tab = SubsetMeta(table = tab, attribute = "URBANSCALE", value = def)
    }
    summary = SummaryMeta(table = tab, regression = reg)
    return(summary)
  })
  
  output$plot = renderPlot({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    quanti = input$quanti
    quali = input$quali
    
    tab$quanti = tab[,quanti]
    if (input$log == "TRUE") tab$quanti = log(tab$quanti)
    tab$Category = tab[,quali]
    
    tab = subset(tab, !is.na(quanti))
    tab = subset(tab, !is.na(Category))
    
    p = ggplot(tab, aes(x = quanti, y = ALPHA, colour = Category)) + geom_point() +   labs(x = quanti, y = "alpha")
    return(p)
  })
  
})