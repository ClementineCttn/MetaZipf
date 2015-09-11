
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
meta$TOTAL_POP = as.numeric(meta$TOTAL_POP)
refs = read.csv("data/zipf_refs.csv", sep=",", dec=".")



my_palette = colorRampPalette(c("seashell", "dodgerblue3"))(n = 299)


shinyServer(function(input, output) {

  output$references = renderDataTable({
    d = refs
    return(d)
  })
  
  
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
    tab = tab[,c("ALPHA", "TERRITORY", "DATE", "N", "URBANSCALE", "R2", "REFERENCE", "URBANDEF", "TRUNCATION", "GEOZONE")]
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
    summaryA = SummaryMeta(table = tab, regression = reg)
    return(summaryA)
  })
  
  output$plot = renderPlot({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    quanti = input$quanti
    quali = input$quali
    
    tab$quanti = tab[,quanti]
    tab$Category = tab[,quali]
    
    tab = subset(tab, !is.na(quanti))
    tab = subset(tab, !is.na(Category))
    
    p = ggplot(tab, aes(x = quanti, y = ALPHA, colour = Category)) +  geom_hline(yintercept=1, size=1, col="grey25") +
      geom_point() +   labs(x = quanti, y = "alpha") 
   
      
    if (input$log == "TRUE") p = p + scale_x_log10()
    return(p)
  })
  
  
  
  
  bb = c("scale4model", "truncation4model","N4model")
  'scale4model' %in% bb
  
  
  output$model = renderTable({
    tab = meta
    if (input$alpha == "Lotka") tab$ALPHA = tab$ALPHALOTKA
    if (input$alpha == "Pareto") tab$ALPHA = tab$ALPHAPARETO
    
    TechnicalSpecs = input$technicalSpecs
    TopicalSpecs = input$topicalSpecs
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")
    
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
     tab = subset(tab, TOTAL_POP > 0)
     tab$COUNTRY_SIZE = as.factor(ifelse(tab$TOTAL_POP <= input$PopVal[[1]], " Small", ifelse(tab$TOTAL_POP >= input$PopVal[[2]], " Large", " Medium")))
     regressants = paste(regressants, " + COUNTRY_SIZE", sep="")}
   
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
    
    if ('alltech' %in% TechnicalSpecs == "TRUE") TechnicalSpecs = c("scale4model",  "truncation4model", "N4model")
    if ('alltop' %in% TopicalSpecs == "TRUE") TopicalSpecs = c("urbanisation4model",  "countrySize", "year4model")

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

    Reference = "Reference Categories"
    if ('urbanisation4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Age of Urbanisation: Old", sep="")
    if ('truncation4model' %in% TechnicalSpecs == "TRUE")Reference = paste(Reference, " | Truncation Level: High", sep="")
    if ('N4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | Sample Size: Large", sep="")
    if ('year4model' %in% TopicalSpecs == "TRUE") Reference = paste(Reference, " | Year: 1950", sep="")
    if ('scale4model' %in% TechnicalSpecs == "TRUE") Reference = paste(Reference, " | City Definition: 1. Local", sep="")
    if ('countrySize' %in% TopicalSpecs  == "TRUE") Reference = paste(Reference, " | Country Size: Large", sep="")
    
    return(Reference)
  })
    
  output$histalpha = renderPlot({
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
    
    histo = ggplot(tab, aes(x = ALPHA))  +
      geom_histogram(binwidth = 0.05, color = "aquamarine3", fill = "aquamarine3") +  
      labs(x = "alpha", y = "frequency") +  geom_vline(xintercept=1, size=1, col="grey25")
    
    
    return(histo)
  })
  
  
})

