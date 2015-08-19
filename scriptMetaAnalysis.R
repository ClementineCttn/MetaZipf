library(ggplot2)

meta = read.csv("~/Documents/MetaZipf/zipf_meta.csv", sep=",", dec=".")
summaryl(meta)



SubsetMeta = function(table, attribute, value, operator="=="){
  tab = table
  tab$att = tab[,attribute]
  if (operator=="==") sub = subset(tab, att == value)
  if (operator==">=") sub = subset(tab, att >= value)
  if (operator=="<=") sub = subset(tab, att <= value)
  sub = sub[,c("REFERENCE", "REGRESSION", "ALPHALOTKA", "ALPHAPARETO", "R2", "URBANDEF", "URBANSCALE", "N", "TRUNCATION_POINT", "DATE", "TERRITORY", "COUNTRY","NITSCH2005")]
  return(sub)
}

SummaryMeta = function(table, regression = "Lotka"){
  tab = table
  if (regression == "Lotka") tab$zipf_coeff = tab$ALPHALOTKA
  if (regression == "Pareto") tab$zipf_coeff = tab$ALPHAPARETO
  references = length(list(unique(tab$REFERENCE))[[1]])
  duration = max(tab$DATE) - min(tab$DATE)
  meanAlpha = mean(tab$zipf_coeff)
  medianAlpha = median(tab$zipf_coeff)
  sdAlpha = sd(tab$zipf_coeff)
  minAlpha = min(tab$zipf_coeff)
  maxAlpha = max(tab$zipf_coeff)
  estimations = dim(tab)[[1]]
  pct_Local = dim(subset(tab, URBANSCALE == "local"))[[1]] / estimations * 100
  pct_Metro = dim(subset(tab, URBANSCALE == "metro"))[[1]] / estimations * 100
  t1 = subset(tab, !is.na(N))
  t2 = subset(tab, !is.na(TRUNCATION_POINT))
  medianN = median(t1$N)
  medianTruncation = median(t2$TRUNCATION_POINT)
  
  Summary = data.frame(estimations, references, duration, 
                       meanAlpha, medianAlpha, sdAlpha, minAlpha, maxAlpha, 
                       pct_Local, pct_Metro,
                       medianN, medianTruncation)
  
  return(Summary)
}



usa = SubsetMeta(table = meta, attribute = "TERRITORY", value = "USA")
summary(usa)

usa_metro = SubsetMeta(table = usa, attribute = "URBANSCALE", value = "metro")
summary(usa_metro)
  
ussr = SubsetMeta(table = meta, attribute = "TERRITORY", value = "USSR")
summary(ussr)

SummaryMeta(ussr)
SummaryMeta(usa)
france = SubsetMeta(table = meta, attribute = "TERRITORY", value = "France")
SummaryMeta(france)
china = SubsetMeta(table = meta, attribute = "TERRITORY", value = "China")
SummaryMeta(china)






p = ggplot(ussr, aes(x = DATE, y = ALPHALOTKA, colour = N))
p + geom_point() 



p = ggplot(meta, aes(x = TRUNCATION_POINT, y = ALPHALOTKA, colour = TERRITORY))
p + geom_point() 




first_model = lm(ALPHALOTKA ~ DATE + TRUNCATION_POINT, data = meta)
summary(first_model)
