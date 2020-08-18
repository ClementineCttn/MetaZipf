setwd("~/Documents/MetaZipf")
library(igraph)
library(scales)
library(wesanderson)
library(ggplot2)
library(ggrepel)
library(ggalt)
library(gridExtra)
library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))

cites <- read.csv2("data/zipf_cites.csv", sep=',')
# meta <- read.csv2("data/zipf_meta.csv", sep=',')
# length(summary(meta$REFERENCE))
 head(cites)

J2D <- read.csv2('journals2Disciplines.csv', sep=';')
head(cites)
summary(J2D)

cites <- data.frame(cites,J2D[match(cites$JOURNAL, J2D$JOURNAL),])
cites$DISCIPLINE <- ifelse(!is.na(cites$DISCPLINE), as.character(cites$DISCPLINE), as.character(cites$DISCIPLINE))
cites$DISCPLINE <- NULL
cites$JOURNAL.1<- NULL

cites[is.na(cites)] <- 0
head(cites)

cites_within <- cites[1:66,c(1,6:71)]
tail(cites_within)
rownames(cites_within) <- cites_within$REFID
cites_within$REFID <- NULL


cmat <- t(as.matrix(cites_within))
dim(cmat)

node.size <- colSums(cmat) +1


cite_year <- cites[1:66,c('REFID', 'YEAR')]
ids <- cite_year$REFID
rownames(cite_year) <- ids
cite_year$REFID <- NULL
node.year <- cite_year$YEAR - 1974

cite_disc <- cites[1:66,c('REFID', 'DISCIPLINE')]
rownames(cite_disc) <- ids
cite_disc$REFID <- NULL
node.disc <- as.character(cite_disc$DISCIPLINE)


node.disc[node.disc == "ECO"] <- "slateblue3"
node.disc[node.disc == "REG"] <- "orangered"
node.disc[node.disc == "GEO"] <- "darkgoldenrod2"
node.disc[node.disc == "PHY"] <- "seagreen3"




########### Network of intra citations

g <- graph_from_adjacency_matrix(cmat, mode = "directed", diag = F)

layout <- layout_nicely(g,2)
g$layout <- layout
fine = 500 # this will adjust the resolving power.
pal.lab = colorRampPalette(c('grey30','black'))
graphCol.lab = pal.lab(fine)[as.numeric(cut(node.year,breaks = fine))]

par(mar = c(0.5, 0.3, 0.5, 0.5))

plot(g, vertex.size=node.size*0.5,
     vertex.label.dist=0.5, 
     vertex.color= alpha("grey",0.6), edge.color = "black", 
     vertex.label.cex = 0.7,  vertex.label.color = node.disc,
     vertex.label.font = 2,  
     edge.arrow.size=0.2)

solos = c("Sua80Jou", "Par83Jou", "Oka79Reg", "Mir86Urb", "Pop74Sov")
cmat_linked <- cmat[rownames(cmat) %!in% solos, colnames(cmat) %!in% solos]
dim(cmat_linked)

node.size <- colSums(cmat_linked) +1

cite_year_linked <- cite_year[rownames(cite_year) %!in% solos, ]
node.year_linked <- cite_year_linked - 1974

cite_disc_linked <- cite_disc[rownames(cite_disc) %!in% solos, ]
node.disc_linked <- as.character(cite_disc_linked)


node.disc_linked[node.disc_linked == "ECO"] <- "slateblue3"
node.disc_linked[node.disc_linked == "REG"] <- "orangered"
node.disc_linked[node.disc_linked == "GEO"] <- "darkgoldenrod2"
node.disc_linked[node.disc_linked == "PHY"] <- "seagreen3"


g <- graph_from_adjacency_matrix(cmat_linked, mode = "directed", diag = F)

layout <- layout_nicely(g,2)
g$layout <- layout

graphCol.lab = pal.lab(fine)[as.numeric(cut(node.year_linked,breaks = fine))]


plot(g, vertex.size=node.size*0.5,
     vertex.label.dist=0.5, 
     vertex.color= alpha("grey",0.6), edge.color = "black", 
     vertex.label.cex = 0.7,  vertex.label.color = node.disc_linked,
     vertex.label.font = 2,  
     edge.arrow.size=0.2)



########### Histogram of intra citations

c <- colSums(cmat)
co <- c[order(-c)]
#cop <- co[co > 0]
cop <- co
str(cop)
par(mar = c(2,2,2,2))
length(cop)
ref <- names(cop)
cope <- data.frame(ref, cop)
cope

cope_full_names <- data.frame(cope, cites[match(cope$ref, cites$REFID),c("AUTHOR", "YEAR", "JOURNAL")])
cope_full_names$ref <- paste(cope_full_names$AUTHOR, cope_full_names$YEAR, sep=" ")

q <- ggplot(cope_full_names, aes(x=ref, y = cop))
q + geom_lollipop(aes(reorder(ref, -cop)),color = "seagreen3", cex=1) +
  coord_flip() +
  labs(x="Reference", y="Number of in-citations from the corpus")
  
################## intro

refs <- read.csv2("data/zipf_refs.csv", sep=';')
dim(refs[refs$IN_HERE == 1,])
head(cites[1:66,])

cites[1:66,1:5]

j <- cites[1:66,1:5]
j$n <- 1
journals <- aggregate(j[,"n"], by=list(j[,"JOURNAL"]), FUN = sum)
q <- ggplot(journals, aes(x= Group.1, y = x))
 q + geom_lollipop(aes(reorder( Group.1, -x)),color = "orangered", cex=1) +
  coord_flip() +
  labs(x="Journal", y="Number of articles in the corpus")


 corpussummary <- data.frame(j, cope[match(j$REFID, cope$ref),])
colnames(corpussummary)[8] <- "in_citations"
mean(corpussummary$YEAR)
mean(corpussummary[corpussummary$in_citations > 10, "YEAR"])


########### network between disciplines

cites_long <- melt(cmat)
head(cites_long)
cites_long_pos <- cites_long[cites_long$value > 0,]
head(cites_long_pos)
j_citing <- j
colnames(j_citing) <- paste("citing", colnames(j), sep="_")
clpd <- data.frame(cites_long_pos, j_citing[match(cites_long_pos$Var1, j_citing$citing_REFID),])
j_cited <- j
colnames(j_cited) <- paste("cited", colnames(j), sep="_")
clpd <- data.frame(clpd, j_cited[match(clpd$Var2, j_cited$cited_REFID),])
head(clpd)

dmat <- dcast(clpd, citing_DISCIPLINE ~ cited_DISCIPLINE, sum)
dmatm <- melt(dmat)

library(igraph)


gd <- graph_from_data_frame(dmatm, directed = T)
vd <- as.data.frame(names(V(gd)))
colnames(vd) <- "discipline"
disciplines <- aggregate(j[,"n"], by=list(j[,"DISCIPLINE"]), FUN = sum)

size.nodes <- data.frame(vd, disciplines[match(vd$discipline, disciplines$Group.1),])
size.nodes.d <- size.nodes$x

plot(gd,edge.width = dmatm$value * 0.5, vertex.size = size.nodes.d * 3,
     vertex.label.cex = 0.7,  edge.curved=.8)


########### network between journals

dmat <- dcast(clpd, citing_JOURNAL ~ cited_JOURNAL, sum)
dmatm <- melt(dmat)
dmatmp <- dmatm[dmatm$value>1,]

gj <- graph_from_data_frame(dmatmp, directed = T)

v <- as.data.frame(names(V(gj)))
colnames(v) <- "journal"

size.nodes <- data.frame(v, journals[match(v$journal, journals$Group.1),])
size.nodes.l <- size.nodes$x
size.nodes

plot(gj,edge.width = dmatmp$value, vertex.label.cex = 0.7,  edge.curved=.4,
     vertex.size = size.nodes.l * 3)



############## Age of publication by journal and disciplines

age_journals <- data.frame(aggregate(j[,"YEAR"], by=list(j[,"JOURNAL"]), FUN = mean), journals$x)
age_disciplines <- data.frame(aggregate(j[,"YEAR"], by=list(j[,"DISCIPLINE"]), FUN = mean), disciplines$x)
rownames(age_disciplines) = age_disciplines$Group.1
rownames(age_journals) = age_journals$Group.1
age_disciplines$Group.1 <- NULL
age_journals$Group.1 <- NULL
colnames(age_disciplines) <- c("mean age", "n articles")
colnames(age_journals) <- c("mean age", "n articles")
yearIntra <- round(mean(j$YEAR), digit=0)
yearIntraPerDisc <- round(age_disciplines, digit=0)
yearIntraJour <- round(age_journals, digit=0)

write.csv(yearIntra, "average_year_publi_internal_all.csv")
write.csv(yearIntraPerDisc, "average_year_publi_internal_per_discipline.csv")
write.csv(yearIntraJour, "average_year_publi_internal_all_per_journal.csv")


  
  ############## external citations

library(stringr)
head(cites)
dim(cites)
cites_out <- cites[67:1221,]
tail(cites_out)

cites_out$YEAR <- gsub("\\D+", "", cites_out$REFID)
cites_out$JOURNAL <- gsub(".*\\d.", "", cites_out$REFID)
cites_out$AUTHOR <- gsub("\\d.*", "", cites_out$REFID)
cites_out$JOURNAL <- gsub("[[:punct:]]", " ", cites_out$JOURNAL)
cites_out$AUTHOR <- gsub("[[:punct:]]", " ", cites_out$AUTHOR)

head(cites_out)
summary(as.factor(cites_out$JOURNAL))/dim(cites_out)[1]
# Diverse pool of citations. MAx % of journals cited = 2.5% (Journal Regional Science) and 2.3 (Urban Studies)
#jou <- as.data.frame(summary(as.factor(cites_out$JOURNAL)))
colnames(cites_out)
cites_out$n_cites <- rowSums(cites_out[,6:71])
single_outcites <- cites_out[order(-cites_out$n_cites),c(1:5,72:73)] 
s_outcites <- single_outcites[,c("REFID", "n_cites")]
colnames(s_outcites) <- c("ref", "n")
s_outcites <- subset(s_outcites[-100,], n>=5)
q <- ggplot(s_outcites, aes(x=ref, y = n))
q + geom_lollipop(aes(reorder(ref, -n)),color = "coral3", cex=1) +
  coord_flip() +
  labs(x="Reference", y="Number of citations from the corpus (>=5)")

# Nitsch cited by 14 of the 66 corpus articles but how many were published after 2005?
cites_out[cites_out$REFID == "Nitsch_2005_Journal_Urban_Economics",]
# 41
14/66
14/41

## look for articles who do not cite Zipf
nozipf<- as.data.frame(colSums(cites_out[cites_out$REFID %in% c("Zipf_1941_Unity_disunity",
                                                                "Zipf_1949_Human_Behavior_Principle_Least_Effort"),6:71]))
colnames(nozipf) <- "ref_zipf"
nozipf$ref_zipf <- ifelse(nozipf$ref_zipf == 0 , "no ref to zipf", "ref to zipf")
nozipf$REFID <- rownames(nozipf)
cites_for_ref_zipfs <- cites[1:66,]
referencing_zipf <- data.frame(cites_for_ref_zipfs, nozipf[match(cites_for_ref_zipfs$REFID, nozipf$REFID),])
rfczpf <- referencing_zipf[,c("YEAR", "DISCIPLINE", "ref_zipf")]

q <- ggplot(rfczpf, aes(x=YEAR))
q + geom_histogram(aes(y = stat(density), color = ref_zipf, fill = ref_zipf), 
                   alpha = 0.2, position = "identity", binwidth = 2) +
  geom_density(aes(color = ref_zipf), size = 1) +
  scale_color_manual(values = c("Coral2", "dodgerblue3")) +
scale_fill_manual(values = c("Coral2", "dodgerblue3"))

table(rfczpf$ref_zipf, rfczpf$DISCIPLINE)


## look for articles published in particular journals
cites_out[cites_out$JOURNAL == "Papers in Regional Science",]


jou_out <- as.data.frame(table(cites_out$JOURNAL))
jou_out <- jou_out[order(-jou_out$Freq),] 
colnames(jou_out) <- c("ref", "n")
jou <- subset(jou_out[-100,], n>=5)
q <- ggplot(jou, aes(x=ref, y = n))
q + geom_lollipop(aes(reorder(ref, -n)),color = "goldenrod3", cex=1) +
  coord_flip() +
  labs(x="Journal", y="Number of citations from the corpus (>=5)")


jou_test<- data.frame(jou, J2D[match(jou$ref,J2D$JOURNAL),])
write.csv2(jou_test,"test_scimago.csv")


  
####### outcitation bipartite network


sumNum = function(x) { 
  s = sum(x, na.rm = T)
  return(s)
}
meanNum = function(x) { 
  s = mean(x, na.rm = T)
  return(s)
}

head(cites_out)
rownames(cites_out) <- paste0(substr(cites_out$AUTHOR,1,3), substr(cites_out$YEAR,1,4),
                              substr(cites_out$JOURNAL,1,3),rownames(cites_out))
total_cite_out <- apply(cites_out[,6:71], 2, FUN = sumNum)
head(cites_out)

cmat <- t(as.matrix(cites_out[,6:71]))

node.size <- colSums(cmat) +1

cmat[1:5,1:5]


cited_inter <- as.data.frame(colSums(cmat))
cited_inter$REFID <- rownames(cited_inter)
cited_out <- as.data.frame(sort(colSums(cmat), decreasing=T))
colnames(cited_out) <- "n_cites"
cited_out$ID <- rownames(cited_out)
cites_out$ID <- rownames(cites_out)
cited_out <- data.frame(cited_out, cites_out[match(cited_out$ID,cites_out$ID), ])


###@ ref we have probably overlooked or missed
write.csv(cited_out[cited_out$n_cites > 5, c("REFID","n_cites")], "most_out_cites.csv")


######### network of disciplines
######### network of journals


######### Bipartite Network of similarity between in papers based on out papers they cite
dim(cites_out)
outcitemat <- as.matrix(cites_out[,6:71])
toutcitemat <- t(outcitemat)

norm_vec <- function(x) sqrt(sumNum(x^2))

refSim <- rownames(toutcitemat)
cosSim <- data.frame()
k=0
ilist <- c()
for(i in refSim){
  ilist <- c(ilist, i)
  for(j in refSim){
    if (j %!in% ilist){
    k <- k + 1
    cosSim[k,1] <- i
    cosSim[k,2] <- j
    vi <- toutcitemat[i,]
    vj <- toutcitemat[j,]
    cosSim[k,3] <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
    }
  }
}
colnames(cosSim) <- c('i', 'j', 'cosSim')
dim(cosSim)
head(cosSim)

#citingN <- apply(toutcitemat,1, FUN = norm_vec)

cs.cit <- cosSim[cosSim$cosSim >= 0.3,]
g.cit <- graph_from_data_frame(cs.cit, directed=F)

citingNs <- as.data.frame(apply(toutcitemat[rownames(toutcitemat) %in% V(g.cit)$name,],1, FUN = norm_vec))
colnames(citingNs) <- "citingN"
citingNs$ref <- rownames(citingNs)
orderedName <- data.frame(V(g.cit)$name)
citingN <- data.frame(orderedName, citingNs[match(orderedName$g.cit..name, citingNs$ref),])[,"citingN"]
citingN <- totalTerms[!is.na(citingN)] 


clln.cit <- cluster_louvain(g.cit)
layout <- layout_nicely(g.cit,2)
g.cit$layout <- layout
plot(g.cit, edge.width = sqrt(cs.cit$cosSim) * 2, vertex.size = citingN,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.cit))


write.csv(cosSim[order(-cosSim$cosSim),], "SimilarCitations.csv")


####### import full texts

cname <- "data/FullText/"
stat.comp<-  function( x,y){
  K <-length(unique(y))
  n <-length(x)
  m <-mean(x)
  TSS <-sum((x-m)^2)
  nk<-table(y)
  mk<-tapply(x,y,mean)
  BSS <-sum(nk* (mk-m)^2)
  result<-c(mk,100.0*BSS/TSS)
  names(result) <-c( paste("G",1:K),"% epl.")
  return(result)
}

dir(cname)

library(tm)
library(cluster)
library(SnowballC)

docs <- Corpus(DirSource(cname))   

summary(docs)   
docs
inspect(docs[2])
#docs = tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "`")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "_")
docs <- tm_map(docs, toSpace, "–")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
#docs <- tm_map(docs, stemDocument)    


dtm <- DocumentTermMatrix(docs)   
dtm 
dtm.m = as.matrix(dtm)
tdm <- TermDocumentMatrix(docs)   
tdm  
tdm.m = as.matrix(tdm)
n = as.data.frame(rownames(tdm))
colnames(n) = c("n")
n$l = nchar(as.character(n$n))
head(n[order(-n$l),], 20)



 tdm.df = as.data.frame(tdm.m)
 write.csv(as.data.frame(tdm.df),"TermsByDoc.csv")

 tdm.df.f <- tdm.df / colSums(tdm.df)
 head(tdm.df.f)
 write.csv(as.data.frame(tdm.df),"frequencyUseTermsByDoc.csv")
 
 
########## intro stats
 
 ########## network of term similarity
 
 termmat <- t(as.matrix(tdm.df.f))
 
refSimTerm <- rownames(termmat)
 cosSimTerm <- data.frame()
 k=0
 ilist <- c()
 for(i in refSimTerm){
   ilist <- c(ilist, i)
   for(j in refSimTerm){
     if (j %!in% ilist){
       k <- k + 1
       cosSimTerm[k,1] <- i
       cosSimTerm[k,2] <- j
       vi <- termmat[i,]
       vj <- termmat[j,]
       cosSimTerm[k,3] <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
     }
   }
 }
 colnames(cosSimTerm) <- c('i', 'j', 'cosSimTerm')
 summary(cosSimTerm)
 head(cosSimTerm)
 
#totalTerm <- apply(termmat,1, FUN = norm_vec)

summary(cosSimTerm)
 cs <- cosSimTerm[cosSimTerm$cosSimTerm > 0.7,]
 g.term <- graph_from_data_frame(cs, directed=F)
 totalTerm <- as.data.frame(apply(termmat[rownames(termmat) %in% V(g.term)$name,],1, FUN = norm_vec))
 colnames(totalTerm) <- "totalTerms"
 totalTerm$ref <- rownames(totalTerm)
 orderedName <- data.frame(V(g.term)$name)
 totalTerms <- data.frame(orderedName, totalTerm[match(orderedName$V.g.term..name, totalTerm$ref),])[,"totalTerms"]
 totalTerms <- totalTerms[!is.na(totalTerms)] 
 clln.term <- cluster_louvain(g.term)
 layout <- layout_nicely(g.term,2)
 g.term$layout <- layout
 plot(g.term, edge.width = sqrt(cs$cosSimTerm) * 2, vertex.size = totalTerms *50,
      vertex.label.cex = 0.7,  
      edge.curved=.2, vertex.color = membership(clln.term))
 
 
write.csv(cosSimTerm[order(-cosSimTerm$cosSimTerm),], "SimilarWording.csv")
 
# membershipTableTerms <- data.frame("IDtxt" = clln.term$names, "groupTerm" = clln.term$membership)
# membershipTableTerms$ID <- substr(membershipTableTerms$IDtxt, 1, 8)
# membershipTableCit <- data.frame("IDtxt" = clln.cit$names, "groupTerm" = clln.cit$membership)
# 
# 

pap = "And05Reg"
pap = "Del04Con"
 cosSimTerm$ij <- paste0(cosSimTerm$i, "_", cosSimTerm$j)
 cosSimTerm$Paper <- ifelse(cosSimTerm$i == paste0(pap, ".txt") | cosSimTerm$j == paste0(pap, ".txt"), 1, 0)
 summary(cosSimTerm)
 cosSimTer <- cosSimTerm[,c("ij","cosSimTerm", "Paper")]
 PairCompa <- cosSim
 summary(cosSim)
 PairCompa$ij <- paste0(PairCompa$i, ".txt_", PairCompa$j, ".txt")
 PairCompa$ji <- paste0(PairCompa$j, ".txt_", PairCompa$i, ".txt")
 PairCompa$CitationSimilarity <- PairCompa$cosSim
 PairCompa <- PairCompa[,c("ij", "ji", "CitationSimilarity")]
 PairCompa <- data.frame(PairCompa, cosSimTer[match(PairCompa$ij, cosSimTer$ij),])
 PairCompa <- data.frame(PairCompa, cosSimTer[match(PairCompa$ji, cosSimTer$ij),])
 PairCompa$TermSimilarity <- ifelse(!is.na(PairCompa$cosSimTerm), PairCompa$cosSimTerm, PairCompa$cosSimTerm.1)
 PairCompa$Papers <- as.factor(ifelse(PairCompa$Paper == 1 | PairCompa$Paper.1 == 1, pap, "Other"))
  summary(PairCompa)
 PairCompaTable <- PairCompa[,c("ij", "CitationSimilarity", "TermSimilarity", "Papers")]
 summary(PairCompaTable)
 
 q <- ggplot(PairCompaTable, aes(x=CitationSimilarity, y = TermSimilarity, color = Papers))
 q + geom_point(cex=1) +
   labs(x="Citation Similarity", y="Wording Similarity")
 
 summary(lm(CitationSimilarity~TermSimilarity, data=PairCompaTable))
  PairCompaTable[order(PairCompaTable$TermSimilarity, PairCompaTable$CitationSimilarity),]
  PairCompaTable[order(-PairCompaTable$CitationSimilarity, -PairCompaTable$TermSimilarity),]
  
  
  paperList <- colnames(cites_out[,6:71])
  
  ############ repatriate estimates and data from articles
  
  # estimates by countries
  
  data <- read.csv2("data/zipf_meta.csv", sep=",", dec=".")
 countryData <- data[data$TERRITORY_TYPE == "Country" & data$REFID %in% paperList,]
country2Ref <- table(countryData$CNTR_ID, countryData$REFID)[-1,]
dim(country2Ref)
country2Ref.mat <- as.matrix(country2Ref[rowSums(country2Ref)>0,colnames(country2Ref) %in% paperList])
country2Ref.mat[country2Ref.mat>0] <- 1
countrymat <- t(country2Ref.mat)

refSim <- rownames(countrymat)
cosSimC <- data.frame()
k=0
ilist <- c()
for(i in refSim){
  ilist <- c(ilist, i)
  for(j in refSim){
    if (j %!in% ilist){
      k <- k + 1
      cosSimC[k,1] <- i
      cosSimC[k,2] <- j
      vi <- countrymat[i,]
      vj <- countrymat[j,]
      s <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
      cosSimC[k,3] <- ifelse(!is.na(s), s, 0)
    }
  }
}
colnames(cosSimC) <- c('i', 'j', 'cosSimC')
head(cosSimC)
summary(cosSimC)


cs.cntr <- cosSimC[cosSimC$cosSimC >= 0.2,]

#countryN <- apply(countrymat[rownames(countrymat) %in% unique(c(cs.cntr$i, cs.cntr$j)),],1, FUN = norm_vec)

g.cntr <- graph_from_data_frame(cs.cntr, directed=F)
countryN <- as.data.frame(apply(countrymat[rownames(countrymat) %in% V(g.cntr)$name,],1, FUN = norm_vec))
colnames(countryN) <- "n_countries"
countryN$ref <- rownames(countryN)
orderedName <- data.frame(V(g.cntr)$name)
orderedCountryN <- data.frame(orderedName, countryN[match(orderedName$V.g.cntr..name, countryN$ref),])[,"n_countries"]
orderedCountryN <- orderedCountryN[!is.na(orderedCountryN)] 
#V(g.cntr)$size <- orderedCountryN

clln.cntr <- cluster_louvain(g.cntr)
layout <- layout_nicely(g.cntr,2)
g.cntr$layout <- layout
plot(g.cntr, edge.width = sqrt(cs.cntr$cosSimC) * 2, vertex.size = orderedCountryN * 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.cntr))


write.csv(cosSimC[order(-cosSimC$cosSimC),], "SimilarCountries.csv")



# estimates by city definition

cityData <- data[!is.na(data$URBANSCALE) & data$REFID %in% paperList,]
city2Ref <- table(cityData$URBANSCALE, cityData$REFID)[,]
dim(city2Ref)
city2Ref.mat <- as.matrix(city2Ref[rowSums(city2Ref)>0,colnames(city2Ref) %in% paperList])
city2Ref.mat[city2Ref.mat>0] <- 1
citymat <- t(city2Ref.mat)

refSim <- rownames(citymat)
cosSimCt <- data.frame()
k=0
ilist <- c()
for(i in refSim){
  ilist <- c(ilist, i)
  for(j in refSim){
    if (j %!in% ilist){
      k <- k + 1
      cosSimCt[k,1] <- i
      cosSimCt[k,2] <- j
      vi <- citymat[i,]
      vj <- citymat[j,]
      s <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
      cosSimCt[k,3] <- ifelse(!is.na(s), s, 0)
    }
  }
}
colnames(cosSimCt) <- c('i', 'j', 'cosSimCt')
head(cosSimCt)
summary(cosSimCt)


cs.city <- cosSimCt[cosSimCt$cosSimCt >= 0.1,]

#cityN <- apply(citymat[rownames(citymat) %in% unique(c(cs.city$i, cs.city$j)),],1, FUN = norm_vec)

g.city <- graph_from_data_frame(cs.city, directed=F)
cityN <- as.data.frame(apply(citymat[rownames(citymat) %in% V(g.city)$name,],1, FUN = norm_vec))
colnames(cityN) <- "n_cities"
cityN$ref <- rownames(cityN)
orderedName <- data.frame(V(g.city)$name)
orderedcityN <- data.frame(orderedName, cityN[match(orderedName$V.g.city..name, cityN$ref),])[,"n_cities"]
orderedcityN <- orderedcityN[!is.na(orderedcityN)] 
#V(g.city)$size <- orderedcityN

clln.city <- cluster_louvain(g.city)
layout <- layout_nicely(g.city,2)
g.city$layout <- layout
plot(g.city, edge.width = sqrt(cs.city$cosSimCt) * 2, vertex.size = orderedcityN * 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.city))


write.csv(cosSimCt[order(-cosSimCt$cosSimCt),], "Similarcities.csv")



# estimates by regression estimator

estimData <- data[!is.na(data$ESTIMATION) & data$REFID %in% paperList,]
estim2Ref <- table(estimData$ESTIMATION, estimData$REFID)[,]
dim(estim2Ref)
estim2Ref.mat <- as.matrix(estim2Ref[rowSums(estim2Ref)>0,colnames(estim2Ref) %in% paperList])
estim2Ref.mat[estim2Ref.mat>0] <- 1
estimmat <- t(estim2Ref.mat)

refSim <- rownames(estimmat)
cosSimEs <- data.frame()
k=0
ilist <- c()
for(i in refSim){
  ilist <- c(ilist, i)
  for(j in refSim){
    if (j %!in% ilist){
      k <- k + 1
      cosSimEs[k,1] <- i
      cosSimEs[k,2] <- j
      vi <- estimmat[i,]
      vj <- estimmat[j,]
      s <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
      cosSimEs[k,3] <- ifelse(!is.na(s), s, 0)
    }
  }
}
colnames(cosSimEs) <- c('i', 'j', 'cosSimEs')
head(cosSimEs)
summary(cosSimEs)


cs.estim <- cosSimEs[cosSimEs$cosSimEs >= 0.7,]

#estimN <- apply(estimmat[rownames(estimmat) %in% unique(c(cs.estim$i, cs.estim$j)),],1, FUN = norm_vec)

g.estim <- graph_from_data_frame(cs.estim, directed=F)
estimN <- as.data.frame(apply(estimmat[rownames(estimmat) %in% V(g.estim)$name,],1, FUN = norm_vec))
colnames(estimN) <- "n_estims"
estimN$ref <- rownames(estimN)
orderedName <- data.frame(V(g.estim)$name)
orderedestimN <- data.frame(orderedName, estimN[match(orderedName$V.g.estim..name, estimN$ref),])[,"n_estims"]
orderedestimN <- orderedestimN[!is.na(orderedestimN)] 
#V(g.estim)$size <- orderedestimN

clln.estim <- cluster_louvain(g.estim)
layout <- layout_nicely(g.estim,2)
g.estim$layout <- layout
plot(g.estim, edge.width = sqrt(cs.estim$cosSimEs) * 2, vertex.size = orderedestimN * 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.estim))


write.csv(cosSimEs[order(-cosSimEs$cosSimEs),], "Similarestims.csv")

# similarity of journal
# similarity of discipline
# similarity of estimate mean value
# similarity of estimate variance
# similarity of year

# predict citation?


