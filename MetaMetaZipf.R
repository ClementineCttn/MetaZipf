setwd("~/Documents/MetaZipf")
library(igraph)
library(scales)
library(wesanderson)
library(ggplot2)
library(ggrepel)
library(ggalt)
library(gridExtra)
library(data.table)
library(stringr)
library(tm)
library(cluster)
library(SnowballC)
library(reshape2)

'%!in%' <- function(x,y)!('%in%'(x,y))
norm_vec <- function(x) sqrt(sumNum(x^2))

sumNum = function(x) { 
  s = sum(x, na.rm = T)
  return(s)
}
meanNum = function(x) { 
  s = mean(x, na.rm = T)
  return(s)
}

sdNum = function(x) { 
  s = sd(x, na.rm = T)
  return(s)
}

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})

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

## import data
cites <- read.csv2("data/zipf_cites.csv", sep=',')


head(cites)
cites_out <- cites[67:1221,]
cites_out$YEAR <- as.numeric(gsub("\\D+", "", cites_out$REFID))
cites_out$JOURNAL <- gsub(".*\\d.", "", cites_out$REFID)
cites_out$AUTHOR <- gsub("\\d.*", "", cites_out$REFID)
cites_out$JOURNAL <- gsub("[[:punct:]]", " ", cites_out$JOURNAL)
cites_out$AUTHOR <- gsub("[[:punct:]]", " ", cites_out$AUTHOR)

J2D <- read.csv2('journals2Disciplines.csv', sep=';')
summary(J2D)

cites_out <- data.frame(cites_out,J2D[match(cites_out$JOURNAL, J2D$JOURNAL),])
summary(cites_out)
cites_out$DISCIPLINE <- cites_out$DISCPLINE
cites_out$DISCPLINE <- NULL
cites_out$JOURNAL.1 <- NULL
dim(cites)
dim(cites_out)

cites <- rbind(cites[1:66,],cites_out)
tail(cites)
cites[is.na(cites)] <- 0
cites_within <- cites[1:66,c(1,6:71)]
tail(cites_within)
rownames(cites_within) <- cites_within$REFID
cites_within$REFID <- NULL
cmat <- t(as.matrix(cites_within))
# 
# 
# dim(cmat)
# 
# node.size <- colSums(cmat) +1
# 
# 
# cite_year <- cites[1:66,c('REFID', 'YEAR')]
# ids <- cite_year$REFID
# rownames(cite_year) <- ids
# cite_year$REFID <- NULL
# node.year <- cite_year$YEAR - 1974
# 
# cite_disc <- cites[1:66,c('REFID', 'DISCIPLINE')]
# rownames(cite_disc) <- ids
# cite_disc$REFID <- NULL
# node.disc <- as.character(cite_disc$DISCIPLINE)
# 
# 
# node.disc[node.disc == "ECO"] <- "slateblue3"
# node.disc[node.disc == "REG"] <- "orangered"
# node.disc[node.disc == "GEO"] <- "darkgoldenrod2"
# node.disc[node.disc == "PHY"] <- "seagreen3"




# ########### Network of intra citations
# 
# g <- graph_from_adjacency_matrix(cmat, mode = "directed", diag = F)
# 
# layout <- layout_nicely(g,2)
# g$layout <- layout
# fine = 500 # this will adjust the resolving power.
# pal.lab = colorRampPalette(c('grey30','black'))
# graphCol.lab = pal.lab(fine)[as.numeric(cut(node.year,breaks = fine))]
# 
# par(mar = c(0.5, 0.3, 0.5, 0.5))
# 
# plot(g, vertex.size=node.size*0.5,
#      vertex.label.dist=0.5, 
#      vertex.color= alpha("grey",0.6), edge.color = "black", 
#      vertex.label.cex = 0.7,  vertex.label.color = node.disc,
#      vertex.label.font = 2,  
#      edge.arrow.size=0.2)
# 
# solos = c("Sua80Jou", "Par83Jou", "Oka79Reg", "Mir86Urb", "Pop74Sov")
# cmat_linked <- cmat[rownames(cmat) %!in% solos, colnames(cmat) %!in% solos]
# dim(cmat_linked)
# 
# node.size <- colSums(cmat_linked) +1
# 
# cite_year_linked <- cite_year[rownames(cite_year) %!in% solos, ]
# node.year_linked <- cite_year_linked - 1974
# 
# cite_disc_linked <- cite_disc[rownames(cite_disc) %!in% solos, ]
# node.disc_linked <- as.character(cite_disc_linked)
# 
# 
# node.disc_linked[node.disc_linked == "ECO"] <- "slateblue3"
# node.disc_linked[node.disc_linked == "REG"] <- "orangered"
# node.disc_linked[node.disc_linked == "GEO"] <- "darkgoldenrod2"
# node.disc_linked[node.disc_linked == "PHY"] <- "seagreen3"
# 
# 
# g <- graph_from_adjacency_matrix(cmat_linked, mode = "directed", diag = F)
# 
# layout <- layout_nicely(g,2)
# g$layout <- layout
# 
# graphCol.lab = pal.lab(fine)[as.numeric(cut(node.year_linked,breaks = fine))]
# 
# 
# plot(g, vertex.size=node.size*0.5,
#      vertex.label.dist=0.5, 
#      vertex.color= alpha("grey",0.6), edge.color = "black", 
#      vertex.label.cex = 0.7,  vertex.label.color = node.disc_linked,
#      vertex.label.font = 2,  
#      edge.arrow.size=0.2)



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


# ########### network between disciplines
# 
# cites_long <- melt(cmat)
# head(cites_long)
# cites_long_pos <- cites_long[cites_long$value > 0,]
# head(cites_long_pos)
# j_citing <- j
# colnames(j_citing) <- paste("citing", colnames(j), sep="_")
# clpd <- data.frame(cites_long_pos, j_citing[match(cites_long_pos$Var1, j_citing$citing_REFID),])
# j_cited <- j
# colnames(j_cited) <- paste("cited", colnames(j), sep="_")
# clpd <- data.frame(clpd, j_cited[match(clpd$Var2, j_cited$cited_REFID),])
# head(clpd)
# 
# dmat <- dcast(clpd, citing_DISCIPLINE ~ cited_DISCIPLINE, sum)
# dmatm <- melt(dmat)
# 
# 
# 
# gd <- graph_from_data_frame(dmatm, directed = T)
# vd <- as.data.frame(names(V(gd)))
# colnames(vd) <- "discipline"
# disciplines <- aggregate(j[,"n"], by=list(j[,"DISCIPLINE"]), FUN = sum)
# 
# size.nodes <- data.frame(vd, disciplines[match(vd$discipline, disciplines$Group.1),])
# size.nodes.d <- size.nodes$x
# 
# plot(gd,edge.width = dmatm$value * 0.5, vertex.size = size.nodes.d * 3,
#      vertex.label.cex = 0.7,  edge.curved=.8)


# ########### network between journals
# 
# dmat <- dcast(clpd, citing_JOURNAL ~ cited_JOURNAL, sum)
# dmatm <- melt(dmat)
# dmatmp <- dmatm[dmatm$value>1,]
# 
# gj <- graph_from_data_frame(dmatmp, directed = T)
# 
# v <- as.data.frame(names(V(gj)))
# colnames(v) <- "journal"
# 
# size.nodes <- data.frame(v, journals[match(v$journal, journals$Group.1),])
# size.nodes.l <- size.nodes$x
# size.nodes
# 
# plot(gj,edge.width = dmatmp$value, vertex.label.cex = 0.7,  edge.curved=.4,
#      vertex.size = size.nodes.l * 3)



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
# 
# # 
# cites_out$YEAR <- as.numeric(gsub("\\D+", "", cites_out$REFID))
# cites_out$JOURNAL <- gsub(".*\\d.", "", cites_out$REFID)
# cites_out$AUTHOR <- gsub("\\d.*", "", cites_out$REFID)
# cites_out$JOURNAL <- gsub("[[:punct:]]", " ", cites_out$JOURNAL)
# cites_out$AUTHOR <- gsub("[[:punct:]]", " ", cites_out$AUTHOR)
# yearInter <- round(mean(cites_out$YEAR, na.rm = T), digit=0)
# write.csv(yearInter, "average_year_publi_external_all.csv")
# # 
# head(cites_out)
# summary(as.factor(cites_out$JOURNAL))/dim(cites_out)[1]
# # Diverse pool of citations. MAx % of journals cited = 2.5% (Journal Regional Science) and 2.3 (Urban Studies)
#jou <- as.data.frame(summary(as.factor(cites_out$JOURNAL)))
colnames(cites_out)
cites_out <- cites[67:1221,]
cites_out$n_cites <- rowSums(cites_out[,6:71])
single_outcites <- cites_out[order(-cites_out$n_cites),c(1:5,72)] 
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

## look for articles who do not cite Zipf (+ summary by year and discipline of publi)
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
                   alpha = 0.4, position = "identity", binwidth = 1) +
  geom_density(aes(color = ref_zipf), size = 1) +
  scale_color_manual(values = c("Coral2", "dodgerblue3")) +
scale_fill_manual(values = c("Coral2", "dodgerblue3"))  + labs(color = "") + guides(fill = FALSE, size = FALSE)+ theme(legend.position = "top")

table(rfczpf$ref_zipf, rfczpf$DISCIPLINE)

## Which are the authors cited from Urban Studies
cites_out[cites_out$JOURNAL == "Urban Studies","AUTHOR"]

## look for total citations per corpus paper
n_citations <- as.data.frame(colSums(cites_out[,6:71], na.rm=T))
colnames(n_citations) <- "n_citations"
n_citations$REFID <- rownames(n_citations)
n_citations_corpus <- data.frame(cites_for_ref_zipfs, n_citations[match(cites_for_ref_zipfs$REFID, n_citations$REFID),])
n_citations_corpus$ref <- paste(n_citations_corpus$AUTHOR, n_citations_corpus$YEAR, sep=" ")

q <- ggplot(n_citations_corpus, aes(x=ref, y = n_citations))
q + geom_lollipop(aes(reorder(ref, -n_citations)),color = "dodgerblue3", cex=1) +
  coord_flip() +
  labs(x="Reference", y="Size of bibliography by corpus article")

q <- ggplot(n_citations_corpus, aes(y=n_citations, x = DISCIPLINE,fill = DISCIPLINE))
q + geom_boxplot(alpha = 0.4) + theme(legend.position = "none")


n_citations_corpus[order(-n_citations_corpus$n_citations),c("ref", "n_citations")]


#####

jou_out <- as.data.frame(table(cites_out$JOURNAL))
jou_out <- jou_out[order(-jou_out$Freq),] 
colnames(jou_out) <- c("ref", "n")
jou <- subset(jou_out[-100,], n>=5)
q <- ggplot(jou, aes(x=ref, y = n))
q + geom_lollipop(aes(reorder(ref, -n)),color = "goldenrod3", cex=1) +
  coord_flip() +
  labs(x="Journal", y="Number of citations from the corpus (>=5)")

# 
# jou_test<- data.frame(jou, J2D[match(jou$ref,J2D$JOURNAL),])
# write.csv2(jou_test,"test_scimago.csv")


  
####### outcitation bipartite network

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





######### Bipartite Network of similarity between in papers based on out papers they cite
dim(cites_out)
outcitemat <- as.matrix(cites_out[,6:71])
toutcitemat <- t(outcitemat)

dim(toutcitemat)
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
summary(cosSim$cosSim)
write.csv(cosSim[order(-cosSim$cosSim),], "simNets/SimilarCitations.csv")

#citingN <- apply(toutcitemat,1, FUN = norm_vec)

cs.cit <- cosSim[cosSim$cosSim >= 0.25,]
g.cit <- graph_from_data_frame(cs.cit, directed=F)


citingNs <- as.data.frame(apply(toutcitemat[rownames(toutcitemat) %in% V(g.cit)$name,],1, FUN = norm_vec))
colnames(citingNs) <- "citingN"
citingNs$ref <- rownames(citingNs)
orderedName <- data.frame(V(g.cit)$name)
citingN <- data.frame(orderedName, citingNs[match(orderedName$V.g.cit..name, citingNs$ref),])[,"citingN"]
citingN <- citingN[!is.na(citingN)] 

clln.cit <- cluster_louvain(g.cit)
layout <- layout_nicely(g.cit,2)
g.cit$layout <- layout
plot(g.cit, edge.width = sqrt(cs.cit$cosSim) * 2, vertex.size = citingN,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.cit))




######### Bipartite Network of similarity between in papers based on the discpline of references they cite
summary(as.factor(cites[1:66,"DISCIPLINE"]))
summary(as.factor(cites_out$DISCIPLINE))
discipline2ref <- aggregate(cites_out[,c(6:71)], by=list(cites_out$DISCIPLINE), FUN = sumNum)
rownames(discipline2ref) <- discipline2ref$Group.1
discipline2ref$Group.1 <- NULL

cols <- colnames(discipline2ref)
for(i in cols){
  absolute <- discipline2ref[,i]
  total <- sum(absolute)
  relative <- absolute/total
  discipline2ref[,paste0("freq_",i)] <- relative
}

summary(discipline2ref)
dim(discipline2ref)

# #ABSOLUTE NUMBERS
# disc2ref <- as.matrix(discipline2ref[,1:66])
# str(disc2ref)
# scaled_disc2ref <- disc2ref / colSums(disc2ref)
# tdisc2ref <- t(disc2ref)
# 
# refSim <- rownames(tdisc2ref)
# cosSim <- data.frame()
# k=0
# ilist <- c()
# for(i in refSim){
#   ilist <- c(ilist, i)
#   for(j in refSim){
#     if (j %!in% ilist){
#       k <- k + 1
#       cosSim[k,1] <- i
#       cosSim[k,2] <- j
#       vi <- tdisc2ref[i,]
#       vj <- tdisc2ref[j,]
#       cosSim[k,3] <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
#     }
#   }
# }
# colnames(cosSim) <- c('i', 'j', 'cosSim')
# dim(cosSim)
# head(cosSim)
# summary(cosSim$cosSim)
# write.csv(cosSim[order(-cosSim$cosSim),], "simNets/SimilarDisciplinesCited_abs.csv")
# head(cosSim[order(-cosSim$cosSim),])
# #citingN <- apply(toutcitemat,1, FUN = norm_vec)
# 
# cs.cit <- cosSim[cosSim$cosSim >= 0.9,]
# g.cit <- graph_from_data_frame(cs.cit, directed=F)
# 
# disCitedNs <- as.data.frame(apply(toutcitemat[rownames(tdisc2ref) %in% V(g.cit)$name,],1, FUN = norm_vec))
# colnames(disCitedNs) <- "disCitedNs"
# disCitedNs$ref <- rownames(disCitedNs)
# orderedName <- data.frame(V(g.cit)$name)
# disCitedNs <- data.frame(orderedName, disCitedNs[match(orderedName$V.g.cit..name, disCitedNs$ref),])[,"disCitedNs"]
# disCitedNs <- disCitedNs[!is.na(disCitedNs)] 
# 
# colnames(orderedName) <- "REFID"
# refinfo <- cites[1:66,c("REFID", "DISCIPLINE")]
# own_discipline <- data.frame(orderedName, refinfo[match(orderedName$REFID, refinfo$REFID),])
# odisc <- own_discipline$DISCIPLINE
#   
# #clln.cit <- cluster_louvain(g.cit)
# layout <- layout_nicely(g.cit,2)
# g.cit$layout <- layout
# plot(g.cit, edge.width = sqrt(cs.cit$cosSim) * 2, vertex.size = disCitedNs,
#      vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = odisc)



#RELATIVE NUMBERS
disc2ref <- as.matrix(discipline2ref[,67:132])
colnames(disc2ref) <- cols
str(disc2ref)
scaled_disc2ref <- disc2ref / colSums(disc2ref)
tdisc2ref <- t(disc2ref)

refSim <- rownames(tdisc2ref)
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
      vi <- tdisc2ref[i,]
      vj <- tdisc2ref[j,]
      cosSim[k,3] <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj)) 
    }
  }
}
colnames(cosSim) <- c('i', 'j', 'cosSim')
dim(cosSim)
head(cosSim)
summary(cosSim$cosSim)
write.csv(cosSim[order(-cosSim$cosSim),], "simNets/SimilarDisciplinesCited_rel.csv")
head(cosSim[order(-cosSim$cosSim),])
#citingN <- apply(toutcitemat,1, FUN = norm_vec)

cs.cit <- cosSim[cosSim$cosSim >= 0.9,]
g.cit <- graph_from_data_frame(cs.cit, directed=F)

disCitedNs <- as.data.frame(apply(toutcitemat[rownames(tdisc2ref) %in% V(g.cit)$name,],1, FUN = norm_vec))
colnames(disCitedNs) <- "disCitedNs"
disCitedNs$ref <- rownames(disCitedNs)
orderedName <- data.frame(V(g.cit)$name)
disCitedNs <- data.frame(orderedName, disCitedNs[match(orderedName$V.g.cit..name, disCitedNs$ref),])[,"disCitedNs"]
disCitedNs <- disCitedNs[!is.na(disCitedNs)] 

colnames(orderedName) <- "REFID"
refinfo <- cites[1:66,c("REFID", "DISCIPLINE")]
own_discipline <- data.frame(orderedName, refinfo[match(orderedName$REFID, refinfo$REFID),])
odisc <- own_discipline$DISCIPLINE

#clln.cit <- cluster_louvain(g.cit)
layout <- layout_nicely(g.cit,2)
g.cit$layout <- layout
plot(g.cit, edge.width = sqrt(cs.cit$cosSim) * 2, vertex.size = disCitedNs,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = odisc)



####### import full texts

cname <- "data/FullText/"
dir(cname)
docs <- Corpus(DirSource(cname))   

summary(docs)   
docs
inspect(docs[2])
#docs = tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

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
 write.csv(as.data.frame(tdm.df.f),"frequencyUseTermsByDoc.csv")
 
 
 
 
 ## look for total words per corpus paper
 n_words <- as.data.frame(colSums(tdm.df, na.rm=T))
 colnames(n_words) <- "n_words"
 n_words$REFID <- substr(rownames(n_words), 1,8)
 n_words_corpus <- data.frame(cites_for_ref_zipfs, n_words[match(cites_for_ref_zipfs$REFID, n_words$REFID),])
 n_words_corpus$ref <- paste(n_words_corpus$AUTHOR, n_words_corpus$YEAR, sep=" ")
 n_words_corpus <- n_words_corpus[n_words_corpus$AUTHOR != "Krakover S.",]
 q <- ggplot(n_words_corpus, aes(x=ref, y = n_words))
 q + geom_lollipop(aes(reorder(ref, -n_words)),color = "seagreen4", cex=1) +
   coord_flip() +
   labs(x="Reference", y="Number of words by corpus article")
 
 q <- ggplot(n_words_corpus, aes(y=n_words, x = DISCIPLINE,fill = DISCIPLINE))
 q + geom_boxplot(alpha = 0.4) + theme(legend.position = "none")
 
 plot(n_words_corpus$n_words,n_words_corpus$YEAR)
 
 
 
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
# 
# pap = "And05Reg"
# pap = "Del04Con"
#  cosSimTerm$ij <- paste0(cosSimTerm$i, "_", cosSimTerm$j)
#  cosSimTerm$Paper <- ifelse(cosSimTerm$i == paste0(pap, ".txt") | cosSimTerm$j == paste0(pap, ".txt"), 1, 0)
#  summary(cosSimTerm)
#  cosSimTer <- cosSimTerm[,c("ij","cosSimTerm", "Paper")]
#  PairCompa <- cosSim
#  summary(cosSim)
#  PairCompa$ij <- paste0(PairCompa$i, ".txt_", PairCompa$j, ".txt")
#  PairCompa$ji <- paste0(PairCompa$j, ".txt_", PairCompa$i, ".txt")
#  PairCompa$CitationSimilarity <- PairCompa$cosSim
#  PairCompa <- PairCompa[,c("ij", "ji", "CitationSimilarity")]
#  PairCompa <- data.frame(PairCompa, cosSimTer[match(PairCompa$ij, cosSimTer$ij),])
#  PairCompa <- data.frame(PairCompa, cosSimTer[match(PairCompa$ji, cosSimTer$ij),])
#  PairCompa$TermSimilarity <- ifelse(!is.na(PairCompa$cosSimTerm), PairCompa$cosSimTerm, PairCompa$cosSimTerm.1)
#  PairCompa$Papers <- as.factor(ifelse(PairCompa$Paper == 1 | PairCompa$Paper.1 == 1, pap, "Other"))
#   summary(PairCompa)
#  PairCompaTable <- PairCompa[,c("ij", "CitationSimilarity", "TermSimilarity", "Papers")]
#  summary(PairCompaTable)
#  
#  q <- ggplot(PairCompaTable, aes(x=CitationSimilarity, y = TermSimilarity, color = Papers))
#  q + geom_point(cex=1) +
#    labs(x="Citation Similarity", y="Wording Similarity")
 
 # summary(lm(CitationSimilarity~TermSimilarity, data=PairCompaTable))
 #  PairCompaTable[order(PairCompaTable$TermSimilarity, PairCompaTable$CitationSimilarity),]
 #  PairCompaTable[order(-PairCompaTable$CitationSimilarity, -PairCompaTable$TermSimilarity),]
 #  
  
  
  
  
  
  
  
  ############ repatriate estimates and data from articles
  
  ##### similarity network of countroes studied
  
  data <- read.csv2("data/zipf_meta.csv", sep=",", dec=".")
  
  
  paperList <- colnames(cites_out[,6:71])
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

write.csv(cosSimC[order(-cosSimC$cosSimC),], "simNets/SimilarCountries.csv")



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




##### similarity network of city definition used

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

write.csv(cosSimCt[order(-cosSimCt$cosSimCt),], "simNets/SimilarCities.csv")


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





##### similarity network of decades studied

str(data)
decadeData <- data[!is.na(data$DATE) & data$REFID %in% paperList,]
decadeData$DECADE <- paste0(substr(as.character(decadeData$DATE), 1, 3), 0, "s")
decade2Ref <- table(decadeData$DECADE, decadeData$REFID)[,]
dim(decade2Ref)
decade2Ref.mat <- as.matrix(decade2Ref[rowSums(decade2Ref)>0,colnames(decade2Ref) %in% paperList])
decade2Ref.mat[decade2Ref.mat>0] <- 1
decademat <- t(decade2Ref.mat)

 refSim <- rownames(decademat)
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
      vi <- decademat[i,]
      vj <- decademat[j,]
      s <- (sumNum(vi * vj)) / (norm_vec(vi) *  norm_vec(vj))
      cosSimCt[k,3] <- ifelse(!is.na(s), s, 0)
    }
  }
}
colnames(cosSimCt) <- c('i', 'j', 'cosSimCt')
head(cosSimCt)
summary(cosSimCt$cosSimCt)


write.csv(cosSimCt[order(-cosSimCt$cosSimCt),], "simNets/SimilarDecades.csv")


cs.decade <- cosSimCt[cosSimCt$cosSimCt >= 0.65,]

#decadeN <- apply(decademat[rownames(decademat) %in% unique(c(cs.decade$i, cs.decade$j)),],1, FUN = norm_vec)

g.decade <- graph_from_data_frame(cs.decade, directed=F)
decadeN <- as.data.frame(apply(decademat[rownames(decademat) %in% V(g.decade)$name,],1, FUN = norm_vec))
colnames(decadeN) <- "n_decades"
decadeN$ref <- rownames(decadeN)
orderedName <- data.frame(V(g.decade)$name)
ordereddecadeN <- data.frame(orderedName, decadeN[match(orderedName$V.g.decade..name, decadeN$ref),])[,"n_decades"]
ordereddecadeN <- ordereddecadeN[!is.na(ordereddecadeN)]
#V(g.decade)$size <- ordereddecadeN

clln.decade <- cluster_louvain(g.decade)
layout <- layout_nicely(g.decade,2)
g.decade$layout <- layout
plot(g.decade, edge.width = sqrt(cs.decade$cosSimCt) * 2, vertex.size = ordereddecadeN * 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.decade))





##### similarity of alpha(s) estimated
head(data)
alphaData <- data[!is.na(data$ALPHALOTKA) & data$REFID %in% paperList,]
alphaData$n <- 1
head(alphaData)
meanAlphaPerRef <- aggregate(alphaData[,"ALPHALOTKA"], by=list(alphaData$REFID), FUN=meanNum)
sdAlphaPerRef <- aggregate(alphaData[,"ALPHALOTKA"], by=list(alphaData$REFID), FUN=sdNum)
sdAlphaPerRef[is.na(sdAlphaPerRef$x),"x"] <- 0
nAlphaPerRef <- aggregate(alphaData[,"n"], by=list(alphaData$REFID), FUN=sumNum)

similarAlpha <- cbind(meanAlphaPerRef, sdAlphaPerRef, nAlphaPerRef)
rownames(similarAlpha) <- similarAlpha$Group.1
similarAlpha[,c(1,3, 5)] <- NULL
colnames(similarAlpha) <- c("meanAlpha", "sdAlpha", "nAlpha")
write.csv2(similarAlpha, "summary_alpha_by_ref.csv")

alphamat <- as.matrix(similarAlpha)

meanData <- mean(alphaData[,"ALPHALOTKA"])
sdData <- sd(alphaData[,"ALPHALOTKA"])
meanN <- mean(similarAlpha$nAlpha)

refSim <- rownames(alphamat)
diff_mean <- data.frame()
diff_sd <- data.frame()
diff_n <- data.frame()
k=0
ilist <- c()
for(i in refSim){
  ilist <- c(ilist, i)
  for(j in refSim){
    if (j %!in% ilist){
      k <- k + 1
      diff_mean[k,1] <- i
      diff_mean[k,2] <- j
      mi <- similarAlpha[i,1]
      mj <- similarAlpha[j,1]
      m <- abs((mi - mj) / meanData)
      diff_mean[k,3] <- m
      
      diff_sd[k,1] <- i
      diff_sd[k,2] <- j
      sdi <-similarAlpha[i,2]
      sdj <- similarAlpha[j,2]
      sd <- abs((sdi - sdj) / sdData)
      diff_sd[k,3] <- sd
      
      diff_n[k,1] <- i
      diff_n[k,2] <- j
      ni <-similarAlpha[i,3]
      nj <- similarAlpha[j,3]
      n <- abs((ni - nj) / meanN)
      diff_n[k,3] <- n
    }
  }
}
colnames(diff_mean) <- c('i', 'j', 'diff_mean')
head(diff_mean)
summary(diff_mean$diff_mean)
write.csv(diff_mean[order(diff_mean$diff_mean),], "simNets/diff_mean.csv")



####### visualisation of mean alpha

cs.alpha <- diff_mean[diff_mean$diff_mean < 0.025,]
g.alpha <- graph_from_data_frame(cs.alpha, directed=F)
alphaM <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.alpha)$name,],1, FUN = norm_vec))
colnames(alphaM) <- "mean_alphas"
alphaM$ref <- rownames(alphaM)
orderedName <- data.frame(V(g.alpha)$name)
orderedalphaM <- data.frame(orderedName, alphaM[match(orderedName$V.g.alpha..name, alphaM$ref),])[,"mean_alphas"]
orderedalphaM <- orderedalphaM[!is.na(orderedalphaM)]
#V(g.alpha)$size <- orderedalphaM

similarAlpha$REFID <- rownames(similarAlpha)
# 
#alphas <- similarAlpha[similarAlpha$REFID %in% names(membership(clln.alpha)),c("REFID", "meanAlpha")]

clln.alpha <- cluster_louvain(g.alpha)
layout <- layout_nicely(g.alpha,2)
g.alpha$layout <- layout

memb <- as.list(membership(clln.alpha))

vertexData <- data.frame(orderedREFID= names(membership(clln.alpha)))
vertexData <- data.frame(vertexData, similarAlpha[match(vertexData$orderedREFID, similarAlpha$REFID),])

for (r in vertexData$REFID){
  vertexData[vertexData$REFID == r,"memb_mean"] <- memb[r]
}
tail(vertexData)
  
plot(g.alpha, edge.width = sqrt(cs.alpha$diff_mean) * 2, vertex.size = vertexData$meanAlpha^2 * 5,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.alpha))

# mean average value of alpha reported by group
aggregate(vertexData[,"meanAlpha"], by=list(vertexData$memb_mean), FUN=meanNum)





###### visuation of sd alpha

colnames(diff_sd) <- c('i', 'j', 'diff_sd')
head(diff_sd)
summary(diff_sd$diff_sd)
write.csv(diff_sd[order(diff_sd$diff_sd),], "simNets/diff_sd.csv")

cs.alpha <- diff_sd[diff_sd$diff_sd < 0.1,]
g.alpha <- graph_from_data_frame(cs.alpha, directed=F)
alphaSD <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.alpha)$name,],1, FUN = norm_vec))
colnames(alphaSD) <- "mean_sds"
alphaSD$ref <- rownames(alphaSD)
orderedName <- data.frame(V(g.alpha)$name)
orderedalphaSD <- data.frame(orderedName, alphaSD[match(orderedName$V.g.alpha..name, alphaSD$ref),])[,"mean_sds"]
orderedalphaSD <- orderedalphaSD[!is.na(orderedalphaSD)]
#V(g.alpha)$size <- orderedalphaM
# 

clln.alpha <- cluster_louvain(g.alpha)
layout <- layout_nicely(g.alpha,2)
g.alpha$layout <- layout

memb <- as.list(membership(clln.alpha))

vertexData <- data.frame(orderedREFID= names(membership(clln.alpha)))
vertexData <- data.frame(vertexData, similarAlpha[match(vertexData$orderedREFID, similarAlpha$REFID),])

for (r in vertexData$REFID){
  vertexData[vertexData$REFID == r,"memb_sd"] <- memb[r]
}
tail(vertexData)

plot(g.alpha, edge.width = sqrt(cs.alpha$diff_sd) * 2, vertex.size = vertexData$sdAlpha * 50,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.alpha))

# mean average value of alpha reported by group
aggregate(vertexData[,"sdAlpha"], by=list(vertexData$memb_sd), FUN=meanNum)






######## visuation of n alpha

colnames(diff_n) <- c('i', 'j', 'diff_n')
head(diff_n)
summary(diff_n$diff_n)
write.csv(diff_n[order(diff_n$diff_n),], "simNets/diff_n.csv")

cs.alpha <- diff_n[diff_n$diff_n < 0.1,]
g.alpha <- graph_from_data_frame(cs.alpha, directed=F)
alphaN <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.alpha)$name,],1, FUN = norm_vec))
colnames(alphaN) <- "mean_ns"
alphaN$ref <- rownames(alphaN)
orderedName <- data.frame(V(g.alpha)$name)
orderedalphaN <- data.frame(orderedName, alphaN[match(orderedName$V.g.alpha..name, alphaN$ref),])[,"mean_ns"]
orderedalphaN <- orderedalphaN[!is.na(orderedalphaN)]
#V(g.alpha)$size <- orderedalphaM
# 

clln.alpha <- cluster_louvain(g.alpha)
layout <- layout_nicely(g.alpha,2)
g.alpha$layout <- layout

memb <- as.list(membership(clln.alpha))

vertexData <- data.frame(orderedREFID= names(membership(clln.alpha)))
vertexData <- data.frame(vertexData, similarAlpha[match(vertexData$orderedREFID, similarAlpha$REFID),])

for (r in vertexData$REFID){
  vertexData[vertexData$REFID == r,"memb_n"] <- memb[r]
}
tail(vertexData)

plot(g.alpha, edge.width = sqrt(cs.alpha$diff_n) * 2, vertex.size = vertexData$nAlpha * 0.4,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = membership(clln.alpha))

# mean average value of alpha reported by group
aggregate(vertexData[,"nAlpha"], by=list(vertexData$memb_n), FUN=meanNum)




################
## FINAL STEP
################



# model of similarity of alpha

# import diff in alpha means
meanAlphaDyads <- read.csv2("simNets/diff_mean.csv", sep=",", stringsAsFactors = F)[,-1]
meanAlphaDyads$dyadID <- paste(meanAlphaDyads$i, meanAlphaDyads$j, sep = "_")
meanAlphaDyads$meanAlphaSim <- -as.numeric(meanAlphaDyads$diff_mean)
meanAlphaDyads[,1:3] <- NULL
summary(meanAlphaDyads)
DYADS <- meanAlphaDyads

DYAD_ID_order <- DYADS$dyadID

# import diff in alpha sd
sdAlphaDyads <- read.csv2("simNets/diff_sd.csv", sep=",", stringsAsFactors = F)[,-1]
sdAlphaDyads$dyadIJ <- paste(substr(sdAlphaDyads$i, 1, 8), 
                             substr(sdAlphaDyads$j, 1, 8), sep = "_")
sdAlphaDyads$dyadJI <- paste(substr(sdAlphaDyads$i, 1, 8), 
                             substr(sdAlphaDyads$j, 1, 8), sep = "_")
sdAlphaDyads$dyadID <- ifelse(sdAlphaDyads$dyadIJ %in% DYAD_ID_order, sdAlphaDyads$dyadIJ, sdAlphaDyads$dyadJI)

sdAlphaDyads$sdAlphaSim <- -as.numeric(sdAlphaDyads$diff_sd)
sdAlphaDyads[,1:5] <- NULL
summary(sdAlphaDyads)
DYADS <- data.frame(DYADS, sdAlphaDyads[match(DYADS$dyadID, sdAlphaDyads$dyadID),"sdAlphaSim"])


# import diff in n alpha 
nAlphaDyads <- read.csv2("simNets/diff_n.csv", sep=",", stringsAsFactors = F)[,-1]
nAlphaDyads$dyadIJ <- paste(substr(nAlphaDyads$i, 1, 8), 
                             substr(nAlphaDyads$j, 1, 8), sep = "_")
nAlphaDyads$dyadJI <- paste(substr(nAlphaDyads$i, 1, 8), 
                             substr(nAlphaDyads$j, 1, 8), sep = "_")
nAlphaDyads$dyadID <- ifelse(nAlphaDyads$dyadIJ %in% DYAD_ID_order, nAlphaDyads$dyadIJ, nAlphaDyads$dyadJI)

nAlphaDyads$nAlphaSim <- -as.numeric(nAlphaDyads$diff_n)
nAlphaDyads[,1:5] <- NULL
summary(nAlphaDyads)
DYADS <- data.frame(DYADS, nAlphaDyads[match(DYADS$dyadID, nAlphaDyads$dyadID),"nAlphaSim"])

# import similarity in alpha distributions
# alphaDyads <- read.csv2("simNets/SimilarAlphas.csv", sep=",", stringsAsFactors = F)[,-1]
# alphaDyads$dyadID <- paste(alphaDyads$i, alphaDyads$j, sep = "_")
# alphaDyads$alphaCosSim <- as.numeric(alphaDyads$cosSimCt)
# alphaDyads[,1:3] <- NULL
# summary(alphaDyads)
# DYADS <- alphaDyads
# 
# 

# import similarity in wording as explanation
wordingDyads <- read.csv2("simNets/SimilarWording.csv", sep=",", stringsAsFactors = F)[,-1]
head(wordingDyads)
wordingDyads$dyadIJ <- paste(substr(wordingDyads$i, 1, 8), 
                             substr(wordingDyads$j, 1, 8), sep = "_")
wordingDyads$dyadJI <- paste(substr(wordingDyads$i, 1, 8), 
                             substr(wordingDyads$j, 1, 8), sep = "_")
wordingDyads$dyadID <- ifelse(wordingDyads$dyadIJ %in% DYAD_ID_order, wordingDyads$dyadIJ, wordingDyads$dyadJI)

wordingDyads$wordingCosSim <- as.numeric(wordingDyads$cosSimTerm)
wordingDyads[,1:5] <- NULL
summary(wordingDyads)
DYADS <- data.frame(DYADS, wordingDyads[match(DYADS$dyadID, wordingDyads$dyadID),"wordingCosSim"])


# import similarity in citation as explanation
citationDyads <- read.csv2("simNets/SimilarCitations.csv", sep=",", stringsAsFactors = F)[,-1]
head(citationDyads)
citationDyads$dyadIJ <- paste(citationDyads$i, citationDyads$j, sep = "_")
citationDyads$dyadJI <- paste(citationDyads$j, citationDyads$i, sep = "_")
citationDyads$dyadID <- ifelse(citationDyads$dyadIJ %in% DYAD_ID_order, citationDyads$dyadIJ, citationDyads$dyadJI)
citationDyads$citationCosSim <- as.numeric(citationDyads$cosSim)
citationDyads[,1:5] <- NULL
summary(citationDyads)
DYADS <- data.frame(DYADS, citationDyads[match(DYADS$dyadID, citationDyads$dyadID),"citationCosSim"])


# import similarity in discipline as explanation
disciplineDyads <- read.csv2("simNets/SimilarDisciplinesCited.csv", sep=",", stringsAsFactors = F)[,-1]
head(disciplineDyads)
disciplineDyads$dyadIJ <- paste(disciplineDyads$i, disciplineDyads$j, sep = "_")
disciplineDyads$dyadJI <- paste(disciplineDyads$j, disciplineDyads$i, sep = "_")
disciplineDyads$dyadID <- ifelse(disciplineDyads$dyadIJ %in% DYAD_ID_order, disciplineDyads$dyadIJ, disciplineDyads$dyadJI)
disciplineDyads$disciplineCosSim <- as.numeric(disciplineDyads$cosSim)
disciplineDyads[,1:5] <- NULL
summary(disciplineDyads)
DYADS <- data.frame(DYADS, disciplineDyads[match(DYADS$dyadID, disciplineDyads$dyadID),"disciplineCosSim"])


# import similarity in countries studied as control
countryDyads <- read.csv2("simNets/SimilarCountries.csv", sep=",", stringsAsFactors = F)[,-1]
head(countryDyads)
countryDyads$dyadIJ <- paste(countryDyads$i, countryDyads$j, sep = "_")
countryDyads$dyadJI <- paste(countryDyads$j, countryDyads$i, sep = "_")
countryDyads$dyadID <- ifelse(countryDyads$dyadIJ %in% DYAD_ID_order, countryDyads$dyadIJ, countryDyads$dyadJI)
countryDyads$countryCosSim <- as.numeric(countryDyads$cosSimC)
countryDyads[,1:5] <- NULL
summary(countryDyads)
DYADS <- data.frame(DYADS, countryDyads[match(DYADS$dyadID, countryDyads$dyadID),"countryCosSim"])

# import similarity in decades studied as control
decadeDyads <- read.csv2("simNets/SimilarDecades.csv", sep=",", stringsAsFactors = F)[,-1]
head(decadeDyads)
decadeDyads$dyadIJ <- paste(decadeDyads$i, decadeDyads$j, sep = "_")
decadeDyads$dyadJI <- paste(decadeDyads$j, decadeDyads$i, sep = "_")
decadeDyads$dyadID <- ifelse(decadeDyads$dyadIJ %in% DYAD_ID_order, decadeDyads$dyadIJ, decadeDyads$dyadJI)
decadeDyads$decadeCosSim <- as.numeric(decadeDyads$cosSimCt)
decadeDyads[,1:5] <- NULL
summary(decadeDyads)
DYADS <- data.frame(DYADS, decadeDyads[match(DYADS$dyadID, decadeDyads$dyadID),"decadeCosSim"])

# import similarity in city definition used as control
cityDefDyads <- read.csv2("simNets/SimilarCities.csv", sep=",", stringsAsFactors = F)[,-1]
head(cityDefDyads)
cityDefDyads$dyadIJ <- paste(cityDefDyads$i, cityDefDyads$j, sep = "_")
cityDefDyads$dyadJI <- paste(cityDefDyads$j, cityDefDyads$i, sep = "_")
cityDefDyads$dyadID <- ifelse(cityDefDyads$dyadIJ %in% DYAD_ID_order, cityDefDyads$dyadIJ, cityDefDyads$dyadJI)
cityDefDyads$cityDefCosSim <- as.numeric(cityDefDyads$cosSimCt)
cityDefDyads[,1:5] <- NULL
summary(cityDefDyads)
DYADS <- data.frame(DYADS, cityDefDyads[match(DYADS$dyadID, cityDefDyads$dyadID),"cityDefCosSim"])

head(DYADS)
colnames(DYADS) <- c("dyadID","meanAlphaSim", "sdAlphaSim", "nAlphaSim", 
                     "fullTextCosSim", "externalCitationCosSim", "disciplineCitationCosSim",
                     "countriesCosSim", "decadesCosSim", "cityDefCosSim")

DYADS$meanAlphaSim_scaled <- scale(DYADS$meanAlphaSim)
DYADS$sdAlphaSim_scaled <- scale(DYADS$sdAlphaSim)
DYADS$nAlphaSim_scaled <- scale(DYADS$nAlphaSim)
DYADS$fullTextCosSim_scaled <- scale(DYADS$fullTextCosSim)
DYADS$externalCitationCosSim_scaled <- scale(DYADS$externalCitationCosSim)
DYADS$disciplineCitationCosSim_scaled <- scale(DYADS$disciplineCitationCosSim)
DYADS$countriesCosSim_scaled <- scale(DYADS$countriesCosSim)
DYADS$decadesCosSim_scaled <- scale(DYADS$decadesCosSim)
DYADS$cityDefCosSim_scaled <- scale(DYADS$cityDefCosSim)


DYADSwithNA <- DYADS
DYADS <- DYADS[complete.cases(DYADS),]
dim(DYADSwithNA)
dim(DYADS)


modeldata<- DYADS[,c("dyadID","meanAlphaSim_scaled", "sdAlphaSim_scaled", "nAlphaSim_scaled",
                     "fullTextCosSim_scaled", "externalCitationCosSim_scaled", "disciplineCitationCosSim_scaled",
                     "countriesCosSim_scaled", "decadesCosSim_scaled", "cityDefCosSim_scaled")]
colnames(modeldata) <- c("ID", "alpha", "sd_alpha", "n_estim","text", "citation", "discipline", "country", "decade", "city")


# modelling
ftm_a <- lm(data = modeldata,
            formula = alpha ~ text,
            na.action = na.omit)
summary(ftm_a)
ecm_a <- lm(data = modeldata,
            formula = alpha ~ citation,
            na.action = na.omit)
summary(ecm_a)
dcm_a <- lm(data = modeldata,
            formula = alpha ~ discipline,
            na.action = na.omit)
summary(dcm_a)

# n_a <- lm(data = modeldata,
#             formula = alpha ~ n_estim,
#             na.action = na.omit)
# summary(n_a)

ctrm_a <- lm(data = modeldata,
            formula = alpha ~ n_estim +
             country * decade * city,
            na.action = na.omit)
summary(ctrm_a)

fullmodel_a <- lm(data = modeldata,
            formula = alpha ~ 
              text + citation + discipline + n_estim +
              country * decade * city,
            na.action = na.omit)
summary(fullmodel_a)


sgm<- stargazer(ftm_a, ecm_a, dcm_a,
          ctrm_a,
          fullmodel_a,
          type="text", out="model_comp_results_meanAlpha.html")


##### represent graph of residuals
modeldata$res <- fullmodel_a$residuals
head(modeldata[order(-modeldata$res),])
modeldata$i <- substr(modeldata$ID, 1,8)
modeldata$j <- substr(modeldata$ID, 10, 17)
summary(modeldata$res)

cs.residuals_resPos <- modeldata[modeldata$res > 1.1,c("i", "j", "res")]
g.residuals <- graph_from_data_frame(cs.residuals_resPos, directed=F)
resPos <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.residuals)$name,],1, FUN = norm_vec))
colnames(resPos) <- "residuals"
resPos$ref <- rownames(resPos)
orderedName <- data.frame(V(g.residuals)$name)
orderedresidualsRes <- data.frame(orderedName, resPos[match(orderedName$V.g.residuals..name, resPos$ref),])[,"residuals"]
orderedresidualsRes <- orderedresidualsRes[!is.na(orderedresidualsRes)]

#clln.residuals <- cluster_louvain(g.residuals)
layout <- layout_nicely(g.residuals,2)
g.residuals$layout <- layout

plot(g.residuals, edge.width = sqrt(cs.residuals_resPos$res) * 2, vertex.size = 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = "green", edge.color = "coral3")



cs.residuals_resNeg <- modeldata[modeldata$res < -2,c("i", "j", "res")]
g.residuals <- graph_from_data_frame(cs.residuals_resNeg, directed=F)
resNeg <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.residuals)$name,],1, FUN = norm_vec))
colnames(resNeg) <- "residuals"
resNeg$ref <- rownames(resNeg)
orderedName <- data.frame(V(g.residuals)$name)
orderedresidualsRes <- data.frame(orderedName, resNeg[match(orderedName$V.g.residuals..name, resNeg$ref),])[,"residuals"]
orderedresidualsRes <- orderedresidualsRes[!is.na(orderedresidualsRes)]

#clln.residuals <- cluster_louvain(g.residuals)
layout <- layout_nicely(g.residuals,2)
g.residuals$layout <- layout

plot(g.residuals, edge.width = sqrt(cs.residuals_resNeg$res) * 2, vertex.size = 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = "orange", edge.color = "dodgerblue3")





#### models of sd alpha

ftm_sd <- lm(data = modeldata,
            formula = sd_alpha ~ text,
            na.action = na.omit)
summary(ftm_sd)
ecm_sd <- lm(data = modeldata,
            formula = sd_alpha ~ citation,
            na.action = na.omit)
summary(ecm_sd)
dcm_sd <- lm(data = modeldata,
            formula = sd_alpha ~ discipline,
            na.action = na.omit)
summary(dcm_sd)

 n_sd <- lm(data = modeldata,
             formula = sd_alpha ~ n_estim,
             na.action = na.omit)
 summary(n_sd)

ctrm_sd <- lm(data = modeldata,
             formula = sd_alpha ~ 
               country * decade * city,
             na.action = na.omit)
summary(ctrm_sd)

fullmodel_sd <- lm(data = modeldata,
                  formula = sd_alpha ~ 
                    text + citation + discipline + n_estim +
                    country * decade * city,
                  na.action = na.omit)
summary(fullmodel_sd)


sgm<- stargazer(ftm_sd, ecm_sd, dcm_sd,n_sd,
                ctrm_sd,
                fullmodel_sd,
                type="text", out="model_comp_results_sdAlpha.html")



##### represent graph of residuals
modeldata$res <- fullmodel_sd$residuals
head(modeldata[order(-modeldata$res),])
modeldata$i <- substr(modeldata$ID, 1,8)
modeldata$j <- substr(modeldata$ID, 10, 17)
summary(modeldata$res)

cs.residuals_resPos <- modeldata[modeldata$res > 1,c("i", "j", "res")]
g.residuals <- graph_from_data_frame(cs.residuals_resPos, directed=F)
resPos <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.residuals)$name,],1, FUN = norm_vec))
colnames(resPos) <- "residuals"
resPos$ref <- rownames(resPos)
orderedName <- data.frame(V(g.residuals)$name)
orderedresidualsRes <- data.frame(orderedName, resPos[match(orderedName$V.g.residuals..name, resPos$ref),])[,"residuals"]
orderedresidualsRes <- orderedresidualsRes[!is.na(orderedresidualsRes)]

#clln.residuals <- cluster_louvain(g.residuals)
layout <- layout_nicely(g.residuals,2)
g.residuals$layout <- layout

plot(g.residuals, edge.width = sqrt(cs.residuals_resPos$res) * 2, vertex.size = 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = "green", edge.color = "coral3")



cs.residuals_resNeg <- modeldata[modeldata$res < -1.5,c("i", "j", "res")]
g.residuals <- graph_from_data_frame(cs.residuals_resNeg, directed=F)
resNeg <- as.data.frame(apply(alphamat[rownames(alphamat) %in% V(g.residuals)$name,],1, FUN = norm_vec))
colnames(resNeg) <- "residuals"
resNeg$ref <- rownames(resNeg)
orderedName <- data.frame(V(g.residuals)$name)
orderedresidualsRes <- data.frame(orderedName, resNeg[match(orderedName$V.g.residuals..name, resNeg$ref),])[,"residuals"]
orderedresidualsRes <- orderedresidualsRes[!is.na(orderedresidualsRes)]

#clln.residuals <- cluster_louvain(g.residuals)
layout <- layout_nicely(g.residuals,2)
g.residuals$layout <- layout

plot(g.residuals, edge.width = sqrt(cs.residuals_resNeg$res) * 2, vertex.size = 2,
     vertex.label.cex = 0.7,  edge.curved=.2, vertex.color = "orange", edge.color = "dodgerblue3")


glossary <- refs[refs$REFID %in% paperList,c("REFID", "AUTHOR", "YEAR", "JOURNAL")]
write.csv2(glossary,"results/glossary.xls")
