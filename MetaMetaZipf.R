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

cites <- read.csv2("data/zipf_cites.csv", sep=';')
head(cites)

cites[is.na(cites)] <- 0
dim(cites)

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
q <- ggplot(cope, aes(x=ref, y = cop))
q + geom_lollipop(aes(reorder(ref, -cop)),color = "seagreen3", cex=1) +
  coord_flip() +
  labs(x="Reference", y="Number of in-citations from the sample")
  
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
  labs(x="Journal", y="Number of articles in the sample")


 samplesummary <- data.frame(j, cope[match(j$REFID, cope$ref),])
colnames(samplesummary)[8] <- "in_citations"
mean(samplesummary$YEAR)
mean(samplesummary[samplesummary$in_citations > 10, "YEAR"])


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
cites_out$AUTHOR <- gsub(".*\\d.", "", cites_out$REFID)
cites_out$JOURNAL <- gsub("[[:punct:]]", " ", cites_out$JOURNAL)
cites_out$AUTHOR <- gsub("[[:punct:]]", " ", cites_out$AUTHOR)

head(cites_out)
summary(as.factor(cites_out$JOURNAL))/dim(cites_out)[1]
# Diverse pool of citations. MAx % of journals cited = 2.5% (Journal Regional Science) and 2.3 (Urban Studies)

jou <- as.data.frame(summary(as.factor(cites_out$JOURNAL)))
jou$ref = rownames(jou)
colnames(jou)[1] <- "n"
dim(jou)
jou <- subset(jou[-100,], n>3)
q <- ggplot(jou, aes(x=ref, y = n))
q + geom_lollipop(aes(reorder(ref, -n)),color = "goldenrod3", cex=1) +
  coord_flip() +
  labs(x="Journal", y="Number of citations from the sample (>3)")

#######

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

########### Network of inter citations
g <- graph_from_incidence_matrix(cmat, directed = T)

layout <- layout_nicely(g,2)
g$layout <- layout
fine = 500 # this will adjust the resolving power.

par(mar = c(0.5, 0.3, 0.5, 0.5))

plot(g, vertex.size=node.size*0.5,
     vertex.label.dist=0.5, 
     vertex.color= alpha("grey",0.6), edge.color = "black", 
     vertex.label.cex = 0.7,  vertex.label.color = alpha("grey",0.6),
     vertex.label.font = 2,  
     edge.arrow.size=0.2)
knljnmnmj
