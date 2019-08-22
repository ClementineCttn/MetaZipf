setwd("~/GitHub/MetaZipf")
library(igraph)
library(scales)
library(wesanderson)
library(ggplot2)
library(ggrepel)
library(ggalt)

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

  