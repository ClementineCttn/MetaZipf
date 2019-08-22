setwd("~/GitHub/MetaZipf")
library(igraph)

cites <- read.csv2("data/zipf_cites.csv", sep=',')
head(cites)

cites[is.na(cites)] <- 0
dim(cites)

cites_within <- cites[1:66,c(1,5:70)]
tail(cites_within)
rownames(cites_within) <- cites_within$REFID
cites_within$REFID <- NULL
cmat <- t(as.matrix(cites_within))
dim(cmat)

node.size <- colSums(cmat)


cite_year <- cites[1:66,c('REFID', 'YEAR')]
rownames(cite_year) <- cite_year$REFID
cite_year$REFID <- NULL
node.year <- cite_year$YEAR - 1974

g <- graph_from_adjacency_matrix(cmat, mode = "directed", diag = F)

layout <- layout_nicely(g,2)
g$layout <- layout
fine = 500 # this will adjust the resolving power.
pal = colorRampPalette(c('black','white'))
graphCol = pal(fine)[as.numeric(cut(node.year,breaks = fine))]

plot(g, vertex.size=node.size*0.5,
     vertex.label.dist=0.5, 
     vertex.color=graphCol, edge.arrow.size=0.2,
     vertex.label.cex = 0.5)
