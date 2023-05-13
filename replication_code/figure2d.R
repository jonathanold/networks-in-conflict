# install.packages("igraph")
library(igraph)
library(foreign)

mx = as.matrix(read.dta('/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/graph.dta'))
nw <- graph.edgelist(mx , directed=F)
kamada_layout <- layout.kamada.kawai(nw)
fr <- layout.fruchterman.reingold(nw)
auto <- layout.auto(nw)
star = layout_components(nw)

V(nw)$size <- sqrt(degree(nw))
#V(nw)$name = seq(1:length(nw)) # unique(mx[,1])
V(nw)$vcolor <- rgb(12/255, 70/255, 90/255, alpha = sqrt(degree(nw) / max(degree(nw))))

V(nw)$degree2 = min(degree(nw)^(1/3),3)
V(nw)$size2 = 0.6*sqrt(degree(nw)/max(degree(nw)))*2


pdf(file = "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/replication_outputs/figures/fig2.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 14) # The h

plot(nw, 
     layout = layout.auto(nw), 
     vertex.color = V(nw)$vcolor,
     vertex.frame.color = V(nw)$vcolor,
     vertex.label.cex = V(nw)$size2,
     #vertex.label = V(nw)$name,
     vertex.label.color = "black",
     edge.curved = 0.01,
     edge.width = 1.0,
     ylim=c(-0.6,0.6),
     xlim=c(-1,1),
     asp = 0)

dev.off()
