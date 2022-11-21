################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 8. 

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly_Interactions#readme
################################################################################


######################### 1. SETTINGS ##########################################

## Clean the environment.
rm(list= ls())

## Check the required packages, install them if necessary, and load them.

if(!require(scales)){
  install.packages("scales")
  library(scales)
}

if(!require(igraph)){
  install.packages("igraph")
  library(igraph)
}


## Load the interaction file

bf_int<-read.csv("BAT-FLY_INTERACTIONS_Species.csv")


## Prepare the data
interactions<-as.data.frame(cbind(bats=bf_int$CurrentBatSpecies, flies=bf_int$CurrentFlySpecies, edges=rep(1, length(bf_int$CurrentFlySpecies))))
interactions<-unique(interactions)

imat<-graph_from_data_frame(interactions, directed=FALSE)


V(imat)$type<-bipartite.mapping(imat)$type
imat



V(imat)$color = V(imat)$type
V(imat)$color = c(alpha("#7a5195",0.7), alpha("#96d0ab",0.7))[V(imat)$type+1]
V(imat)$size<-V(imat)$type
V(imat)$size[which(V(imat)$size==FALSE)]<-rep(3,length(which(V(imat)$size==FALSE)))#bats
V(imat)$size[which(V(imat)$size==TRUE)]<-rep(2,length(which(V(imat)$size==TRUE)))#flies
V(imat)$shape <- c("circle", "square")[V(imat)$type+1]

E(imat)$color<-"gray"

algoritmo<-layout.auto(imat)
## Plot and export

png(filename="BFnet.png", width=4000, height=4100, res=600)
par(las=1,mar=c(0,0,0,0))
layout(matrix(c(2,1), ncol=1), heights=c(0.04,0.96))
plot(imat,vertex.label.cex=1,vertex.color = V(imat)$color, 
     vertex.size = V(imat)$size, edge.curved=.3,layout = algoritmo, rescale=T, 
     vertex.label.color="black",vertex.label=NA)


plot(x=NULL, y=NULL, ann=F,xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1), type="n", bty="n")
legend(x=0.8, y=1, legend=c("Bats", "Flies"),pch=c(21,22), pt.cex=1.2,
       pt.bg=c(alpha("#7a5195",0.7), alpha("#96d0ab",0.7)),ncol=2, bty="n",
       x.intersp=0.5)
dev.off()
