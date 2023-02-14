################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 3. Bat-Fly Neotropical network. Lines depict interactions between bats 
####and bat flies, most species are part of a giant component.

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

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(bipartite)){
  install.packages("bipartite")
  library(bipartite)
}

## Load the interaction file

bf_int<-read.csv("data/BatFly_Species.csv")


## Prepare the data
#removing unidentified species
interactions<-as.data.frame(cbind(bats=bf_int$CurrentBatSpecies, flies=bf_int$CurrentFlySpecies))
interactions<-unique(interactions)

interactions[which(str_detect(interactions$bats, " sp\\.| aff\\.| cf\\.")), ]
interactions[which(str_detect(interactions$flies, " sp\\.| aff\\.| cf\\.| complex|Streblidae| group|Morphospecies")), ]

interactions<-interactions[
  -sort(c(which(str_detect(interactions$bats, " sp\\.| aff\\.| cf\\.")), 
          which(str_detect(interactions$flies, " sp\\.| aff\\.| cf\\.| complex|Streblidae| group|Morphospecies")))),]

#building the network
imat<-graph_from_data_frame(interactions, directed=FALSE)

V(imat)$type<-bipartite.mapping(imat)$type
imat

#customizing the network

V(imat)$color = V(imat)$type
V(imat)$color = c(alpha("#7a5195",0.7), alpha("#96d0ab",0.7))[V(imat)$type+1]
V(imat)$size<-V(imat)$type
V(imat)$size[which(V(imat)$size==FALSE)]<-rep(3,length(which(V(imat)$size==FALSE)))#bats
V(imat)$size[which(V(imat)$size==TRUE)]<-rep(2,length(which(V(imat)$size==TRUE)))#flies
V(imat)$shape <- c("circle", "square")[V(imat)$type+1]

E(imat)$color<-"gray"

algoritmo<-layout.auto(imat)


######################### 2. PLOTTING ######################################

##export without names

png(filename="figures/Figure_3.png", width=4000, height=4100, res=600)
par(las=1,mar=c(0,0,0,0))
layout(matrix(c(2,1), ncol=1), heights=c(0.04,0.96))
plot(imat,vertex.label.cex=0.2,vertex.color = V(imat)$color, 
     vertex.size = V(imat)$size, edge.curved=.3,layout = algoritmo, rescale=T, 
     vertex.label=NA)


plot(x=NULL, y=NULL, ann=F,xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1), type="n", bty="n")
legend(x=0.8, y=1, legend=c("Bats", "Flies"),pch=c(21,22), pt.cex=1.2,
       pt.bg=c(alpha("#7a5195",0.7), alpha("#96d0ab",0.7)),ncol=2, bty="n",
       x.intersp=0.5)
dev.off()
#-----------------------------
#Optional
#with names
png(filename="BFnetnames.png", width=4000, height=4100, res=600)
par(las=1,mar=c(0,0,0,0))
layout(matrix(c(2,1), ncol=1), heights=c(0.04,0.96))
plot(imat,vertex.label.cex=0.2,vertex.color = V(imat)$color, 
     vertex.size = V(imat)$size, edge.curved=.3,layout = algoritmo, rescale=T, 
     vertex.label.color="darkred")


plot(x=NULL, y=NULL, ann=F,xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1), type="n", bty="n")
legend(x=0.8, y=1, legend=c("Bats", "Flies"),pch=c(21,22), pt.cex=1.2,
       pt.bg=c(alpha("#7a5195",0.7), alpha("#96d0ab",0.7)),ncol=2, bty="n",
       x.intersp=0.5)
dev.off()