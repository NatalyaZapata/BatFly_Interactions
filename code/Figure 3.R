################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 3. Overview of bat-fly interactions included in BatFly. 
####           (a) Relative richness of fly families (Nycteribiidae and Streblidae) per bat family. 
####           (b) Bat-fly network built for the entire Neotropical region with all records included in 
####           our data set, even uncommon associations.  

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

## Import the data FIGURE 3 (a) Relative richness of fly families (Nycteribiidae and Streblidae) per bat family.
data1<-read.csv("data/BatFly_Species_Interactions.csv", sep=",")
data2<-read.csv("data/BatFly_Bat_Pop.csv", sep=",")
data3<-read.csv("data/BatFly_Fly_Pop.csv", sep=",")

## Check the data
class(data1)
str(data1)
head(data1)
tail(data1)

class(data2)
str(data2)
head(data2)
tail(data2)

class(data3)
str(data3)
head(data3)
tail(data3)

#removing unidentified species

data1[which(str_detect(data1$CurrentBatSpecies, " sp\\.| aff\\.| cf\\.")), ]
data1[which(str_detect(data1$CurrentFlySpecies, " sp\\.| aff\\.| cf\\.| complex|Streblidae| group|Morphospecies")), ]

data1<-data1[
  -sort(c(which(str_detect(data1$CurrentBatSpecies, " sp\\.| aff\\.| cf\\.")), 
          which(str_detect(data1$CurrentFlySpecies, " sp\\.| aff\\.| cf\\.| complex|Streblidae| group|Morphospecies")))),]

## Organize information by families
fam<-unique(cbind(data2$BatFamily, data2$CurrentBatSpecies))
flyfam<-unique(cbind(data3$FlyFamily, data3$CurrentFlySpecies))

which(table(flyfam[,2])>1)#check

family<-NULL
for (i in 1:length(data1$CurrentBatSpecies)){#passing families to bat species in data2 
  
  family[[i]]<-fam[which(fam[,2]==data1$CurrentBatSpecies[i]),1]
}

family<-unlist(family)
length(family)
class(family)
str(family)
head(family)
tail(family)


flyfamily<-NULL
for (i in 1:length(data1$CurrentFlySpecies)){#adding families to fly species in data3
  
  flyfamily[[i]]<-flyfam[which(flyfam[,2]==data1$CurrentFlySpecies[i]),1]
}

flyfamily<-unlist(flyfamily)
length(flyfamily)
class(flyfamily)
str(flyfamily)
head(flyfamily)
tail(flyfamily)


plotdata<-unique(cbind(data1$CurrentFlySpecies,family,flyfamily))
#the number of times a family name is repeated indicates parasite richness
plotdata<-as.matrix(table(plotdata[,2],plotdata[,3]))
plotdata<-plotdata[order(rowSums(plotdata)),]
plotdata<-cbind(Nycteribiidae=(plotdata[,1]), 
                Streblidae= plotdata[,2]) 
class(plotdata)
str(plotdata)
head(plotdata)
tail(plotdata)


## Load the interaction file FIGURE 3 (b) Bat-fly network built for the entire Neotropical region with all records included in 
#### our data set, even uncommon associations.

bf_int<-read.csv("data/BatFly_Species_Interactions.csv")


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

png(filename="figures/Figure_3.png", width=4900, height=4000, res=600)
layout(matrix(c(1,2,3,4), ncol=2, byrow = T), heights=c(10,90)) 
layout.show(4)

par( mar=c(0, 0, 0, 0)) #Legends barplot figure 3A
plot(x=NULL, y=NULL, ann=F,xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1), type="n", bty="n")
legend(x=0.4, y=0.4, legend=c("Nycteribiidae", "Streblidae"),pch=c(22), pt.cex=1.2,
       pt.bg=c(alpha("#E5EFC1",0.7), alpha("#39AEA9",0.7)),ncol=2, bty="n",
       x.intersp=0.5)
text(x=0.1, y=0.4,expression(bold("(a)")),cex=1.8)

plot(x=NULL, y=NULL, ann=F,xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1), type="n", bty="n") #Legends network figure 3B
legend(x=0.39, y=0.4, legend=c("Bats", "Flies"),pch=c(21,22), pt.cex=1.2,
       pt.bg=c(alpha("#7a5195",0.7), alpha("#96d0ab",0.7)),ncol=2, bty="n",
       x.intersp=0.5)
text(x=0.1, y=0.4,expression(bold("(b)")),cex=1.8)

par(las=1, mar=c(15, 7.5, 1, 0)) #Barplot Figure 3A
bar<-barplot(t(100*plotdata/sum(plotdata)), horiz=T, xlim=c(0,60), xlab="Relative parasite richness (%)", col=c("#E5EFC1", "#39AEA9"))
text(y=bar, x=colSums(t(100*plotdata/sum(plotdata)))+0.02*100, 
     (plotdata[,1]+plotdata[,2]),cex=0.9)

par( mar=c(10, 1, 0, 1)) #Network Figure 3B
plot(imat,vertex.label.cex=0.2,vertex.color = V(imat)$color, 
     vertex.size = V(imat)$size, edge.curved=.3,layout = algoritmo, rescale=T, 
     vertex.label=NA)



dev.off()













#-----------------------------
#Optional
#with export networks with species names
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