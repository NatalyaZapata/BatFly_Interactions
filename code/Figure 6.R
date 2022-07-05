################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of worldwide bat-fly interactions.
#### Figure 1. Distribution of sampling locations included in BatFly.

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly-A-dataset-of-worldwide-bat-fly-interactions/blob/main/README.md
################################################################################




######################### 1. SETTINGS ##########################################

## Clean the environment
rm(list= ls())

## Load the packages
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}

## Import the data sets
data1<-read.csv("data/BatFly_Species.csv", sep=",")
data2<-read.csv("data/BatFly_Bat_Pop.csv", sep=",")
data3<-read.csv("data/BatFly_Fly_Pop.csv", sep=",")


## Determine bat species on each roost type
dfa <- str_split_fixed(data2$BatRoost, ' ', 5)

vdfa<-c(dfa[,1:NCOL(dfa)])
un.roost<-unique(vdfa)[-7]#save unique and delete empty

droost<-cbind.data.frame(data2$CurrentBatSpecies, dfa)
droost<-unique(droost)
nrow(droost)

batroostrich<-rep(NA,length(un.roost))
for (i in 1: length(un.roost)){
  batroostrich[i]<-sum(droost==un.roost[i])
}
batroostrich

roostbat<-(data.frame(un.roost,batroostrich))

roostbat<-roostbat[order(roostbat$batroostrich),]


## Determine fly species on each roost type

interaction<-data.frame(bat=data1$CurrentBatSpecies,fly=data1$CurrentFlySpecies)
unq.inter<-(unique(interaction))
nrow(unq.inter)

rero<-data.frame(cave=rep(0,nrow(unq.inter)),`tree cavity`=rep(0,nrow(unq.inter)), foliage=rep(0,nrow(unq.inter)),
                 human=rep(0,nrow(unq.inter)),termite=rep(0,nrow(unq.inter)), tent=rep(0,nrow(unq.inter)), cut=rep(0,nrow(unq.inter)))
colnames(rero)

## Classify fly species based on their host's roost
for (i in 1:nrow(unq.inter)){
  d<-which(unq.inter[i,1]==droost$`data2$CurrentBatSpecies`)
  if (sum(droost[d,-1]=="Cave")>0){
    rero$cave[i]<-1  
  }
  if (sum(droost[d,-1]=="Treecavity")>0){
    rero$tree.cavity[i]<-1  
  }
  if (sum(droost[d,-1]=="Foliage")>0){
    rero$foliage[i]<-1  
  }
  if (sum(droost[d,-1]=="Human-madestructure")>0){
    rero$human[i]<-1  
  }
  if (sum(droost[d,-1]=="Termitenest")>0){
    rero$termite[i]<-1  
  }
  if (sum(droost[d,-1]=="Tent")>0){
    rero$tent[i]<-1  
  }
  if (sum(droost[d,-1]=="Cutbank")>0){
    rero$cut[i]<-1  
  }
  
}

flyroost<-(cbind(unq.inter$fly,rero))


flyroost<-ddply(flyroost, "unq.inter$fly", numcolwise(sum))
flynames<-flyroost$`unq.inter$fly`
flyroost[flyroost > 0] <- 1
flyroost$`unq.inter$fly`<-flynames

colSums(flyroost[2:8])

######################### 2. PLOTTING ######################################

png("figures/Figure_6.png", res = 300,
    width = 4000, height = 2000, unit = "px")

par(las=1, mar=c(5, 10, 4, 1))
layout(matrix(c(1,2), ncol=2))


barplot(roostbat$batroostrich,
        names.arg=c("Cut bank", "Termite nest", "Tent", "Foliage",
                    "Human-made structure", "Tree cavity", "Cave"), 
        xlab="Bat richness", horiz=T, col="#7a5195",main="A",
        xlim=c(0,140))

barplot(sort(colSums(flyroost[2:8])), 
        names.arg=c("Cut bank", "Termite nest", "Tent", "Foliage",
                    "Human-made structure", "Tree cavity", "Cave"), 
        xlab="Fly richness", horiz=T, col="#96d0ab", main="B",
        xlim=c(0,250))

dev.off()
