################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 6. Richness of bat flies (Nycteribiidae and Streblidae) 
####           per bat family

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly_Interactions#readme
################################################################################


######################### 1. SETTINGS ##########################################

## Clean the environment
rm(list= ls())


## Check the required packages, install them if necessary, and load them.


if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
## Import the data
data1<-read.csv("data/BatFly_Species.csv", sep=",")
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

which(table(flyfam[,2])>1)

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
plotdata<-cbind(Nycteribiidae=(plotdata[,1]+plotdata[,2]), 
                Streblidae= plotdata[,3]) 
class(plotdata)
str(plotdata)
head(plotdata)
tail(plotdata)


######################### 2. PLOTTING ######################################


png("figures/Figure_6.png", res = 300,
    width = 2100, height = 2000, unit = "px")
par(las=1, mar=c(4, 8, 1, 2))

bar<-barplot(t(100*plotdata/sum(plotdata)), horiz=T, xlim=c(0,60), xlab="Relative parasite richness (%)", col=c("#E5EFC1", "#39AEA9"))

legend(x=0.3*100, y=7, legend=colnames(plotdata), pch=19, pt.cex=1.5,
       col=c("#E5EFC1", "#39AEA9"),bty = "n", x.intersp=0.5, y.intersp=0.9)

text(y=bar, x=colSums(t(100*plotdata/sum(plotdata)))+0.02*100, 
     (plotdata[,1]+plotdata[,2]),cex=0.9)

dev.off()
