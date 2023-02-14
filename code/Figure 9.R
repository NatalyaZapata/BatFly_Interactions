################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 9. Interaction accumulation curve by sampling site. 
####           Gray polygon represents the 95% confidence interval of the curve.

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly_Interactions#readme
################################################################################


######################### 1. SETTINGS ##########################################


## Clean the environment
rm(list= ls())

## Check the required packages, install them if necessary, and load them.

if(!require(vegan)){
  install.packages("vegan")
  library(vegan)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}


## Import the data
data1<-read.csv("data/Batfly_Species.csv", sep=",")
##removing unidentified species
data1<-data1[ -sort(c(which(str_detect(data1$CurrentBatSpecies, " sp\\.| cf\\.| aff\\.")), 
               which(str_detect(data1$CurrentFlySpecies, " sp\\.| cf\\.| aff\\.| complex|Streblidae| group|Morphospecies")))),]


interactions<-paste(data1$CurrentBatSpecies,data1$CurrentFlySpecies)
studyinter<-table(data1$SiteCode, interactions)
length(unique(interactions))


table(data1$SiteCode, interactions)

interactionpersite<-spread(as.data.frame(studyinter), interactions, Freq, fill = 0)


## Check the data
class(interactionpersite)
str(interactionpersite)
head(interactionpersite)
tail(interactionpersite)

#Estimating completeness via non-parametric Chao2
esti<-specpool(interactionpersite[,-1])
round(esti$Species/esti$chao*100, digits=2)

######################### 2. PLOTTING ######################################
estimator<-poolaccum(interactionpersite[,-1], permutations=1000 )
obs.curve<-summary(estimator, display = "S")
chao.curve<-summary(estimator, display = "chao")


png("figures/Figure_9.png", res = 300,
    width = 2100, height = 2000, unit = "px")

layout(matrix(c(2,1)),heights=c(10,90))
layout.show(2)
par(las=1, mar=c(4, 4, 1, 1))

plot(x=NULL,
     y=NULL, 
     xlim=c(0,max(obs.curve$S[,1])), 
     ylim=c(0,max(round(chao.curve$chao[,2], digits=0))+19),
     xlab="Number of sampling sites", 
     ylab="Number of unique interactions",
     yaxp=c(0,max(round(chao.curve$chao[,2], digits=0))+19,5))

cilim<-seq(1,length(obs.curve$S[,1]), length.out=11)

polygon(x=c(rev(obs.curve$S[,1][cilim]),obs.curve$S[,1][cilim]),
        y=c(rev(obs.curve$S[,3][cilim]),obs.curve$S[,4][cilim]),
        col="gray80", border=NA)

lines(x=obs.curve$S[,1], y=obs.curve$S[,2], lwd=2)



polygon(x=c(rev(obs.curve$S[,1][cilim]),obs.curve$S[,1][cilim]),
        y=c(rev(chao.curve$chao[,3][cilim]),chao.curve$chao[,4][cilim]),
        col="gray80", border=NA)

lines(x=obs.curve$S[,1], y=chao.curve$chao[,2], col="black", lty=2, lwd=2)

par(las=1, mar=c(0, 0, 0, 0))
plot(x=NULL,
     y=NULL, 
     xlim=c(0,1), 
     ylim=c(0,1),
     xaxt="n", yaxt="n", bty="n")

legend(x=0.15, y=0.5, 
       legend=c("Sample-based rarefaction curve", "Chao2 estimator"), 
       lty=c(1,2), lwd=2, bty="n", ncol=2)

dev.off()


