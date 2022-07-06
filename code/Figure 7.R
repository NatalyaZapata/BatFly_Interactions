################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of worldwide bat-fly interactions.
#### Figure 7. Interaction accumulation curve by sampling site. 
####           Gray polygon represents the 95% confidence interval of the curve.

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly_Interactions#readme
################################################################################


######################### 1. SETTINGS ##########################################


## Clean the environment
rm(list= ls())


## Check the folders
if (!dir.exists(path = "code")){
  dir.create(path = "code")
} else {
  print("Dir already exists!")
}

if (!dir.exists(path = "data")){
  dir.create(path = "data")
} else {
  print("Dir already exists!")
}

if (!dir.exists(path = "figures")){
  dir.create(path = "figures")
} else {
  print("Dir already exists!")
}


## Load the packages

if(!require(vegan)){
  install.packages("vegan")
  library(vegan)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}


## Import the data
data1<-read.csv("data/Batfly_Species.csv", sep=",")

interactions<-paste(data1$CurrentBatSpecies,data1$CurrentFlySpecies)
studyinter<-table(data1$SiteCode, interactions)
length(unique(interactions))

interactionpersite<-spread(as.data.frame(studyinter), interactions, Freq, fill = 0)

## Check the data
class(interactionpersite)
str(interactionpersite)
head(interactionpersite)
tail(interactionpersite)


######################### 2. PLOTTING ######################################


png("figures/Figure_7.png", res = 300,
    width = 2100, height = 2000, unit = "px")

par(las=1, mar=c(5, 5, 4, 2))
obs.curve<-specaccum(interactionpersite[,-1])
plot(obs.curve, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="gray80",
     xlab="Sampling sites", ylab="Number of unique interactions")

dev.off()

