################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 5. IUCN conservation status of the bat species

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

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

## Import data from the IUCN Red List of Threated Species
## (https://www.iucnredlist.org)
batIUCN<-read.csv("data/batIUCN.csv", sep=",")

## Check the data
class(batIUCN)
str(batIUCN)
head(batIUCN)
tail(batIUCN)


## Import the data
data<-read.csv("data/BatFly_Bat_Pop.csv", sep=",")

bats<-unique(data$CurrentBatSpecies)
bats<-bats[-which(str_detect(bats, " sp\\.| aff\\.| cf\\."))]


cat<-NULL
for (i in 1:length(bats)){
  cat[[i]]<-ifelse(identical(batIUCN$redlistCategory[which(batIUCN$scientificName==bats[i])], character(0))
                   , NA,batIUCN$redlistCategory[which(batIUCN$scientificName==bats[i])]) 
}
cat<-unlist(cat)
batcat<-cbind(bats,cat)


## Check the data
class(batcat)
str(batcat)
head(batcat)
tail(batcat)


######################### 2. PLOTTING ######################################


png("figures/Figure_4.png", res = 300,
    width = 2500, height = 1500, unit = "px")

par(las=1, mar=c(5, 4, 4, 3))

bar<-barplot(sort(table(batcat[,2]), decreasing = T)/sum(table(batcat[,2]))*100,
        horiz=F, ylab="Bat species (%)",
        col="#7a5195", ylim=c(0,100))

text(x=bar, y=sort(table(batcat[,2]), decreasing = T)/sum(table(batcat[,2]))*100
     +3, sort(table(batcat[,2]), decreasing = T),cex=0.9)

dev.off()


