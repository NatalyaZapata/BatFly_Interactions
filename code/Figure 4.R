#Figure 4. IUCN conservation status of the bat species

#Import data IUCN Red List of Threated Species (https://www.iucnredlist.org)
batIUCN<-read.csv("batIUCN.csv", sep=",")
head(batIUCN)

#Import data set
data2<-read.csv("BatFly_Bat_Pop.csv", sep=",")
bats<-unique(data2$CurrentBatSpecies)
cat<-NULL
for (i in 1:length(bats)){
  cat[[i]]<-ifelse(identical(batIUCN$redlistCategory[which(batIUCN$scientificName==bats[i])], character(0))
                   , NA,batIUCN$redlistCategory[which(batIUCN$scientificName==bats[i])]) 
}
cat<-unlist(cat)
batcat<-cbind(bats,cat)

#Plot

png("Figure_4.png", res = 300,
    width = 2500, height = 1500, unit = "px")

par(las=1, mar=c(5, 4, 4, 3))

barplot(sort(table(batcat[,2]), decreasing = T)/sum(table(batcat[,2])),
        horiz=F, ylab="Bat species (%)",
        col="#7a5195", ylim=c(0,1))

dev.off()
