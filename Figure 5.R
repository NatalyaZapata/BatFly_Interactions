#Figure 5. Richness of bat flies (Nycteribiidae and Streblidae) per bat family
#Import data sets

data2<-read.csv("BatFly_Bat_Pop.csv", sep=",")
data3<-read.csv("BatFly_Fly_Pop.csv", sep=",")

fam<-unique(cbind(data2$BatFamily, data2$CurrentBatSpecies))
flyfam<-unique(cbind(data3$FlyFamily, data3$CurrentFlySpecies))

which(table(flyfam[,2])>1)

family<-NULL
for (i in 1:length(data1$CurrentBatSpecies)){#assing families to bat species in data2
  
  family[[i]]<-fam[which(fam[,2]==data1$CurrentBatSpecies[i]),1]
}

family<-unlist(family)

flyfamily<-NULL
for (i in 1:length(data1$CurrentFlySpecies)){#assing families to fly species in data3
  
  flyfamily[[i]]<-flyfam[which(flyfam[,2]==data1$CurrentFlySpecies[i]),1]
}

flyfamily<-unlist(flyfamily)

length(flyfamily)

plotdata<-unique(cbind(data1$CurrentFlySpecies,family,flyfamily))

plotdata<-as.matrix(table(plotdata[,2],plotdata[,3]))#the number of times a family name is repeated indicates parasite richness
plotdata<-plotdata[order(rowSums(plotdata)),]

#Plot

png("Figure_5.png", res = 300,
    width = 2100, height = 2000, unit = "px")
par(las=1, mar=c(4, 8, 1, 2))

barplot(t(plotdata), horiz=T, xlim=c(0,200), xlab="Parasite richness", col=c("#E5EFC1","#E5EFC1", "#39AEA9"))

legend(x=120, y=15, legend=colnames(plotdata), pch=19, pt.cex=1.5,col=c("#ffffff","#E5EFC1", "#39AEA9"),bty = "n")

dev.off()
