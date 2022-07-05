################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of worldwide bat-fly interactions.
#### Figure 2. Number of studies throughout the recorded period 

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly-A-dataset-of-worldwide-bat-fly-interactions/blob/main/README.md
################################################################################

######################### 1. SETTINGS ##########################################

## Clean the environment
rm(list= ls())

## Importing data
data<-read.csv("data/BatFly_References.csv", sep=",")

## Finding missing years
years<-as.data.frame(table(data$Year))

dy<-rep(NA, length(seq(1989,2022, by=1)))
for (i in 1:length(seq(1989,2022, by=1))){
  dy[i]<-sum(seq(1989,2022, by=1)[i]==years$Var1)
}

miss.year<-cbind(seq(1989,2022, by=1), dy)[which(cbind(seq(1989,2022, by=1), dy)[,2]==0),1]
miss.year<-data.frame(Var1=sapply(miss.year, as.factor), Freq=rep(0, length(miss.year)))
all.years<-rbind(years, miss.year)


yeardata<-all.years[order((as.numeric(as.character(all.years$Var1)))),]
yeardata<-yeardata[-34,]#remove unpublished data sets (brand new data)


######################### 2. PLOTTING ######################################

png("figures/Figure_2.png", res = 300,
    width = 3000, height = 2000, unit = "px")

par(las=1, mar=c(5, 5, 4, 2))
barplot(yeardata$Freq, names.arg=yeardata$Var1, ylim=c(0,16),
        xlab="Year of publication", ylab="Number of studies")

dev.off()