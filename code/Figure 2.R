################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of Neotropical bat-fly interactions.
#### Figure 2. Number of studies throughout the recorded period 

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly_Interactions#readme
################################################################################


######################### 1. SETTINGS ##########################################

## Clean the environment
rm(list= ls())



## Import the data
data<-read.csv("data/BatFly_References.csv", sep=",")


## Check the data
class(data)
str(data)
head(data)
tail(data)


## Find the missing years
years<-as.data.frame(table(data$Year))

dy<-rep(NA, length(seq(1904,2022, by=1)))
for (i in 1:length(seq(1904,2022, by=1))){
  dy[i]<-sum(seq(1904,2022, by=1)[i]==years$Var1)
}

miss.year<-cbind(seq(1904,2022, by=1), dy)[which(cbind(seq(1904,2022, by=1), dy)[,2]==0),1]
miss.year<-data.frame(Var1=sapply(miss.year, as.factor), Freq=rep(0, length(miss.year)))
all.years<-rbind(years, miss.year)


yeardata<-all.years[order((as.numeric(as.character(all.years$Var1)))),]

class(yeardata)
str(yeardata)
head(yeardata)
tail(yeardata)
nrow(yeardata)
View(yeardata)
######################### 2. PLOTTING ######################################


png("figures/Figure_2.png", res = 300,
    width = 3000, height = 2000, unit = "px")

par(las=1, mar=c(5, 5, 4, 2))
barplot(yeardata$Freq, names.arg=yeardata$Var1, ylim=c(0,16),
        xlab="Year of publication", ylab="Number of studies")
        #xaxp=c(1904, 2022,59), xaxt='n')


dev.off()


