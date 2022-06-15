#Figure 3. Relative frequency of interactions of the 15 most frequently recorded bat (A) and fly species (B).

#import data set
data1<-read.csv("BAT-FLY_INTERACTIONS_Species.csv", sep=",")

#Plot A

png("Figure_3.png", res = 300,
    width = 4000, height = 2000, unit = "px")

layout(matrix(c(1,2), ncol=2))
par(las=1, mar=c(4, 11, 1, 2))

plotdata2<-sort(table(data1$CurrentBatSpecies))


barplot(100*plotdata2[(length(plotdata2)-14):length(plotdata2)]/length(data1$CurrentBatSpecies),
        horiz=T, xlim=c(0,10),xaxt="n", xlab="Relative frequency of recorded interactions (%)",
        col="#7a5195", font.axis=3, main="A")
axis(1, seq(0,10,by=2),)

#Plot B

par(las=1, mar=c(4, 12, 1, 2))

plotdata3<-sort(table(data1$CurrentFlySpecies))


barplot(100*plotdata3[(length(plotdata3)-14):length(plotdata3)]/length(data1$CurrentFlySpecies),
        horiz=T, xaxt="n", col="#96d0ab", font.axis=3, 
        xlim=c(0,8), xlab="Relative frequency of recorded interactions (%)", main="B")

axis(1, seq(0,8,by=2),)

dev.off()

