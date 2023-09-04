################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### BATFLY: A dataset of worldwide bat-fly interactions.
#### Figure 1. Distribution of sampling locations included in BatFly.

#### See README for further info:
#### https://github.com/NatalyaZapata/BatFly_Interactions#readme
################################################################################


######################### 1. SETTINGS ##########################################

## Clean the environment
rm(list= ls())


## Load the packages
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggsn)){
  install.packages("ggsn")
  library(ggsn)
}

if(!require(grDevices)){
  install.packages("grDevices")
  library(grDevices)
}

if(!require(sf)){
  install.packages("sf")
  library(sf)
}

if(!require(rnaturalearth)){
  install.packages("rnaturalearth")
  library(rnaturalearth)
}

if(!require(rnaturalearthdata)){
  install.packages("rnaturalearthdata")
  library(rnaturalearthdata)
}

if(!require(ggspatial)){
  install.packages("ggspatial")
  library(ggspatial)
}

if(!require(rgeos)){
  install.packages("rgeos")
  library(rgeos)
}


if(!require(grid)){
  install.packages("grid")
  library(grid)
}

if(!require(ggplotify)){
  install.packages("ggplotify")
  library(ggplotify)
}


## Import the data figure A
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

######################### 2. PLOTTING FIGURE A ######################################



bar <- ~{
  par(
    mar = c(3, 10.5, 2.5, 7),
    mgp = c(2, 0.5, 0), las=1
  )
  barplot(yeardata$Freq, names.arg=yeardata$Var1, ylim=c(0,20),
          xlab="Year of publication", ylab="Number of studies", font.lab=2, cex.names = 0.9, cex.axis = 0.9, tcl = -0.3 )
}


## Import the data Figure B
sites <- read.csv("data/BatFly_Sites.csv")
scope <- read.csv("data/BatFly_Sampling.csv")
points <- (cbind.data.frame(sites$Latitude, sites$Longitude, 
                        scope$BatEcologicalScale, scope$FlyEcologicalScale))
colnames(points)<-c("Latitude", "Longitude", "Bat_Scope", "Fly_Scope")



## Check the points
class(points)
str(points)
head(points)
tail(points)


######################### 2. PLOTTING ######################################



## Load the world map from the mapdata package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


sf_use_s2(FALSE)



## Plot the map
map <- ggplot(data = world) +
  geom_sf(colour = "white", fill = "#d3d3d3") +
  #coord_sf(lims_method="geometry_bbox", expand = TRUE) +
  coord_sf(xlim = c(-121, -30), ylim = c(-58,40), expand = F) +
  
  theme_bw() + 
  ## Plot the sites
  geom_point(data = points, aes(x = Longitude, y = Latitude, 
                                     colour = Bat_Scope,
                                     fill = Bat_Scope,
                                     alpha = Bat_Scope,
                                     shape= Fly_Scope), 
              size = 2) +
  ## Customize the colors and labels
  scale_color_manual(values = c("#5fc23e","#fa3448","#ca8d00","#cc18b1")) +
  scale_shape_manual(values = c(24,25)) +
  scale_alpha_manual(values = c(0.6,1,1,0.6),guide="none") +
  scale_fill_manual(values = c("#5fc23e","#fa3448","#ca8d00","#cc18b1"),guide="none") +
  labs(colour = "Bat scope", fill=NA, shape="Fly scope", x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 11, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black", vjust = -2,
                                    face = "bold"),
        axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                    face = "bold"),
        legend.position = c(0.2,0.4),
        legend.background = element_rect(fill = "NA"),
        legend.key = element_rect(fill = "NA"),
        plot.margin = unit(rep(0.5,4), "lines")) +
  # Add a scale bar
  ggspatial::annotation_scale(location = "bl", width_hint = 0.3,
                              bar_cols = c("grey30", "white")) +
 
  ## Add a north arrow
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    height = unit(1.5, "cm"), 
                                    width = unit(1.5, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(
                                      fill = c("white","grey30")))
  
map



# making rows and columns of different widths/heights
## Export the figure as a PNG image

png("figures/Figure_1.png", res = 300,
    width = 3000, height = 2800, unit = "px")
plot_grid(bar, 
          map, nrow=2,labels = c('A', 'B'), hjust = -9, label_size = 20,
          align = 'hv', axis = "b", scale = c(.9, 1),
          rel_heights = c(1,2),
          rel_widths = c(1)
)
dev.off()

library("grid")
library("ggplotify")
library(cowplot)
library("vcd")

