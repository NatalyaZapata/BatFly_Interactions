################################################################################
##### BATFLY: A dataset of worldwide bat-fly interactions.
##### Figure 1. Distribution of sampling locations included in BatFly.
##### See README for further info:
##### https://github.com/NatalyaZapata/BatFly-A-dataset-of-worldwide-bat-fly-interactions/blob/main/README.md
################################################################################



#Set the stage
cat("\014")  

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

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

if (!dir.exists(path = "results")){
  dir.create(path = "results")
} else {
  print("Dir already exists!")
}

# Load the packages
library(dplyr)
library(ggplot2)
library(grDevices)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

# Import the data sets
sites <- read.csv("data/batfly sites.csv")
scope <- read.csv("data/batfly sampling.csv")
data<-(cbind.data.frame(sites$Latitude, sites$Longitude, 
                        scope$BatEcologicalScale, scope$FlyEcologicalScale))
colnames(data)<-c("Latitude", "Longitude", "Bat_Scope", "Fly_Scope")

# Check the data
class(data)
str(data)
head(data)

# Load the world map from the mapdata package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

sf_use_s2(FALSE)

# Make the map
mapa <- ggplot(data = world) +
  geom_sf(colour = "white", fill = "#d3d3d3") +
  #coord_sf(lims_method="geometry_bbox", expand = TRUE) +
  coord_sf(xlim = c(-170, 180), ylim = c(-62,89), expand = F) +
  
  theme_bw() + 
  # Plot the sites
  geom_point(data = data, aes(x = Longitude, y = Latitude, 
                                     colour = Bat_Scope,
                                     fill = Bat_Scope,
                                     alpha = Bat_Scope,
                                     shape= Fly_Scope), 
              size = 2) +
  # Customize the colors and labels
  scale_color_manual(values = c("#5fc23e","#fa3448","#ca8d00","#cc18b1")) +
  scale_shape_manual(values = c(24,25)) +
  scale_alpha_manual(values = c(0.6,1,1,0.6),guide="none") +
  scale_fill_manual(values = c("#5fc23e","#fa3448","#ca8d00","#cc18b1"),guide="none") +
  labs(colour = "Bat scope", fill=NA, shape="Fly scope", x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 11, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black", vjust = -4,
                                    face = "bold"),
        axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                    face = "bold"),
        legend.position = c(0.1,0.4),
        legend.background = element_rect(fill = "NA"),
        legend.key = element_rect(fill = "NA"),
        plot.margin = unit(rep(0.5,4), "lines")) +
 
  # Add a north arrow
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    height = unit(1.5, "cm"), 
                                    width = unit(1.5, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(
                                      fill = c("white","grey30")))


# See the map
mapa

# Export the map as a PNG image
png("Figure_1.png", res = 300,
    width = 4000, height = 2200, unit = "px")
mapa

dev.off()
