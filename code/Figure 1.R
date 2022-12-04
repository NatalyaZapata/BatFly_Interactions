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


## Import the data
sites <- read.csv("data/BatFly_Site.csv")
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
  coord_sf(xlim = c(-130, -30), ylim = c(-58,55), expand = F) +
  
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
 
  ## Add a north arrow
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    height = unit(1.5, "cm"), 
                                    width = unit(1.5, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(
                                      fill = c("white","grey30")))
  

## Export the map as a PNG image
png("figures/Figure_1.png", res = 300,
    width = 4000, height = 2200, unit = "px")
map
dev.off()
