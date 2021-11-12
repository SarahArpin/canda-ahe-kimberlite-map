library(tidyverse)
library(Rcpp)
library(sf) # the base package manipulating shapes
library(rgdal) # geo data abstraction library
library(geojsonio) # geo json input and output
library(spdplyr) # the `dplyr` counterpart for shapes
library(rmapshaper) # the package that allows geo shape transformation
library(magrittr) # data wrangling
library(ggplot2) # general data wrangling and plotting
library(sp)

########################################################################################################################
crs_string <- "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2
canada_cd <- st_read("data/canada_cd_sim.geojson", quiet = TRUE) # canada
canada_cd_t <- st_transform(canada_cd, crs_string) # canada with transformed to correct crs

theme_map <- function(base_size=9, base_family="") { # 3
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}
# Define the filling colors for each province; max allowed is 9 but good enough for the 13 provinces + territories
map_colors <- RColorBrewer::brewer.pal(9, "Pastel1") %>% rep(37) # 4

# Plot the maps
ggplot() +
  geom_sf(aes(fill = PRUID), color = "gray60", size = 0.1, data = canada_cd) + # 5
  coord_sf(crs = crs_string) + # 6
  scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
########################################################################################################################
bbox <- st_bbox(crop_cd[[2]][[1]])
bbox <- st_set_crs(bbox, crs_string)
st_crs(bbox)
st_crop(canada_cd, bbox) 
########################################################################################################################
sf_cities = city_coords %>%
  dplyr::select(long, lat) %>% # 1
  as.matrix() %>% # 2
  st_multipoint(dim = 'XY') %>% # 3
  st_sfc() %>% # 4
  st_set_crs(4269) # 5

crop_custom <- function(poly.sf) {
  poly.sp <- as(poly.sf, "Spatial")
  print(poly.sp)
  poly.sp.crop <- crop(poly.sp, extent(c(-120, -100, 58, 80)))
  st_as_sf(poly.sp.crop)
}
cropped <- crop_custom(canada_cd)

ggplot() +
  geom_sf(aes(fill = PRUID), color = "gray60", size = 0.1, data = cropped) + # 5
  geom_sf(data = sf_cities, color = 'red', size = 3)+
  coord_sf(crs = crs_string) + # 6
  scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
################################################################################################
#different library and method
library(raster)
library(maps)
library(mapdata)
library(ggmap)
canada<-getData("GADM",country="CAN",level=1)
canada_1<-st_as_sf(canada)

#plot canada, similar overall plot as the method above
plot(canada,xlim=c(-141,-53),ylim=c(40,85),col="#FFFFCC",border="#33CC00",axes=T,las=1)
invisible(text(getSpPPolygonsLabptSlots(canada),labels=as.character(substr(canada$HASC_1,4,5)), 
               cex=0.75, col="black",font=2))
################################################################################################
#narrow down to a region using terrain map from google
lats<-c(50,80)
lons<-c(-150,-100)
bb<-make_bbox(lon=lons,lat=lats,f=0.05)
cda<-get_map(bb,zoom=4,maptype="terrain")

conn = textConnection("city	lat	long
coronation_gulf	68.33	-110.368")
city_coords = read.delim(conn, stringsAsFactors = F)

lat <- c(55,68,75)
long <- c(-145,-135,-115)
values = c(8,15,7)
fake_data=data.frame(lat,long,values)

#plot region with point on the interested area
ggmap(cda)+xlim(lons)+ylim(lats)+
  theme_bw()+labs(x="Longitude",y="Latitude")+
  geom_point(data=fake_data,aes(long,lat,size=values))+
  geom_text(data=fake_data,aes(x=long,y=lat,label=values),hjust=1.5, vjust=0.2)+
  geom_point(data=city_coords,aes(x=long,y=lat),color="red",size=5)+
  theme(legend.position = "none")










