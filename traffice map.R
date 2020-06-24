library(dplyr)
library(ggplot2)
library(ggmap)
library(rgdal)
library(maptools)
library(sp)
library(rgeos)
library(geosphere)

WGS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs <- CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

xmax <- max(Road_f$long)
xmin <- min(Road_f$long)
ymax <- max(Road_f$lat)
ymin <- min(Road_f$lat)



df <- dataz[[1]]
df <- df[!(df$speed == 0 |
             df$x > xmax | df$x < xmin |
             df$y > ymax | df$y < ymin),]
pz <- SpatialPoints(cbind(df$x, df$y), proj4string = WGS) %>%
  spTransform(crs)
pts_list <- list(pz)


for (i in 2:24){
  df <- dataz[[i]]
  df <- df[!(df$speed == 0 |
               df$x > xmax | df$x < xmin |
               df$y > ymax | df$y < ymin),]
  pz <- SpatialPoints(cbind(df$x, df$y), proj4string = WGS) %>%
    spTransform(crs)
  pts_list <- append(pts_list, pz)
}

for (i in 12:12){
  snap <- snapPointsToLines(pts_list[[i]], Road_utm)
  snap_xy <- spTransform(snap, WGS)
  snap_df <- snap_xy %>% as.data.frame()
  snap_freq <- snap_df %>% group_by(nearest_line_id) %>% summarise(
    freq = n()
  )
  trffc <- merge(Road_f, snap_freq, 
                 by.x = "id", by.y = "nearest_line_id")
  road_m <- ggmap(mp, extent = "panel", maprange = FALSE)+
    geom_path(data = trffc, aes(x = long, y = lat, 
                               color = freq, group = id), size = 1)+
    scale_color_gradient(low = "dark green", high = "red",
                         guide_legend(title = "Traffic"))+
    labs(title = paste("Taxi traffic in HCM city at 00:00"),
         x = "lon", y = "lat")
  road_m
  ggsave(paste("Traffic_", timez[i],".jpg", sep = ""), plot = road_m,
         path = "D:/HCM taxi", width = 20, height = 11.25, units = "in")
}















