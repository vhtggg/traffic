######### Library and shit ################# ################


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

xmin <- min(Road_f$long)
xmax <- max(Road_f$long)
ymin <- min(Road_f$lat)
ymax <- max(Road_f$lat)

############### Make point + adjusting ##### ###############
df <- dataz[[1]]
df <- df[!(df$speed == 0 |
             df$x > xmax | df$x <xmin |
             df$y > ymax | df$y < ymin),]
pz <- SpatialPoints(cbind(df$x, df$y), proj4string = WGS) %>%
  spTransform(crs)
pts_list <- list(pz)


for (i in 2:24){
  df <- dataz[[i]]
  df <- df[!(df$speed == 0 |
               df$x > xmax | df$x <xmin |
               df$y > ymax | df$y < ymin),]
  pz <- SpatialPoints(cbind(df$x, df$y), proj4string = WGS) %>%
    spTransform(crs)
  pts_list <- append(pts_list, pz)
}


###### SNapping + make traffic map ver 1 ### ##########################
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
    labs(title = paste("Taxi traffic in HCM city at ", timez[i],":00",
                       sep = ""),
         x = "lon", y = "lat")
  road_m
  ggsave(paste("Traffic_", timez[i],".jpg", sep = ""), plot = road_m,
         path = "D:/HCM taxi", width = 20, height = 11.25, units = "in")
}

########## calculate distance ############## #########
idz <- trffc$id %>% unique()
distz <- data.frame("id" = c(rep(NA, length(idz))), "dist" = c(rep(NA, length(idz))))


for (i in 1:length(idz)) {
  a <- filter(trffc, id == idz[i])
  distz$dist[i] <- distGeo(c(a$long[1], a$lat[1]), c(a$long[2], a$lat[2]))
  distz$id[i] <- idz[i]
}
tezt <- merge(distz, trffc, by = "id")
tezt$den <- tezt$freq/tezt$dist*100


teztz <- tezt[!(tezt$den > 10),]
road_m <- ggmap(mp, extent = "panel", maprange = FALSE)+
  geom_path(data = teztz, aes(x = long, y = lat, 
                              color = den, group = id), size = 1)+
  scale_color_gradient(low = "green4", high = "red3",
                       guide_legend(title = "Traffic"))+
  labs(title = paste("Taxi traffic in HCM city"),
       x = "lon", y = "lat")
road_m


############# SPEED ######################## ##############

sp_df_12 <- cbind.data.frame(snap_df, "Speed" = df$speed)
sp_freq_12 <- sp_df_12 %>% group_by(nearest_line_id) %>% summarise(
  freq = n(),
  meansp = mean(Speed)
)

trffc_sp <- merge(Road_f, sp_freq_12, 
                  by.x = "id", by.y = "nearest_line_id")
spmap <- ggmap(mp, extent = "panel", maprange = FALSE)+
  geom_path(data = trffc_sp, aes(x = long, y = lat, 
                              color = rank, group = id), size = 1)+
  scale_color_manual(values = c("red4", "red1", "orange", "green"),
                       guide_legend(title = "Average Speed"))+
  labs(title = paste("Taxi average speed in HCM city at ", timez[12],":00",
                     sep = ""),
       x = "lon", y = "lat")
spmap
ggsave(paste("A_speed_", timez[i],".jpg", sep = ""), plot = spmap,
       path = "D:/HCM taxi", width = 20, height = 11.25, units = "in")

trffc_sp$rank <- NA
for (i in 1:length(trffc_sp$rank)) {
  if (trffc_sp$meansp[i] < 3) trffc_sp$rank[i] = 1 else {
    if (trffc_sp$meansp[i] < 5) trffc_sp$rank[i] = 2 else {
      if (trffc_sp$meansp[i] < 10) trffc_sp$rank[i] = 3 else
        trffc_sp$rank[i] = 4
    }
  }
}



cl <- c("0-3" = "red4", 
        "3-5" = "red",
        "5-10" = "orange",
        "> 10" = "dark green")

spmap <- ggmap(mp, extent = "panel", maprange = FALSE)+
  geom_path(data = trffc_sp, aes(x = long, y = lat, 
                                 color = factor(rank), group = id), size = 1)+
  scale_color_manual(values = c("red4", "red", "orange", "dark green"),
                     breaks = c(1,2,3,4),
                     label = c("0-3", "3-5", "5-10","> 10"),
                     guide_legend(title = "Average Speed (km/h)"))+
  labs(title = paste("Taxi average speed in HCM city at ", timez[12],":00",
                     sep = ""),
       x = "lon", y = "lat")
spmap










































