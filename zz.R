dtzz <- dtz[[1]] %>% filter(secz < 30)%>%list()
dtzz <- append(dtzz,
               dtz[[1]] %>% filter(secz >= 30)%>%list())
for (i in 2:5){
  dtzz <- append(dtzz,
                 dtz[[i]] %>% filter(secz < 30)%>%list())
  dtzz <- append(dtzz,
                 dtz[[1]] %>% filter(secz >= 30)%>%list())
}
################################################################
df <- dtzz[[1]]
df <- df[!(df$speed == 0 |
             df$x > xmax | df$x <xmin |
             df$y > ymax | df$y < ymin),]
dfz <- list(df)
pz <- SpatialPoints(cbind(df$x, df$y), proj4string = WGS) %>%
  spTransform(crs)
pts_list <- list(pz)

for (i in 2:10){
  df <- dtzz[[i]]
  df <- df[!(df$speed == 0 |
               df$x > xmax | df$x <xmin |
               df$y > ymax | df$y < ymin),]
  dfz <- append(dfz,
                list(df))
  pz <- SpatialPoints(cbind(df$x, df$y), proj4string = WGS) %>%
    spTransform(crs)
  pts_list <- append(pts_list, list(pz))
}
sp_17 <- as.data.frame(NULL)
for (i in 1:1){
  snap <- snapPointsToLines(pts_list[[i]], Road_utm)
  snap_df <- spTransform(snap, WGS)%>%as.data.frame()
  sp_17 <- cbind.data.frame(snap_df, "Speed" = dfz[[i]]$speed)%>%
    rbind(sp_17)
}

sum_17 <-sp_17 %>% group_by(nearest_line_id) %>% summarise(
  freq = n(),
  meansp = mean(Speed),
  varsp = var(Speed)
)

tezt <- merge(Road_f, sum_17, 
               by.x = "id", by.y = "nearest_line_id")
tezt$rank <- NA
for (i in 1:length(tezt$rank)) {
  if (tezt$meansp[i] < 3) tezt$rank[i] = 1 else {
    if (tezt$meansp[i] < 5) tezt$rank[i] = 2 else {
      if (tezt$meansp[i] < 10) tezt$rank[i] = 3 else
        tezt$rank[i] = 4
    }
  }
}


for (i in 1:length(tezt$id)) if (tezt$freq[i] == 1) tezt$varsp[i] = 0 





spmap <- ggmap(mp, extent = "panel", maprange = FALSE)+
  geom_path(data = tezt, aes(x = long, y = lat, 
                                 color = factor(rank), group = id), size = 1)+
  scale_color_manual(values = c("red4", "red", "orange", "dark green"),
                     breaks = c(1,2,3,4),
                     label = c("0-3", "3-5", "5-10","> 10"),
                     guide_legend(title = "Average Speed (km/h)"))+
  labs(title = "Taxi average speed 17h->17h02",
       x = "lon", y = "lat")
spmap


road_m <- ggmap(mp, extent = "panel", maprange = FALSE)+
  geom_path(data = tezt, aes(x = long, y = lat, 
                              color = varsp, group = id), size = 1)+
  scale_color_gradient(low = "green4", high = "red3",
                       guide_legend(title = "Traffic"))+
  labs(title = paste("Taxi traffic in HCM city"),
       x = "lon", y = "lat")
road_m

