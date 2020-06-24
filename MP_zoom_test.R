

library(dplyr)
library(ggplot2)
library(ggmap)
library(rgdal)
library(maptools)
library(sp)
library(rgeos)
library(geosphere)

#new bound - HCM
xmin = 105.5
xmax = 107.5
ymax = 11.5
ymin = 9.9


#Zoom HCM

xmin = 106.2
xmax = 107.2
ymax = 11.4
ymin = 10.4

#zoom 13 HCM
xmin = 106.69
xmax = 106.91
ymax = 11.05
ymin = 10.8


# date converting
dt$hourz <- as.POSIXlt.character(dt$datetime, format = "%d/%m/%Y %H:%M:%S") %>% 
  strftime(format = "%H")

hour.location <- group_by(dt, hourz) %>% slice(1)
class(hour.location)

dt_z <- dt[!(dt$X > xmax | dt$X < xmin |
             dt$Y > ymax | dt$Y < ymin),]

dt_h <- group_by(dt_z, id, hourz, dayz, minz) %>% slice(1)

cter <- c(mean(dt$X), mean(dt$Y))
cter <- c(106.473910, 10.808367) #cter HCM
cter <- c(106.65, 10.85) # HCM cter 2
cter <- c(106.8, 10.92 ) #HCM cter zoom 13


mp <- get_map(location = cter, zoom = 12)

htmap <- ggmap(mp, extent = "panel", maprange = FALSE) + 
  stat_density2d(data = dt_h, 
                 aes(x = X, y = Y, fill = ..level.., alpha = 0.1),
                 size = 0.01, bins = 3, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red")
htmap
pmap <- ggmap(mp, extent = "panel")+
  geom_point(data = dt_z, mapping = aes(x = X, y = Y), colour = "red",
             size = 0.1)
pmap

ggsave("zz.jpg", plot = pmap,  
       path = "C:/Users/Administrator/Downloads", width = 20, height = 11.25, units = "in")
