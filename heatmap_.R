library(dplyr)
library(jsonlite)
library(ggmap)
library(ggplot2)

z
head(z)
cter <- c(mean(z$x),mean(z$y))
mp <- get_map(location = cter, zoom = 13)
final <- ggmap(mp, extent = "panel")+
  geom_point(data = z, mapping = aes(x = z$x, y = z$y), colour = "red",
             size = 2, alpha = 0.1)
final
###################################################
htmap <- ggmap(mp, extent = "panel", maprange = FALSE) + 
geom_density2d(data = z, aes(x = z$x, y = z$y)) + 
stat_density2d(data = z, aes(x = z$x, y = z$y, fill = ..level.., alpha = ..level..),
                size = 0.01, bins = 16, geom = "polygon") + 
scale_fill_gradient(low = "green", high = "red") + 
scale_alpha(range = c(0.00, 0.25), guide = FALSE) + 
theme(legend.position = "none", axis.title = element_blank(), 
       text = element_text(size =12))
htmap
mp
