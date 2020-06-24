library(dplyr)
library(jsonlite)
library(ggplot2)
library(ggmap)


lt <- list.files(path = "/media/tgggvx/000ACBE9000FD609/TH"
                 ,pattern = "*.json", full.names = TRUE)
########### Make data #################
final <- as.data.frame(NULL)
for (i in 1:length(lt)){
  tezt <- as.data.frame(read_json(lt[i], simplifyVector = TRUE))
  tezt$hourz <- as.POSIXlt.character(tezt$date, format = "%d/%m/%Y %H:%M:%S") %>% 
    strftime(format = "%H")
  loc24 <- as.data.frame(group_by(tezt, hourz) %>% slice(1))
  final <- rbind(final,loc24)
}
timez <- unique(final$hourz)
for (i in 1:24){
  assign(paste("final",timez[i],sep = "_"), filter(final, hourz == timez[i]))
}



dataz <- list(final_00, final_01, final_02, final_03, 
              final_04, final_05, final_06, final_07,
              final_08, final_09, final_10, final_11,
              final_12, final_13, final_14, final_15,
              final_16, final_17, final_18, final_19,
              final_20, final_21, final_22, final_23)
rm(tezt, loc24)

############# Mapping ####################
cter <- c(mean(final$x),mean(final$y))
mp <- get_map(location = cter, zoom = 13)
for(i in 2:6){
  htmap <- ggmap(mp, extent = "panel", maprange = FALSE) + 
    stat_density2d(data = as.data.frame(dataz[i]), 
                   aes(x = x, y = y, fill = ..level.., alpha = 0.1),
                   size = 0.01, bins = 8, geom = "polygon") + 
    scale_fill_gradient(low = "green", high = "red", 
                        guide_legend(title = "Mật độ xe")) +
    scale_alpha(guide = FALSE)+
    labs(title = paste("Mật độ taxi tại TP.HCM vào", timez[i],"giờ",sep = " "), 
         x = "Vĩ độ", y = "Kinh độ")
  ggsave(paste("heatmap_",timez[i],".jpg",sep = ""), plot = htmap,  
         path = "/home/tgggvx/Desktop/heatmap/",
         width = 20, height = 11.25, units = "in")
}






cter <- c(mean(final$x),mean(final$y))
mp <- get_map(location = cter, zoom = 13)
pmap <- ggmap(mp, extent = "panel")+
  geom_point(data = as.data.frame(dataz[1]), mapping = aes(x = x, y = y), colour = "red",
             size = 0.1)
pmap
htmap <- ggmap(mp, extent = "panel", maprange = FALSE) + 
  stat_density2d(data = as.data.frame(dataz[5]), 
                 aes(x = x, y = y, fill = ..level.., alpha = 0.1),
                 size = 0.01, bins = 8, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", guide_legend(title = "density")) +
  scale_alpha(guide = FALSE)+
  labs(title = "Car density HCM", x = "Vi do", y = "Kinh do")
htmap

ggsave("heatmap_hour_00.jpg", plot = pmap,  
       path = "/home/tgggvx/Desktop/heatmap/", width = 20, height = 11.25, units = "in")







