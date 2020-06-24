library(dplyr)
library(jsonlite)

lt <- list.files(path = "/media/tgggvx/000ACBE9000FD609/TH"
           ,pattern = "*.json", full.names = TRUE)

###### loop for data final ######

z <- as.data.frame(NULL)
for(i in 100:200){
  a <- read_json(lt[i])
  b <- as.data.frame(a[1])
  for(j in 1:length(a)){
    b1 <-as.data.frame(a[i+1])
    b <- rbind(b,b1)
  }
  z <- rbind(z,b)
}
final <- rbind(final,z)



