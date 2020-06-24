tezt <- as.data.frame(read_json(lt[1], simplifyVector = TRUE))
tezt$hourz <- as.POSIXlt.character(tezt$date, format = "%d/%m/%Y %H:%M:%S") %>% 
  strftime(format = "%H")

hour.location <- group_by(tezt, hourz) %>% slice(1)
class(hour.location)
