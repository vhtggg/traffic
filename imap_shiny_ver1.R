

############### making imap ###############
pal <- colorFactor(palette = c("red4", "red", "orange", "dark green"),
                   domain = c(1, 2 , 3, 4))
imap <- leaflet() %>% addTiles()


for (i in 1:(4610/2)) {
  j = i*2-1
  imap <- imap %>% addPolylines(
    lng = c(trffc_sp$long[j], trffc_sp$long[j+1]),
    lat = c(trffc_sp$lat[j] , trffc_sp$lat[j+1] ),
    color = pal(trffc_sp$rank)[j],
    weight = 5,
    opacity = 1
  )
}
################## upload shiny #############
ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    imap 
  })
}
shinyApp(ui, server)

