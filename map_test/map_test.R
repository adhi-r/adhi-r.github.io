# testing interactive map making for hiring lab
# Adhi R. <adhir@indeed.com
# using this post: https://rstudio.github.io/leaflet/choropleths.html

library(tidyverse)
library(leaflet)
library(sp)

states <-
  geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                          , what = "sp")

states$runif_test <- runif(52, 0, 1000)
class(states)
names(states)


mapbox_api_key <-
  "pk.eyJ1IjoiYWRoaXJhamFwcmFiIiwiYSI6ImNrOHhya3F4NDFhamEzZnA2enpyZzJ4bXkifQ.KDyQDWWPj0ZkTxPcrTatxA"

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addTiles()

m %>% addPolygons()

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("Oranges", domain = states$density, bins = bins)
# more colors can be found here: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/

m %>% addPolygons(
  fillColor = ~ pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
)

m %>% addPolygons(
  fillColor = ~ pal(runif_test),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
)

m %>% addPolygons(
  fillColor = ~ pal(runif_test),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE
  )
)


labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup><br/>%g random number<br/>Hi Rachel and Margot!",
  states$name,
  states$density,
  states$runif_test
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~ pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE
  ),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"
  )
)
m

m %>% addLegend(
  pal = pal,
  values = ~ density,
  opacity = 0.7,
  title = NULL,
  position = "bottomright"
)


# indeed colors?
if (FALSE){
ind_pal <-
  colorRamp(c("#2164F3", "#FF6600"), interpolate = "spline")

m %>% addPolygons(
  fillColor = ~ ind_pal(quantile(density)),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
)
}
