# testing interactive map making for hiring lab
# Adhi R. <adhir@indeed.com
# using this post: https://rstudio.github.io/leaflet/choropleths.html
# potentially useful comment combining albersusa and leaflet: https://github.com/rstudio/leaflet/issues/172#issuecomment-413613736
# potentially useful package: https://github.com/hrbrmstr/albersusa
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sp)

states <-
  geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                          , what = "sp")

states$runif_test <- runif(52, 0, 1000)
class(states)
names(states)

jed_data <- read_csv(file = "state_change_to_apr10.csv")

augmented_jed_data <- jed_data %>%
  mutate(upper_state = str_to_upper(state),
         jobs = round(jobs * 100, 1)) %>%
  left_join(
    tibble(state_abb = state.abb, state_name = state.name),
    by = c("upper_state" = "state_abb")
  ) %>%
  mutate(state_name = if_else(state == "dc", "District of Columbia", state_name)) %>%
  arrange(state_name) %>%
  bind_rows(tibble(
    state = "pr",
    jobs = 0,
    upper_state = "PR",
    state_name = "Puerto Rico"
  ))

augmented_jed_data

states$jobs_metric <- augmented_jed_data$jobs

mapbox_api_key <-
  "pk.eyJ1IjoiYWRoaXJhamFwcmFiIiwiYSI6ImNrOHhya3F4NDFhamEzZnA2enpyZzJ4bXkifQ.KDyQDWWPj0ZkTxPcrTatxA"

m <- leaflet(states) %>%
  setView(-96, 37.8, 5)

m %>% addPolygons()

bins1 <- (c(0, -0.1, -0.2, -0.3, -0.4, -0.5) * 100)

bins2 <-
  c(-0.5, -0.45, -0.4, -0.35, -0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0) * 100
bins3 <- c(-0.5, -0.4, -0.3, -0.2, -0.1, 0) * 100
bins4 <- c(0, -.3, -.32, -.35, -.5) * 100
my_pal <-
  colorBin("Blues", domain = states$jobs_metric, bins = bins4, reverse = TRUE)
# more colors can be found here: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/

labels <- sprintf(
  "<strong>%s</strong><br/>Change in postings trend: %g&#37;",
  states$name,
  states$jobs_metric
) %>% lapply(htmltools::HTML)

m_int <- m %>% addPolygons(
  fillColor = ~ my_pal(jobs_metric),
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
) %>% addLegend(
  pal = my_pal,
  values = ~ jobs_metric,
  opacity = 0.7,
  title = "Change in postings trend",
  position = "bottomright",
  #na.label = NA,
  labFormat = labelFormat(suffix = "%", between = "% to ")
)
m_int %>%
  setMapWidgetStyle(list(background = "white"))


# indeed colors?
if (FALSE) {
  ind_pal <-
    colorRampPalette(c("#2164F3", "#FF6600"), interpolate = "spline")(5)

  m %>% addPolygons(
    fillColor = ~ ind_pal(jobs_metric),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7
  )
}
