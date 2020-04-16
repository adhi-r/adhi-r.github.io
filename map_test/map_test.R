# testing interactive map making for hiring lab
# Adhi R. <adhir@indeed.com
# using this post: https://rstudio.github.io/leaflet/choropleths.html
# potentially useful comment combining albersusa and leaflet: https://github.com/rstudio/leaflet/issues/172#issuecomment-413613736
# potentially useful package: https://github.com/hrbrmstr/albersusa
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sp)

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
    jobs = NA,
    upper_state = "PR",
    state_name = "Puerto Rico"
  ))

augmented_jed_data

states <-
  geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                          , what = "sp")

states$runif_test <- runif(52, 0, 1000)
class(states)
names(states)



states$jobs_metric <- augmented_jed_data$jobs

m <- leaflet(states, options = leafletOptions(zoomControl = FALSE,
                                              minZoom = 4, maxZoom = 4)) %>%
  setView(-96, 37.8, 4)

my_pal <-
  colorNumeric(
    colorRampPalette(c("#C8D8FC","#2164F3", "#020B1E"), interpolate = "spline")(10),
    domain = states$jobs_metric,
    reverse = TRUE,
    na.color = NA
  )
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
) %>%
  setMapWidgetStyle(list(background = "white"))

source("addTitle.R")
m_int  %>%
  addTitle("Decline in job postings by state",
           fontSize = 30,
           fontFamily = "Helvetica")



colorRampPalette(c("#4097F8","#2164F3", "#223C92"), interpolate = "spline")(5)
