library(arrow)
library(sf)
library(dplyr)
library(tigris)
library(rdeck) 
library(tmap)
options(tigris_use_cache = TRUE)

petersburg<- counties(state="VA", cb=T) %>%
  filter(NAME=="Petersburg")
pb_bbox<-counties(state="VA", cb=T) %>%
  filter(NAME=="Petersburg") %>%
  st_bbox() 

pb_buildings <- open_dataset('s3://overturemaps-us-west-2/release/2024-05-16-beta.0/theme=buildings?region=us-west-2') %>%
  filter(bbox$xmin > pb_bbox$xmin,
         bbox$ymin > pb_bbox$ymin,
         bbox$xmax < pb_bbox$xmax,
         bbox$ymax < pb_bbox$ymax) %>%
  select(id, geometry, height) %>%
  collect() %>%
  st_as_sf(crs=4326) %>%
  mutate(height=ifelse(is.na(height),8,height))

#saveRDS(pb_buildings,"pb_buildings.RDS")
rdeck(map_style = mapbox_light(), 
      initial_view_state = view_state(
        center = c(-77.39132, 37.20429),
        zoom = 11.3,
        bearing = -60,
        pitch = 76
      )) %>%
  add_polygon_layer(
    data = pb_buildings, 
    name = "Petersburg",
    get_polygon = geometry, 
    get_elevation = height, 
    get_fill_color = scale_color_linear(
      col = height,
      palette = viridisLite::inferno(100, direction = -1)
    ),
    extruded = TRUE, 
    opacity = 0.5)

tm_shape(petersburg)+
  tm_borders()+
  tm_shape(pb_buildings)+
  tm_dots(col="height", pal=c("orange","blue"), size=0.05)