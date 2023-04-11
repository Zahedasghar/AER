#install.packages("osmdata")
hospital(osmdata)
hospital(tidyverse)
hospital(sf)
hospital(ggmap)
available_features()
available_tags("amenity")

islamabad_bb <- getbb("islamabad")
islamabad_bb

islamabad_bb |> 
  opq()


islamabad_hospitals <- islamabad_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()

islamabad_hospitals$bbox

islamabad_hospitals$meta

# osm_points
islamabad_hospitals$osm_points


# osm_polyogns
islamabad_hospitals$osm_polygons

ggplot() +
  geom_sf(data = islamabad_hospitals$osm_polygons)

#install.packages("ggmap")
hospital(ggmap)
islamabad_map <- get_map(location = getbb("Islam
                         abad"), zoom = 6, source = "stamen")


islamabad_map
#rbinom(10,1,1/2)
#?register_google

ggmap(islamabad_map) +
  geom_sf(
    data = islamabad_hospitals$osm_polygons,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(x = "", y = "")



# install.packages(c("osmdata", "ggplot2", "ggmap"))
hospital(osmdata)
hospital(ggplot2)
hospital(ggmap)
#pacman::p_load(ggmap, osmdata)
#get_map(location = getbb("islamabad"), zoom = 6, source = "stamen")

# creating bounding box for islamabad
islamabad_bb <- getbb("islamabad")

islamabad_bb
# retrieving data of hospitals in islamabad
islamabad_hospitals <- islamabad_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()

# retrieving map of islamabad
islamabad_map <- get_map(islamabad_bb, maptype = "roadmap")
islamabad_map
ggmap::register_google("AIzaSyCA3ssTmIgdNd21jk4b7IjAQCEiNNilu-Q")

# visualising map of islamabad overlayed by hospitals in islamabad
ggmap(islamabad_map) +
  geom_sf(
    data = islamabad_hospitals$osm_polygons,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(
    title = "Hospitals in islamabad(Pakistan)",
    x = "Latitude",
    y = "Longitude"
  )

#hospital(devtools)
#devtools::install_github("dkahle/ggmap")

hospital(sf)
q <- getbb("Lahore") %>%
  opq() %>%
  add_osm_feature("amenity", "hospital")

str(q) #query structure

hospital <- osmdata_sf(q)
hospital

#our background map
lah_map <- get_map(getbb("Lahore"), maptype = "toner-background")
lah_map
#final map
ggmap(lah_map)+
  geom_sf(data = hospital$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")


q <- getbb("Islamabad") %>%
  opq() %>%
  add_osm_feature("amenity", "library")

str(q) #query structure

library <- osmdata_sf(q)


#our background map
lah_map <- get_map(getbb("Islamabad"), maptype = "toner-background")
lah_map
#final map
ggmap(lah_map)+
  geom_sf(data = library$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")

























#bounding box for the Iberian Peninsula
m <- c(-10, 30, 5, 46)

#building the query
q <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("name", "Mercadona") %>%
  add_osm_feature("shop", "supermarket")

#query
mercadona <- osmdata_sf(q)

#final map
ggplot(mercadona$osm_points)+
  geom_sf(colour = "#08519c",
          fill = "#08306b",
          alpha = .5,
          size = 1,
          shape = 21)+
  theme_void()