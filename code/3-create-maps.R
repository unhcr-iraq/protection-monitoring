## testing a plot of coordinates
plot(data.map$latitude, data.map$longitude )

###Maps!!


################# Prepare map background ########
# Iraq bbox           38.782   28.833   49.433   37.358

#get_stamenmap(bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
#              zoom = 10,
#              maptype = c("terrain", "watercolor", "toner"),
#              crop = TRUE, messaging = FALSE, urlonly = FALSE,
#              filename = "ggmapTemp", color = c("color", "bw"))

require(ggplot2)
require(ggmap)

iraqstamentoner <-get_stamenmap(bbox = c(left = 37.782, bottom = 27.833, right = 50.433, top = 38.358),
                                zoom = 6,
                                maptype = "toner",
                                crop = TRUE, messaging = FALSE, urlonly = FALSE,
                                filename = "out/iraqstamentoner.png", color =  "bw")
toner <- ggmap(iraqstamentoner)
ggsave("out/map-toner.png", toner, width=8, height=6,units="in", dpi=300)



#osm <- get_openstreetmap(bbox = c(left = 37.782, bottom = 27.833, right = 50.433, top = 38.358),
#                         scale = OSM_scale_lookup(6))
#osmm <- ggmap(osm)
#ggsave("plot/map/map-osm.png", osmm, width=8, height=6,units="in", dpi=300)

googleterrain <- get_map(location = c(lon =44.538574, lat = 32.750323),
                         color = "color", source = "google",maptype = "terrain", zoom = 6)
googleeterrain <- ggmap(googleterrain)
#ggsave("plot/map/map-googleterrain.png", googleeterrain, width=8, height=6,units="in", dpi=300)




rm(map1)
map1 <-ggmap(googleterrain)+
  geom_point(aes(size = 1, x = longitude, y = latitude),data=data.map, color="coral1")+  
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map1.png", map1, width=8, height=6, units="in", dpi=300)
rm(map1)


# Let's try hexabin to view the data!
rm(maphex)
maphex <- ggmap(googleterrain)
maphex <- maphex +
  stat_summary_hex(aes(data.map$longitude,  data.map$latitude, z = data.map$altitude))+
  #coord_equal() +
  theme_bw()+ scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "altitude") +
  ggtitle("Relief")

print(maphex)
ggsave("out/maphex.png", maphex, width=8, height=6,units="in", dpi=300)
