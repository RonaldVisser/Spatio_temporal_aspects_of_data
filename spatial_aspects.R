# load libraries
library(RPostgres)
library(getPass)
library(sf)
library(rpostgis)
library(ggplot2)
library(reshape2)
library(dplyr)
library(stringr)
library(ggpubr)

# connect to database
con <- dbConnect(Postgres(), user='postgres', password=getPass::getPass(), dbname='romhout')
con_soil <- dbConnect(Postgres(), user='postgres', password=getPass::getPass(), dbname='eu_soil')

location <- rpostgis::pgGetGeom(con, query = "Select id, land, plaats, toponym, objecttype, \"UTM_point\" as geom from locatie where id <> 'XXX'")
# export locations as GeoPackage
st_write(location, "data/location.gpkg", append = FALSE)
waterways <- rpostgis::pgGetGeom(con, geom = "the_geom", name = (c("public","Waterlopen_UTM31_ED50_Zone31N")))
sea <- rpostgis::pgGetGeom(con, geom = "the_geom", name = (c("public","EuropeSea")))

landuse <- pgGetGeom(con_soil, geom = "utm_geom", name = (c("public","SGDB_PTR_small")))
env_zone <- st_read("data/enz_v8_23031.gpkg")

#calculate distances between lines and points
distance_river <- as.data.frame(st_distance(location$geom, waterways$geom))
# set row and columnnames
row.names(distance_river) <- location$id
colnames(distance_river) <- waterways$ID
# minimal distance to water
distance_river_min <- as.data.frame(apply(distance_river, 1, FUN = min))
colnames(distance_river_min) <- "distance_river"
# minimal distance to water
distance_sea <- as.data.frame(as.vector(st_distance(location$geom, sea$geom)))
row.names(distance_sea) <- location$id
colnames(distance_sea) <- "distance_sea"

distances <- cbind(distance_river_min, distance_sea)

distances$distance_min <- pmin(distances$distance_river,distances$distance_sea)

distances %>%
  ggplot(aes(x=distance_min/1000)) + geom_histogram() +
  labs(x="Minimal distance to water (km)")

water_dist_violin <- distances %>%
  ggplot(aes(x=distance_min/1000)) +
  #geom_boxplot(color="blue", fill="blue", alpha=0.2, notch=TRUE, notchwidth = 0.8,) +
  geom_violin(aes(y=(rep(0,265))), color="blue", fill="blue", alpha=0.2) +
  geom_dotplot(aes(y=(rep(0,265))), binwidth = 0.5, stackdir = "center", dotsize = 0.3) +
  labs(x="Minimal distance to water (km)", y="") +
  theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())
ggsave("export/minimal_distance_water_violin.png", water_dist_violin)


location_soil_db <- st_intersection(location,landuse)

location_land_use_bar <- location_soil_db %>%
  ggplot(aes(x=(factor(USEDO)))) + geom_bar() +
  scale_x_discrete(labels= c("No information", "Pasture, grassland, grazing land", "Arable land, cereals", "Forest, coppice", "Horticulture", "Moor")) +
  labs(title = "Dominant land use", x="", y="Count") + coord_flip()
ggsave("export/location_land_use_bar.png", location_land_use_bar)

location_subsoil_water_bar <- location_soil_db %>%
  ggplot(aes(x=(factor(AWC_SUB, levels = c("#", "L", "M", "H", "VH"))))) + geom_bar() +
  scale_x_discrete(labels= c("No information", "Low ( < 100 mm/m)", "Medium (100 - 140 mm/m)",  "High (140 - 190 mm/m)", "Very high ( > 190 mm/m)")) +
  labs(title="Subsoil available water capacity", x="", y="Count") + coord_flip()
ggsave("export/location_subsoil_water_bar.png", location_subsoil_water_bar)


location_texture_bar <- location_soil_db %>%
  ggplot(aes(x=(factor(TEXT)))) + geom_bar() +
  scale_x_discrete(labels= c("No information", "Coarse (clay < 18 % and sand > 65 %)",
                             str_wrap("Medium (18% < clay < 35% and sand > 15%, or clay < 18% and 15% < sand < 65%)",40),
                             "Medium fine (clay < 35 % and sand < 15 %)",
                             "Fine (35 % < clay < 60 %)",
                             "No texture (because of organic layer)")) +
  labs(title ="Texture", x="", y="Count") + coord_flip()
ggsave("export/location_texture_bar.png", location_texture_bar)

location_altitude <- location_soil_db %>%
  ggplot(aes(x=factor(ALT, levels = c("#", "L", "H")))) + geom_bar() +
  scale_x_discrete(labels= c("No information", "Lowlands & intermediate", "Uplands & mountains")) +
  labs(title="Altitude", x="", y="Count") + coord_flip()
ggsave("export/location_altitude.png", location_altitude)

ggarrange(location_texture_bar, location_subsoil_water_bar, location_land_use_bar, location_altitude, align = "v")
ggsave("export/location_soil_water_land_altitude.png", width = 12, height = 8)

env_zone <- st_transform(env_zone, "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs")
location_env_zone <- st_intersection(location,env_zone)

location_env_zone %>%
  ggplot(aes(x=factor(EnZ))) + geom_bar() +
  scale_x_discrete(labels= c("Atlantic North", "Continental", "Atlantic Central", "Pannonian")) +
  labs(x="", y="Count") + coord_flip()
ggsave("export/environmental_zone_bar.png")


combined_data <- cbind(distances, location_landuse, location_env_zone)

water_dist_landuse_violin <- combined_data %>%
  select(USEDO, distance_min) %>%
  ggplot(aes(x=factor(USEDO), y=distance_min/1000)) +
  geom_violin(color="blue", fill="blue", alpha=0.2) +
  scale_x_discrete(labels= c("No information", "Pasture, grassland, grazing land", "Arable land, cereals", "Forest, coppice", "Horticulture", "Moor")) +
  labs(x="Domininant land use", y="Minimal distance to water (km)") +
  coord_flip()
ggsave("export/location_land_use_waterdistances_violin.png", water_dist_landuse_violin)

#plot(st_collection_extract(st_voronoi(st_combine(location))))

