# load libraries
library(RPostgres)
library(getPass)
library(rpostgis)
library(sf)
library(dplyr)
library(ggplot2)
#library(RColorBrewer)
library(ggpubr)
library(ggspatial)

# connect to database
con <- dbConnect(Postgres(), user='postgres', password=getPass::getPass(), dbname='romhout')

# read data from database
sites_dates <- pgGetGeom(con, query = "select locatie.id, plaats as place, st_setsrid(locatie.\"UTM_point\", 23031) as geom, min(\"1e_jaar\") as earliest, max(ne_jaar) as latest
from locatie, monsters, metingen where locatie.id = monsters.locatieid and monsters.monstercode = metingen.monstercode and id <> 'XXX'
group by id, plaats order by id")

europe <- rpostgis::pgGetGeom(con, geom = "the_geom", name = (c("public","Europa_UTM31_ED50_Zone31N")))

st_write(sites_dates, "data/sites_dates.gpkg", append = FALSE)

sites_dates <- st_read("data/sites_dates.gpkg")

centuries <- seq(-100,500,100)
centuries_half <- seq(-100,500,50)

ggplot(europe) + geom_sf() + geom_sf(data = sites_dates) + theme_bw() +
  coord_sf(xlim = c(st_bbox(sites_dates)[1], st_bbox(sites_dates)[3]),
           ylim = c(st_bbox(sites_dates)[2], st_bbox(sites_dates)[4])) +
  annotation_north_arrow(location = "tl", height = unit(.5, "cm"), width = unit(.5, "cm")) +
  annotation_scale()
# if error check sf_proj_search_paths() for different proj-versions


for (i in centuries) {
  sites <- sites_dates %>% filter(latest <= i & latest > (i-100) )
  if(i<0) {
    end_yr <- paste0(abs(i), " BC")
    start_yr <- paste0(abs(i-100), " BC")
  } else if(i==0) {
    start_yr <- paste0(abs(i-100), " BC")
    end_yr <- "BC-AD"
  } else if (i==100) {
    start_yr <- "BC-AD"
    end_yr <- paste0("AD ", i)
  } else {
      start_yr <- paste0("AD ", i-100)
      end_yr <- paste0("AD ", i)
    }
  assign(paste0("map_100_", i), ggplot(europe) + geom_sf() + geom_sf(data = sites) + theme_bw() +
    coord_sf(xlim = c(st_bbox(sites_dates)[1], st_bbox(sites_dates)[3]),
             ylim = c(st_bbox(sites_dates)[2], st_bbox(sites_dates)[4])) +
    annotation_north_arrow(location = "tl", height = unit(.5, "cm"), width = unit(.5, "cm")) +
    annotation_scale() + labs(title= paste0("From ", start_yr, " to ", end_yr))  +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_line(colour = "transparent"),
          plot.title = element_text(size = 12))
          )
  ggsave(paste0("export/maps_spatio_temp_100/spatio_temporal_", gsub(" ", "_", start_yr), "_to_", gsub(" ", "_", end_yr), ".png"), get(paste0("map_100_", i)))
}

ggarrange(`map_100_-100`, map_100_0, map_100_100, map_100_200, map_100_300, map_100_400, map_100_500, nrow = 3, ncol = 3)
ggsave("export/maps_spatio_temp_100/spatio_temporal_overview.png", width = 6, height = 10, dpi = 600)

for (i in centuries_half) {
  sites <- sites_dates %>% filter(latest <= i & latest > (i-50) )
  if(i<0) {
    end_yr <- paste0(abs(i), " BC")
    start_yr <- paste0(abs(i-50), " BC")
  } else if(i==0) {
    start_yr <- paste0(abs(i-50), " BC")
    end_yr <- "BC-AD"
  } else if (i==50) {
    start_yr <- "BC-AD"
    end_yr <- paste0("AD ", i)
  } else {
    start_yr <- paste0("AD ", i-50)
    end_yr <- paste0("AD ", i)
  }
  assign(paste0("map_50_", i), ggplot(europe) + geom_sf() + geom_sf(data = sites) + theme_bw() +
           coord_sf(xlim = c(st_bbox(sites_dates)[1], st_bbox(sites_dates)[3]),
                    ylim = c(st_bbox(sites_dates)[2], st_bbox(sites_dates)[4])) +
           annotation_north_arrow(location = "tl", height = unit(.5, "cm"), width = unit(.5, "cm")) +
           annotation_scale() + labs(title= paste0("From ", start_yr, " to ", end_yr))  +
           theme(axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 panel.grid.major = element_line(colour = "transparent"),
                 plot.title = element_text(size = 12))
         )
  ggsave(paste0("export/maps_spatio_temp_50/spatio_temporal_", gsub(" ", "_", start_yr), "_to_", gsub(" ", "_", end_yr), ".png"), get(paste0("map_50_", i)))
}

ggarrange(`map_50_-100`, `map_50_-50`, map_50_0, map_50_50, map_50_100, map_50_150, map_50_200, map_50_250, map_50_300, map_50_350, map_50_400, map_50_450, map_50_500, nrow = 5, ncol = 4)
ggsave("export/maps_spatio_temp_50/spatio_temporal_overview.png", width = 7, height = 14, dpi = 600)




