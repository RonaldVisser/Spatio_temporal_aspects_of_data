# load libraries
library(RPostgres)
library(getPass)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# connect to database
con <- dbConnect(Postgres(), user='postgres', password=getPass::getPass(), dbname='romhout')

# read data from database
rings_year <- dbGetQuery(con, "Select datering, count(datering) as n from waardevol where waardevol.radiaal_code not in (\'Q\',\'X\',\'Y\')  group by datering;")
nr_series_measured <- dbGetQuery(con, "SELECT  metingen.jaar_van_meting as year_measurement, count(metingen.jaar_van_meting) as n,locatie.lab as laboratory FROM 	monsters, locatie, metingen
  WHERE locatie.ID = monsters.locatieid	and monsters.monstercode = metingen.monstercode and ne_jaar is not null and lab <> 'Trier'
  GROUP BY metingen.jaar_van_meting, locatie.lab;")
archaeology_nl <- read.csv("data/Archeologische_onderzoeksmeldingen.csv")

# plot number of tree rings per year (all radii)
ggplot(rings_year, aes(x=datering, y=n)) + geom_area(color="blue", fill="blue", alpha=0.2) +
  labs(x="Years", y = "Number of tree rings") +
  scale_x_continuous(breaks = c(-499,-249,0,250,500), labels = c("500 BC", "250 BC", "BC/AD", "250 AD", "500 AD"))
ggsave("export/number_tree_rings_year.png")

nr_series_measured %>%
  filter(year_measurement == 0 | is.na(year_measurement))
# 933 series Dendronet: measuring date not known

lab_colours <- brewer.pal(8, "Dark2")
lab_colours <- c(lab_colours[-6], "#f3a6b2")

# plot number of series per year measured
nr_series_measured %>%
  filter(year_measurement != 0) %>%
  ggplot(aes(x=year_measurement, y=n, fill = laboratory)) + geom_col() +
  scale_fill_manual(values = lab_colours, name = "Laboratory") +
  labs(x= "Year of measurement", y = "Number of radii measured")
ggsave("export/measurements_per_year_lab.png", width = 7)


# research per year: source data Archeologisch Informatiesysteem - Rijksdienst voor het Cultureel Erfgoed | De Erfgoedmonitor
archaeology_nl %>%
  filter(Year > 1979) %>%
  ggplot(aes(x=Year, y=Archeologische.onderzoeksmeldingen)) + geom_col() +
  labs(y= "Archaeological fieldwork projects")
ggsave("export/archaeological_research_nl.png")

# relation between number of tree ring series measured and number of archaeological studies in the Netherlands
ggarrange(
nr_series_measured %>%
  filter(laboratory == "RING" | laboratory == "BAAC") %>%
  group_by(year_measurement) %>%
  arrange(year_measurement) %>%
  mutate(n_total = sum(n))  %>%
  select(year_measurement, n_total) %>%
  ggplot(aes(x=year_measurement, y = n_total)) + geom_col() +
    labs(y = "Number of radii measured") +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(vjust = -2)) +
    scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010)),
archaeology_nl %>%
  filter(Year > 1984 & Year < 2013) %>%
  ggplot(aes(x=Year, y=Archeologische.onderzoeksmeldingen)) + geom_col() +
    labs(y= "Archaeological fieldwork projects") + scale_y_reverse() +
    scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010), position = "top") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank())
, nrow = 2, align = "v")
ggsave("export/tree_ring_research_vs_arch_research_nl.png", height = 8, width = 8)



