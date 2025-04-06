# load libraries
library(RPostgres)
library(getPass)
library(reshape2)
library(dplyr)
library(ggplot2)

# connect to database
con <- dbConnect(Postgres(), user='postgres', password=getPass::getPass(), dbname='romhout')

# read series_with_series comparisons
series_with_series <- dbReadTable(con, "series_with_series")

series_sgc_melt <- series_with_series |>
  select("Series_A", "Radius_A", "Series_B", "Radius_B","SGC", "SSGC") |>
  mutate(GLK = SGC + SSGC/2) |>
  melt(id.vars=c("Series_A", "Radius_A", "Series_B", "Radius_B") ,
       measure.vars=c("SGC", "SSGC", "GLK"))

series_sgc_melt |>
  ggplot() + geom_boxplot(aes(x=value), fill="dodgerblue2") + facet_wrap(~variable, scales="free_x", ncol = 1)
ggsave("export/roman_data_sgc_boxplot.png")

#summary(series)

series_with_series_species <- dbGetQuery(con, "select \"Series_A\", \"Radius_A\", \"Series_B\", \"Radius_B\", \"SGC\", \"SSGC\",
(\"SGC\" + \"SSGC\"/2) as \"GLK\",
species_a.soort as species
from
series_with_series,
(select monstercode, soort from monsters) as species_a,
(select monstercode, soort from monsters) as species_b
where
\"Series_A\" = species_a.monstercode and
\"Series_B\" = species_b.monstercode and
species_a.soort = species_b.soort
")

series_species_sgc_melt <- series_with_series_species |>
  melt(id.vars=c("Series_A", "Radius_A", "Series_B", "Radius_B", "species") ,
       measure.vars=c("SGC", "SSGC", "GLK"))

series_species_sgc_melt |>
  ggplot() + geom_boxplot(aes(x=value, y=species), fill="dodgerblue2") + facet_wrap(~variable, scales="free_x", ncol = 1)
ggsave("export/roman_data_sgc_species_boxplot.png", width = 8, height = 8)

series_with_series_species |>
  group_by(species) |>
  summarise(mean_sgc = mean(SGC, na.rm = TRUE), sd_sgc = sd(SGC, na.rm = TRUE),
            mean_ssgc = mean(SSGC, na.rm = TRUE), sd_ssgc = sd(SSGC, na.rm = TRUE), max_sscg = max(SSGC, na.rm = TRUE),
            mean_glk= mean(GLK, na.rm = TRUE), sd_glk = sd(GLK, na.rm = TRUE), n = n())

series_with_series_species |>
  group_by(species) |>
  summarise(n = n(), n_ssgc_10 = sum(SSGC>0.1, na.rm = TRUE), prop_ssgc_10 = n_ssgc_10/n)
