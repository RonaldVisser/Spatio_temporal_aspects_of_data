# load libraries
library(RPostgres)
library(getPass)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# connect to database
con <- dbConnect(Postgres(), user='postgres', password=getPass::getPass(), dbname='romhout')

sites <- dbGetQuery(con, "Select id, objecttype from locatie where id <> 'XXX';")
site_types <- read.csv("data/site_types.csv")
sites <- sites %>%
  left_join(site_types, by = join_by(objecttype == Nederlands))

sites %>%
  ggplot(aes(x=English.category)) + geom_bar() + coord_flip() +
  labs(x="", y="Number of sites")
ggsave("export/site_categories.png")


site_n_samples <- dbGetQuery(con, "Select locatieid, count(monstercode) as n from monsters where locatieid <> 'XXX' group by locatieid;")

sites %>%
  left_join(site_n_samples, by = join_by(id == locatieid)) %>%
  ggplot(aes(x=English.category, y = n)) + geom_col() + coord_flip() +
  labs(x="", y="Number of samples")
ggsave("export/site_categories_samples.png")

ggarrange(
  sites %>%
    ggplot(aes(x=English.category)) + geom_bar() + coord_flip() +
    labs(x="", y="Number of sites") + scale_y_reverse() +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank() )
  ,
  sites %>%
    left_join(site_n_samples, by = join_by(id == locatieid)) %>%
    ggplot(aes(x=English.category, y = n)) + geom_col() + coord_flip() +
    labs(x="", y="Number of samples") + theme(axis.text.y=element_text(hjust=0.5), axis.ticks.y = element_blank())
  , align = "v", ncol = 2)
ggsave("export/sites_categories.png", width = 10)
