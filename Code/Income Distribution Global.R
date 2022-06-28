#Income Distribution

rm(list=ls())

library(tidyverse)
library(sf)

income<-read.csv(file="./Data/income.csv")
globe<-st_read("./Data/Globe/WB_countries_Admin0_10m.shp")
trans<-read.csv("./Data/trans.csv")

core.in<-income %>%
  select(Country.Name, X2017)

core.map<-globe %>%
  select(NAME_EN, ECONOMY)

core<-merge(core.map, trans, by="NAME_EN")
core<-merge(core, core.in, by="Country.Name")

b<-sum(core$X2017, na.rm=TRUE)

core2 <- core %>%
  select(!X:X.1) %>%
  mutate(share = X2017/b)

globe<-ggplot(core2)+
        geom_sf(aes(fill=share), colour = "white")+
        xlab("")+
        ylab("")+
        scale_fill_viridis_c(breaks = seq(0,.04,.005),
                             na.value = "white",
                             option = "inferno",
                             trans = "sqrt")+
        theme_bw()+
        theme(  axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank())

