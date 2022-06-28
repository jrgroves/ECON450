#Income Distribution

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

vars = c("B17020_001","B17020_002", "B06011_001", "B19083_001")

acs<-get_acs(geography="state",
             variables = vars,
             year = 2020,
             geometry = TRUE)
acs.map<-acs %>%
  select(NAME, GEOID) %>%
  distinct()



acs.data<-acs %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(Med_Inc = B06011_001,
         Gini = B19083_001) %>%
  mutate(Poverty = B17020_002 / B17020_001) %>%
  select(GEOID, Poverty, Med_Inc, Gini) %>%
  filter(GEOID != "02") %>%
  filter(GEOID != "72") %>%
  filter(GEOID != "15")

acs.core<-merge(acs.map, acs.data, by="GEOID", all.y=TRUE)

ggplot(acs.core)+
  geom_sf(aes(fill = Gini))


b<-acs.core %>% 
  mutate(bin = cut_interval(acs.core$Gini, n = 6))
ggplot(b)+
  geom_sf(aes(fill = bin))
