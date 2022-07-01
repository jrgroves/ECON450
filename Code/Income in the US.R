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
  geom_sf(aes(fill = Med_Inc))+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Median Income by State",
       fill = "Median Income",
       caption = "Source: ACS 2020 - 5 Year Data")+
  scale_fill_gradient2(
    low = "red", 
    mid = "white", 
    high = "blue", 
    midpoint = 32691,
    limits=c(25261, 52328))


ggplot(acs.core)+
  geom_sf(aes(fill = Gini))+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Gini Coefficient by State",
       fill = "Gini Coefficient",
       caption = "Score: ACS 2020 - 5 Year Data")+
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0.4664,
    limits=c(0.40, 0.55))

ggplot(acs.core)+
  geom_sf(aes(fill = Poverty))+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Percentage of Population Below Poverty Line",
       fill = NULL,
       caption = "Score: ACS 2020 - 5 Year Data")+
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0.12709,
    limits=c(0.05, 0.20))

