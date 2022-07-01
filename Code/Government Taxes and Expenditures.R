rm(list=ls())

library(tidyverse)
library(bea.R)

beaKey<-"1F30DEA8-AEBC-4A97-BF86-4DB1EF5BFD18"

#Government Employment####
userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T60400A',
                     'Year' = "ALL")

temp1 <- beaGet(userSpecList, asWide = FALSE)

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T60400B',
                     'Year' = "ALL")

temp2 <- beaGet(userSpecList, asWide = FALSE)


userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T60400C',
                     'Year' = "ALL")

temp3 <- beaGet(userSpecList, asWide = FALSE)


userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T60400B',
                     'Year' = "ALL")

temp4 <- beaGet(userSpecList, asWide = FALSE)

temp<-rbind(temp1, temp2, temp3, temp4)

emp<-temp %>%
  filter(LineNumber == 3 | LineDescription == "Government" |
           LineDescription == "Federal" | LineDescription == "State and local") %>%
  select(LineDescription, TimePeriod, DataValue) %>%
  rename(Desc = LineDescription,
         Year = TimePeriod,
         Value = DataValue) %>%
  group_by(Year, Desc) %>%
  distinct(Year, Desc, Value) %>%
  pivot_wider(names_from = Desc, values_from = Value) %>%
  rename(Private = 'Private industries',
         SandL = 'State and local')%>%
  mutate(Total = Private + Government,
         Year = as.numeric(Year))%>%
  pivot_longer(!Year, names_to = "Sector", values_to = "Employment") %>%
  mutate(Employmnet = Employment / 1000)

emp1<-emp %>%
  filter(Sector == "Private" | Sector == "Government" | Sector=="Total")

emp.plot1<-ggplot(emp1) +
                  geom_line(aes(Year, Employment/1000, color=Sector), size=1)+
                  scale_x_continuous(breaks = seq(1930,2000,10))+
                  scale_y_continuous(breaks = seq(0,150,25))+
                  ylab("Employment (In Thousands)")+
                  labs(title="Employment by Sector",
                       caption = "Source: BEA NIPA Table 6.4")+
                  theme_bw() 

emp2<-emp %>%
  filter(Sector != "Private" & Sector != "Total")


ggplot(emp2) +
  geom_line(aes(Year, Employment/1000, color=Sector), size=1)+
  scale_x_continuous(breaks = seq(1930,2000,10))+
  ylab("Employment (In Thousands)")+
  labs(title="Government Employment by Level",
       caption = "Source: BEA NIPA Table 6.4")+
  theme_bw()+
  scale_color_discrete(breaks=c("Government","Federal","SandL"),
                       labels = c("Governement","Federal","State and Local"))

rm(temp1, temp2, temp3, temp4, temp)

#GDP Values####

lines<-c(1, 22)

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T10106',
                     'Year' = "ALL")

temp <- beaGet(userSpecList, asWide = FALSE)

data<-temp %>%
  filter(LineNumber %in% lines) %>%
  select(LineDescription, DataValue, TimePeriod) %>%
  rename(Desc = LineDescription,
         Value = DataValue,
         Year = TimePeriod) %>%
  group_by(Year, Desc) %>%
  distinct(Year, Desc, Value) %>%
  pivot_wider(names_from = Desc, values_from = Value) %>%
  rename(GDP = 'Gross domestic product',
         Government = 'Government consumption expenditures and gross investment')%>%
  mutate(Year = as.numeric(Year))%>%
  pivot_longer(!Year, names_to = "Measures", values_to = "Dollars") %>%
  mutate(Dollars = Dollars / 1000000)


ggplot(data) +
  geom_line(aes(Year,Dollars, color=Measures), size=1)+
  ylab("Trillions of Chained Dollars (2012)")+
  scale_x_discrete(breaks = seq(1930,2000,10))+
  labs(title="GDP and Government Spending",
       caption = "Source: BEA NIPA Table 6.4")+
  theme_bw()

  

#Government Employment####

lines<-c(7,11,12,13,35,38,43,52,57,62,65,73,74,79,69,70,87,92)

l2 <- c(3, 87, 92)

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T60400D',
                     'Year' = 2020)

emp<-beaGet(userSpecList, asWide = FALSE)


data<-emp %>%
  filter(LineNumber %in% l2) %>%
  select(LineDescription, DataValue) %>%
  rename(Desc = LineDescription,
         Value = DataValue)

# Compute the position of labels
data <- data %>% 
  arrange(Desc) %>%
  mutate(prop = Value / sum(data$Value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic pie chart
ggplot(data, aes(x="", y=prop, fill=Desc)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")

#Government Expenditures by Function####


l.tot<-c(1,7,8,13,27,28,29,30,36)
l.fed <- c(43,48,49,54,67,68,69,70,74)
l.stl <- c(81,86,91,101,102,105,106,112)

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T31600',
                     'Year' = 2020)

exp.fun<-beaGet(userSpecList, asWide = FALSE)


data<-exp.fun %>%
  filter(LineNumber %in% l.tot) %>%
  select(LineDescription, DataValue) %>%
  rename(Function = LineDescription,
         Value = DataValue) %>%
  mutate(prop = Value / sum(data$Value) *100)%>%
  arrange(Function)


tot.exp.fun<-ggplot(data, aes(x="", y=prop, fill=Function)) +
                    geom_bar(stat="identity", width=1, color="white") +
                    coord_polar("y", start=0) +
                    theme_void() + 
                    theme(legend.position="right")+
                    ggtitle("Total Government Expenditure by Function")+
  scale_fill_brewer(palette="Set1")

data<-exp.fun %>%
  filter(LineNumber %in% l.fed) %>%
  select(LineDescription, DataValue) %>%
  rename(Function = LineDescription,
         Value = DataValue) %>%
  mutate(prop = Value / sum(data$Value) *100)%>%
  arrange(Function)


tot.exp.fun2<-ggplot(data, aes(x="", y=prop, fill=Function)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Expenditure by Function: Federal")+
  scale_fill_brewer(palette="Set1")

data<-exp.fun %>%
  filter(LineNumber %in% l.stl) %>%
  select(LineDescription, DataValue) %>%
  rename(Function = LineDescription,
         Value = DataValue) %>%
  add_row(Function = 'National Defense',  Value = 0) %>%
  mutate(prop = Value / sum(data$Value) *100) %>%
  arrange(Function)

tot.exp.fun3<-ggplot(data, aes(x="", y=prop, fill=Function)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Expenditure by Function: Federal")+
  scale_fill_brewer(palette="Set1")

#Government Receipts by Source####

l.tot<-c(3,4,5,6)
l.fed <- c(3,4,8,9)
l.stl <- c(3,6,11)

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'Q',
                     'TableName' = 'T30100',
                     'Year' = 2022)

rec<-beaGet(userSpecList, asWide = FALSE)

data<-rec %>%
  filter(LineNumber %in% l.tot) %>%
  select(LineDescription, DataValue) %>%
  rename(Source = LineDescription,
         Value = DataValue) %>%
  mutate(prop = (Value / sum(data$Value)) *100)%>%
  arrange(Source)


tot.tax.fun1<-ggplot(data, aes(x="", y=prop, fill=paste0(Source," ",round(prop, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Expenditure by Source: Total")+
  labs(caption = "Source: BEA NIPA Table 3.1")+
  scale_fill_brewer(palette="Set1", name = "Source")

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'Q',
                     'TableName' = 'T30200',
                     'Year' = 2022)

rec<-beaGet(userSpecList, asWide = FALSE)

data<-rec %>%
  filter(LineNumber %in% l.fed) %>%
  select(LineDescription, DataValue) %>%
  rename(Source = LineDescription,
         Value = DataValue) %>%
  mutate(prop = (Value / sum(data$Value)) *100)%>%
  arrange(Source)


tot.tax.fun2<-ggplot(data, aes(x="", y=prop, fill=paste0(Source," ",round(prop, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Expenditure by Source: Federal")+
  labs(caption = "Source: BEA NIPA Table 3.2")+
  scale_fill_brewer(palette="Set1", name = "Source")

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'Q',
                     'TableName' = 'T30300',
                     'Year' = 2022)

rec<-beaGet(userSpecList, asWide = FALSE)

data<-rec %>%
  filter(LineNumber %in% l.stl) %>%
  select(LineDescription, DataValue) %>%
  rename(Source = LineDescription,
         Value = DataValue) %>%
  add_row(Source = 'Taxes from the rest of the world',  Value = 0) %>%
  mutate(prop = (Value / sum(data$Value)) *100) %>%
  arrange(Source)

tot.tax.fun3<-ggplot(data, aes(x="", y=prop, fill=paste0(Source," ",round(prop, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Expenditure by Source: State and Local")+
  labs(caption = "Source: BEA NIPA Table 3.3")+
  scale_fill_brewer(palette="Set1", name = "Source")

#Source of Personal Taxes####
