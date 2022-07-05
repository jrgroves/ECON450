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

lines<-c(1)

userSpecList <- list('UserID' = beaKey , 
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableName' = 'T10105',
                     'Year' = "ALL")

temp <- beaGet(userSpecList, asWide = FALSE)

GDP<-temp %>%
  filter(LineNumber %in% lines) %>%
  select(LineDescription, DataValue, TimePeriod) %>%
  rename(Desc = LineDescription,
         Value = DataValue,
         Year = TimePeriod) %>%
  group_by(Year, Desc) %>%
  distinct(Year, Desc, Value) %>%
  pivot_wider(names_from = Desc, values_from = Value) %>%
  rename(GDP = 'Gross domestic product') %>%
  mutate(Year = as.numeric(Year))%>%
  pivot_longer(!Year, names_to = "Measures", values_to = "GDP") %>%
  mutate(Year = as.numeric(Year)) %>%
  select(!Measures)
  
  #Obtain Price Indices to turn current to Real
  userSpecList <- list('UserID' = beaKey ,
                       'Method' = 'GetData',
                       'datasetname' = 'NIPA',
                       'Frequency' = 'A',
                       'TableName' = 'T10104',
                       'Year' = 'ALL') 
  
  temp<-beaGet(userSpecList, asWide = FALSE)
  
  Index <- temp %>%
    filter(LineNumber == 1) %>%
    select(DataValue, TimePeriod) %>%
    rename(Index = DataValue,
           Year = TimePeriod) %>%
    mutate(Index = Index / 100,
           Year = as.numeric(Year))
  
  #Federal Data
  
  lines<-c(34,37,38,39,40,41,42)
  
  userSpecList <- list('UserID' = beaKey ,
                       'Method' = 'GetData',
                       'datasetname' = 'NIPA',
                       'Frequency' = 'A',
                       'TableName' = 'T30100',
                       'Year' = 'ALL') 
  
  temp<-beaGet(userSpecList, asWide = FALSE)
  
  data<-temp %>%
    filter(LineNumber %in% lines) %>%
    select(DataValue, LineDescription, TimePeriod) %>%
    rename(Dollars = DataValue,
           Source = LineDescription,
           Year = TimePeriod) %>%
    pivot_wider(names_from = Source, values_from = Dollars) %>%
    rename(Capital = 'Less: Consumption of fixed capital',
           Receipts = 'Total receipts') %>%
    mutate(Capital = Capital * (-1),
           Expenditure = rowSums(across('Current expenditures':Capital), na.rm = TRUE),
           Year = as.numeric(Year)) %>%
    select(Year, Receipts, Expenditure) %>%
    pivot_longer(!Year, names_to = "Source", values_to = "Dollars")  %>%
    full_join(Index, by="Year") %>%
    mutate(Total_Dollars_Real = Dollars / Index) %>%
    select(!Index)
    
  core<-data %>%
    full_join(GDP, by = "Year")
  
  #Federal Data
  lines<-c(40, 43, 44, 45, 46, 47, 48)
  
  userSpecList <- list('UserID' = beaKey ,
                       'Method' = 'GetData',
                       'datasetname' = 'NIPA',
                       'Frequency' = 'A',
                       'TableName' = 'T30200',
                       'Year' = 'ALL') 
  
  temp<-beaGet(userSpecList, asWide = FALSE)
  
  data<-temp %>%
    filter(LineNumber %in% lines) %>%
    select(DataValue, LineDescription, TimePeriod) %>%
    rename(Dollars = DataValue,
           Source = LineDescription,
           Year = TimePeriod) %>%
    pivot_wider(names_from = Source, values_from = Dollars) %>%
    rename(Capital = 'Less: Consumption of fixed capital',
           Receipts = 'Total receipts') %>%
    mutate(Capital = Capital * (-1),
           Expenditure = rowSums(across('Current expenditures':Capital), na.rm = TRUE),
           Year = as.numeric(Year)) %>%
    select(Year, Receipts, Expenditure) %>%
    pivot_longer(!Year, names_to = "Source", values_to = "Dollars.Fed")  %>%
    full_join(Index, by="Year") %>%
    mutate(Total_Dollars.Fed_Real = Dollars.Fed / Index) %>%
    select(!Index)
  
  core<-core %>%
    full_join(data, by = c("Year", "Source"))
              
  #State and Local Data
  lines<-c(35, 39, 40, 41, 42, 43)
  
  userSpecList <- list('UserID' = beaKey ,
                       'Method' = 'GetData',
                       'datasetname' = 'NIPA',
                       'Frequency' = 'A',
                       'TableName' = 'T30300',
                       'Year' = 'ALL') 
  
  temp<-beaGet(userSpecList, asWide = FALSE)
  
  data<-temp %>%
    filter(LineNumber %in% lines) %>%
    select(DataValue, LineDescription, TimePeriod) %>%
    rename(Dollars = DataValue,
           Source = LineDescription,
           Year = TimePeriod) %>%
    pivot_wider(names_from = Source, values_from = Dollars) %>%
    rename(Capital = 'Less: Consumption of fixed capital',
           Receipts = 'Total receipts') %>%
    mutate(Capital = Capital * (-1),
           Expenditure = rowSums(across('Current expenditures':Capital), na.rm = TRUE),
           Year = as.numeric(Year)) %>%
    select(Year, Receipts, Expenditure) %>%
    pivot_longer(!Year, names_to = "Source", values_to = "Dollars.SaL")  %>%
    full_join(Index, by="Year") %>%
    mutate(Total_Dollars.SaL_Real = Dollars.SaL / Index) 
  
  core<-core %>%
    full_join(data, by = c("Year", "Source"))
  
  #Graphs
  
  ggplot(core) +
    geom_line(aes(x = Year, y = GDP/1000000, color = "GDP"), size = 1) +
    geom_line(aes(x = Year, y=Total_Dollars_Real/1000000, color = Source), size = 1)+
    labs(title = "Real GDP and Governmnet Spending and Receipts",
         caption = "Sourc: BEA NIPA",
         color = "")+
    theme_bw() +
    ylab("Dollars (in Trillions)")
  
  
  ggplot(subset(core, Source=="Receipts")) +
    geom_line(aes(x = Year, y = Total_Dollars_Real/1000000, color = "All Government"), size = 1) +
    geom_line(aes(x = Year, y = Total_Dollars.Fed_Real/1000000, color = "Federal"), size = 1) +
    geom_line(aes(x = Year, y = Total_Dollars.SaL_Real/1000000, color = "State and Local"), size = 1)  +
    labs(title = "Government Real Receipts by Level",
         caption = "Sourc: BEA NIPA",
         color = "")+
    theme_bw() +
    ylab("Dollars (in Trillions)")
              
  ggplot(subset(core, Source=="Expenditure")) +
    geom_line(aes(x = Year, y = Total_Dollars_Real/1000000, color = "All Government"), size = 1) +
    geom_line(aes(x = Year, y = Total_Dollars.Fed_Real/1000000, color = "Federal"), size = 1) +
    geom_line(aes(x = Year, y = Total_Dollars.SaL_Real/1000000, color = "State and Local"), size = 1)  +
    labs(title = "Government Real Expenditures by Level",
         caption = "Sourc: BEA NIPA",
         color = "")+
    theme_bw() +
    ylab("Dollars (in Trillions)")              
  
  core<-core %>%
    mutate(Tot.GDP = Total_Dollars_Real / GDP,
           Fed.GDP = Total_Dollars.Fed_Real / GDP,
           SaL.GDP = Total_Dollars.SaL_Real / GDP,
           across(Tot.GDP:SaL.GDP, ~.x/10))
  
  
  ggplot(subset(core, Source=="Receipts")) +
    geom_line(aes(x = Year, y = Tot.GDP, color = "Total"), size = 1) +
    geom_line(aes(x = Year, y = Fed.GDP, color = "Federal"), size = 1) +
    geom_line(aes(x = Year, y = SaL.GDP, color = "State and Local"), size = 1) +
    labs(title = "Government Receipts as Share of Real GDP",
         caption = "Sourc: BEA NIPA",
         color = "")+
    theme_bw() +
    ylab("Share of Real GDP")     
  
  
  
  ggplot(subset(core, Source=="Expenditure")) +
    geom_line(aes(x = Year, y = Tot.GDP, color = "Total"), size = 1) +
    geom_line(aes(x = Year, y = Fed.GDP, color = "Federal"), size = 1) +
    geom_line(aes(x = Year, y = SaL.GDP, color = "State and Local"), size = 1) +
    labs(title = "Government Expenditures as Share of Real GDP",
         caption = "Sourc: BEA NIPA",
         color = "")+
    theme_bw() +
    ylab("Share of Real GDP")    
  
  ggplot(subset(core, Source=="Expenditure")) +
    geom_line(aes(x = Year, y = Tot.GDP, color = "Total Rec."), size = 1) +
    geom_line(aes(x = Year, y = Fed.GDP, color = "Federal Rec."), size = 1) +
    geom_line(aes(x = Year, y = SaL.GDP, color = "State and Local Rec"), size = 1) +
    labs(title = "Government Expenditures as Share of Real GDP",
         caption = "Sourc: BEA NIPA",
         color = "")+
    theme_bw() +
    ylab("Share of Real GDP")   
  

#Government Expenditures by Function####

l.tot<-c(2,7,8,13,27,28,29,30,36)
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
  mutate(prop = Value / sum(Value))%>%
  arrange(Function)


tot.exp.fun<-ggplot(data, aes(x="", y=prop, fill=paste0(Function," ",round(prop*100, 1),"%"))) +
                    geom_bar(stat="identity", width=1, color="white") +
                    coord_polar("y", start=0) +
                    theme_void() + 
                    theme(legend.position="right")+
                    labs(title = "Total Government Expenditure by Function",
                         caption = "Source: BEA NIPA")+
                    scale_fill_brewer(palette="Set1", name = "Function")

data<-exp.fun %>%
  filter(LineNumber %in% l.fed) %>%
  select(LineDescription, DataValue) %>%
  rename(Function = LineDescription,
         Value = DataValue) %>%
  mutate(prop = Value / sum(Value))%>%
  arrange(Function)


tot.exp.fun2<-ggplot(data, aes(x="", y=prop, fill=paste0(Function," ",round(prop*100, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right")+
  labs(title = "Government Expenditure by Function: Federal",
       caption = "Source: BEA NIPA")+
  scale_fill_brewer(palette="Set1", name = "FUnction")

data<-exp.fun %>%
  filter(LineNumber %in% l.stl) %>%
  select(LineDescription, DataValue) %>%
  rename(Function = LineDescription,
         Value = DataValue) %>%
  add_row(Function = 'National Defense',  Value = 0) %>%
  mutate(prop = Value / sum(Value)) %>%
  arrange(Function)

tot.exp.fun3<-ggplot(data, aes(x="", y=prop, fill=paste0(Function," ",round(prop*100, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Expenditure by Function: State and Local")+
  scale_fill_brewer(palette="Set1", name = "Function")

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
  mutate(Total = sum(data$Value),
         prop = (Value / Total)) %>%
  arrange(Source)


tot.tax.fun1<-ggplot(data, aes(x="", y=prop, fill=paste0(Source," ",round(prop*100, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Receipts by Source: Total")+
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
  mutate(Total = sum(data$Value),
         prop = (Value / Total)) %>%
  arrange(Source)


tot.tax.fun2<-ggplot(data, aes(x="", y=prop, fill=paste0(Source," ",round(prop*100, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Receipts by Source: Federal")+
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
  mutate(Total = sum(data$Value),
         prop = (Value / Total)*100 ) %>%
  arrange(Source)

tot.tax.fun3<-ggplot(data, aes(x="", y=prop, fill=paste0(Source," ",round(prop, 1),"%"))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void() + 
  theme(legend.position="right")+
  ggtitle("Government Recepts by Source: State and Local")+
  labs(caption = "Source: BEA NIPA Table 3.3")+
  scale_fill_brewer(palette="Set1", name = "Source")


