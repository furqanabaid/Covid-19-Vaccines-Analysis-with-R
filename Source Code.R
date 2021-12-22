#Covid-19 Vaccines Analysis
library(data.table)
library(plotly)
library(ggplot2)
library(ggthemes)
library(lambda.r)
library(lubridate)
library(stringr)
library(reticulate)
library(dplyr)
library(Hmisc)

#importing file and reading first 10 dows of data
file<-read.csv("country_vaccinations.csv")
x<-head(file,10)
x
#exploring this data 
sum<-summary(file)
sum
#Vaccines taken by countries
file$date <- mdy_hms(file$date)
df = sort(table(file$country))
View(head(df,10))
#removing duplicates
remove_countries = c('England','Northern Ireland','Scotland','Wales')
file <- file %>%
  filter (!country %in% remove_countries) 
dfrm = sort(table(file$country))
View(head(dfrm,10))
#Vaccines available in this dataset (Country Name, Location, No of Vaccines)
dfa = sort(table(file$vaccines), decreasing  = TRUE)
View(head(dfa,10))
#Available vaccine and the country
dfam <- file
dfam = dfam[c(13,1)]
head(dfam,10)
#Vaccines which are taking each of the country mentioned
DF$vaccines <- str_replace_all(dfam$vaccines, " ","")
vaccine_val<- unique(dfam$vaccines)
vaccine<- vector()
for (i in vaccine_val){
  for (j in strsplit(i, ",")){
    vaccine<- c(vaccine, j)
  }
}
vaccineArr<- unique(vaccine)
vaccineArr
arr<-data.frame(vaccineArr)
View(arr)
#World Map
x<-unique(file$country)
x
some.eu.countries <- c(x)
some.eu.maps <- map_data("world", region = some.eu.countries)
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 2, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")
