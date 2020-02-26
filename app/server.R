#### please set your word directory first!
setwd("../data")

packages.used=c("geosphere","dplyr","shiny", "tidyr","shinydashboard","plotly",
                "shinydashboard", "leaflet","shinyWidgets","stringr","ggplot2",
                "lubridate","packrat","rsconnect")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(geosphere)  ## for calculating the distance between lng, lat
library(dplyr)
library(shiny)
library(tidyr)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(stringr)
library(ggplot2)
library(lubridate)
library(plotly)    ## plotly
library(packrat)   ## for publish shiny
library(rsconnect) ## for publish shiny


#### read data
load(file = "tab1_data.RData")   ## tab1
airbnb = read.csv("AB_NYC_2019.csv")
airbnb = airbnb %>% dplyr::select(id,latitude,longitude)
restaurant1 = read.csv("restaurant.csv")
restaurant1 = restaurant1 %>% filter(as.numeric(str_sub(restaurant1$INSPECTION.DATE,-4,-1))>=2019) %>% 
  drop_na() %>%
  head(5000)
restaurant = restaurant1 %>% 
  dplyr::select(DBA, Latitude, Longitude, CUISINE.DESCRIPTION)
crime  = read.csv("crime.csv") %>% 
  mutate(OCCUR_DATE = as.character(OCCUR_DATE) %>% as.Date("%m/%d/%Y")) %>%
  filter(year(OCCUR_DATE)>=2019)


#### data preprocessing
### tab1_search
number_houses = tab1_data %>%
  group_by(id) %>%
  count()

number_hosts = length(unique(tab1_data$host_id))

number_reviews = tab1_data %>%
  group_by(id) %>%
  distinct(number_of_reviews) %>%
  summarise(reviews = sum(number_of_reviews))
number_reviews = sum(number_reviews$reviews)

### tab3_summary
## top expensive/cheap
tab3_data = tab1_data %>%
  select(id, neighbourhood_group, price.x) %>%
  group_by(id) %>%
  mutate(price = round(mean(price.x))) %>%
  select(id, neighbourhood_group, price)
tab3_data = distinct(tab3_data)

tab3_data_expensive = tab3_data %>%
  filter(price > 0) %>%
  group_by(neighbourhood_group) %>%
  arrange(desc(price)) %>%
  top_n(10, price)
tab3_data_expensive$id <- factor(tab3_data_expensive$id, levels = tab3_data_expensive$id[order(tab3_data_expensive$price)])


tab3_data_cheap = tab3_data %>%
  filter(price > 0) %>%
  group_by(neighbourhood_group) %>%
  arrange(desc(price)) %>%
  top_n(-10, price)
tab3_data_cheap$id <- factor(tab3_data_cheap$id, levels = tab3_data_cheap$id[order(-tab3_data_cheap$price)])


## price
data1=read.csv("analysisData.csv",header=TRUE, stringsAsFactors = FALSE)
data1$last_review = as.Date(data1$last_review)
data1$month<-month(data1$last_review)

data1_new=data1%>%
  dplyr::select(price,neighbourhood_group_cleansed,month)%>%
  rename(neighbourhood_group=neighbourhood_group_cleansed)

price_data<-  data1_new%>%
  group_by(neighbourhood_group,month) %>% 
  mutate(mean_price=as.numeric(mean(price)))%>%
  ungroup()%>%
  arrange(month)
priceTS <- ggplot(price_data, aes(month,mean_price, color = neighbourhood_group))+
  geom_line(aes(group = neighbourhood_group))+
  ggtitle("Price variability by time/neighborhood")+
  xlab("month")+
  scale_x_continuous(limits=c(1,12), breaks=seq(1,12,1))+
  guides(fill = guide_legend(title = "Neighbourhood group"))+
  theme_set(theme_bw())+
  theme(panel.grid.major=element_line(colour=NA))+
  theme_classic() + 
  theme(axis.line = element_blank()) + 
  theme(legend.position="bottom")

data2_new=data1%>%
  dplyr::select(price,room_type,month)
price_data2<-  data2_new%>%
  group_by(room_type,month) %>% 
  mutate(mean_price=as.numeric(mean(price)))%>%
  ungroup()%>%
  arrange(month)
priceTS2=ggplot(price_data2, aes(month,mean_price, color = room_type))+
  geom_line(aes(group = room_type))+
  ggtitle("Price variability by time/room_type")+
  xlab("month")+
  scale_x_continuous(limits=c(1,12), breaks=seq(1,12,1))+
  guides(fill = guide_legend(title = "room_type"))+
  theme_set(theme_bw())+
  theme(panel.grid.major=element_line(colour=NA)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) + 
  theme(legend.position="bottom")

