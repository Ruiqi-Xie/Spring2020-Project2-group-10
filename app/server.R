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

server <- function(input, output,session) {
  filtered_tab1 = reactive({
    tab1_data %>%
      filter(neighbourhood_group %in% input$neighborhood) %>%
      filter(price.x >= input$price[1] & price.x <= input$price[2]) %>%
      filter(room_type %in% input$roomtype) %>%
      filter(start_date >= input$dateRange[1] & end_date <= input$dateRange[2])
  })
  
  output$plot <- renderLeaflet({
    tab1 = filtered_tab1()
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
      addTiles() %>%
      setView(lng = -74.00, lat = 40.71, zoom = 12) %>%
      addMarkers(
        lng = tab1$longitude,
        lat = tab1$latitude,
        popup = paste("ID:",tab1$id,"<br/>",
                      "Price:",tab1$price.x, "<br/>",
                      "Minimum nights:",tab1$minimum_nights.x, "<br/>"),
        clusterOptions = markerClusterOptions()) %>%
      addProviderTiles("CartoDB.Positron")
  })
  id_r <- reactive({input$id})
  cuisine_type_r <- reactive({input$cuisine_type})
  dist_r <- reactive({input$dist})
  output$rplot <- renderLeaflet({
    current_location <- airbnb %>% filter(id==id_r()) %>% dplyr::select(longitude,latitude)
    filtered_restaurant <- restaurant %>% filter(CUISINE.DESCRIPTION %in% cuisine_type_r())
    locations = as.matrix(cbind(filtered_restaurant$Longitude,filtered_restaurant$Latitude))
    distances = distHaversine(locations, current_location)
    data = filtered_restaurant[distances<dist_r()*1609.34,]
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = current_location$longitude, 
              lat= current_location$latitude, 
              zoom = 12) %>%
      addMarkers(lng = ~current_location$longitude,
                 lat = ~current_location$latitude, 
                 popup = "Chosen House",
                 icon = list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/unigrid-phantom-buildings-vol-1/60/008_041_home_apartment_house_building_3-512.png'
                             ,iconSize = c(25,25))
      ) %>%
      addCircles(lng = ~current_location$longitude,
                 lat = ~current_location$latitude, 
                 radius = dist_r()*1609.34) %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude, 
                 popup = paste(
                   "Name:", data$DBA, "<br>",
                   "Type:", data$CUISINE.DESCRIPTION
                 ),
                 icon=list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/food-solid-icons-volume-1/128/030-512.png'
                           ,iconSize = c(25,25)))%>%
      addProviderTiles("CartoDB.Positron")
    
    
  })
  # define reactive
  click_all_age_group_r <- reactive({input$click_all_age_group})
  click_no_age_group_r <- reactive({input$click_no_age_group})
  c.id_r <- reactive({input$c.id})
  added_makers_r <- reactive({input$added_makers})
  c.dist_r <- reactive({input$c.dist})
  
  observeEvent(click_all_age_group_r(), {
    updateCheckboxGroupInput(session, "added_makers",
                             choices = c("<18","18-24","25-44","45-64","65+","UNKNOWN"),
                             selected = c("<18","18-24","25-44","45-64","65+","UNKNOWN"))
  })
  observeEvent(click_no_age_group_r(), {
    updateCheckboxGroupInput(session, "added_makers",
                             choices = c("<18","18-24","25-44","45-64","65+","UNKNOWN"),
                             selected = NULL)
  })
  output$cplot <- renderLeaflet({
    c.current_location <- airbnb %>% filter(id==c.id_r()) %>% dplyr::select(longitude,latitude)
    filtered_crime <- crime %>% filter(VIC_AGE_GROUP %in% added_makers_r())
    c.locations = as.matrix(cbind(filtered_crime$Longitude,filtered_crime$Latitude))
    c.distances = distHaversine(c.locations, c.current_location)
    c.data = filtered_crime[c.distances<c.dist_r()*1609.34,]
    leaflet(c.data) %>%
      addTiles() %>%
      setView(lng = c.current_location$longitude, 
              lat= c.current_location$latitude, 
              zoom = 12) %>%
      addMarkers(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 popup = "Chosen House",
                 icon = list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/unigrid-phantom-buildings-vol-1/60/008_041_home_apartment_house_building_3-512.png'
                             ,iconSize = c(25,25))) %>%
      addCircles(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 radius = c.dist_r()*1609.34) %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude, 
                 popup = paste(
                   "Occur time: ", c.data$OCCUR_TIME, "<br>",
                   "Victim Gender:", c.data$VIC_SEX,"<br>",
                   "Victim Age:", c.data$VIC_AGE_GROUP
                 ),
                 icon=list(iconUrl = 'https://cdn0.iconfinder.com/data/icons/crime-investigation-basic-lineal-color/512/41_Revolver-512.png'
                           ,iconSize = c(25,25)),
                 clusterOptions = markerClusterOptions())%>%
      addProviderTiles("CartoDB.Positron")
    
    
  })
  output$cplot <- renderLeaflet({
    c.current_location <- airbnb %>% filter(id==input$c.id) %>% dplyr::select(longitude,latitude)
    filtered_crime <- crime %>% filter(VIC_AGE_GROUP %in% input$added_makers)
    c.locations = as.matrix(cbind(filtered_crime$Longitude,filtered_crime$Latitude))
    c.distances = distHaversine(c.locations, c.current_location)
    c.data = filtered_crime[c.distances<input$c.dist*1609.34,]
    leaflet(c.data) %>%
      addTiles() %>%
      setView(lng = c.current_location$longitude, 
              lat= c.current_location$latitude, 
              zoom = 12) %>%
      addMarkers(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 popup = "Chosen House",
                 icon = list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/unigrid-phantom-buildings-vol-1/60/008_041_home_apartment_house_building_3-512.png'
                             ,iconSize = c(25,25))) %>%
      addCircles(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 radius = input$c.dist*1609.34) %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude, 
                 popup = paste(
                   "Occur time: ", c.data$OCCUR_TIME, "<br>",
                   "Victim Gender:", c.data$VIC_SEX,"<br>",
                   "Victim Age:", c.data$VIC_AGE_GROUP
                 ),
                 icon=list(iconUrl = 'https://cdn0.iconfinder.com/data/icons/crime-investigation-basic-lineal-color/512/41_Revolver-512.png'
                           ,iconSize = c(30,30)),
                 clusterOptions = markerClusterOptions())%>%
      addProviderTiles("CartoDB.Positron")
    
    
  })
  # crime
  colors <- c("#74d2e7", "#2dde98", "#ffc168", "#ff6c5f", "#8e43e7")
  output$crime_pie <- renderPlotly({
    crime_pie.data <- crime %>%
      dplyr::group_by(BORO) %>%
      dplyr::summarize(n=n()) %>%
      arrange(BORO)
    plot_ly() %>% add_pie(data = crime_pie.data, 
                          labels = ~BORO, 
                          values = ~n,
                          name = names(crime_pie.data)[1],
                          domain = list(row = 0, column = 0),
                          marker = list(colors = colors))%>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$crime_hist <- renderPlotly({
    crime_hist.data <-
      crime %>%
      dplyr::mutate(month=month(OCCUR_DATE)) %>%
      dplyr::select(INCIDENT_KEY,BORO,month) %>%
      dplyr::group_by(month,BORO) %>%
      dplyr::arrange(BORO)
    plot_ly(crime_hist.data, x=~month, color=~BORO) %>%
      add_histogram() %>%
      layout(title="Histogram for crime in each month",
             xaxis = list(title="Month",zeroline=F),
             yaxis = list(title="Count",zeroline=F))%>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  #grade_pie
  # define reactive
  grade_type_r <- reactive({input$grade_type})
  cuisine_type.hist_r <- reactive({input$cuisine_type.hist})
  output$grade <- renderPlotly({
    restaurant_boro <- restaurant1 %>% 
      dplyr::select(DBA, BORO, CUISINE.DESCRIPTION, GRADE) %>% 
      dplyr::filter(GRADE != "", BORO != 0) %>%
      dplyr::filter(CUISINE.DESCRIPTION %in% c("Japanese","American","Spanish","Italian","Korean",
                                               "Chinese","French","Thai","Mexican","Indian")) %>%
      dplyr::filter(GRADE != "P",GRADE != "N", GRADE != "Z") %>%
      dplyr::group_by(BORO, GRADE) %>%
      dplyr::summarise(count=n())
    r_boro_a <- restaurant_boro %>%
      dplyr::filter(GRADE=="A")
    r_boro_b <- restaurant_boro %>%
      dplyr::filter(GRADE=="B")
    r_boro_c <- restaurant_boro %>%
      dplyr::filter(GRADE=="C")
    if(grade_type_r()=="A"){
      plot_ly() %>% add_pie(data = r_boro_a, 
                            labels = ~BORO, 
                            values = ~count,
                            name = names(restaurant_boro)[1],
                            domain = list(row = 0, column = 0),
                            marker = list(colors = colors))%>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
    }
    else if(grade_type_r()=="B"){
      plot_ly() %>% add_pie(data = r_boro_b, 
                            labels = ~BORO, 
                            values = ~count,
                            name = names(restaurant_boro)[1],
                            domain = list(row = 0, column = 0),
                            marker = list(colors = colors))%>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
      
    }
    else if(grade_type_r()=="C"){
      plot_ly() %>% add_pie(data = r_boro_c, 
                            labels = ~BORO, 
                            values = ~count,
                            name = names(restaurant_boro)[1],
                            domain = list(row = 0, column = 0),
                            marker = list(colors = colors))%>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
    }
  })
  filtered_tab3_area_1 = reactive({
    tab3_data_expensive %>%
      filter(neighbourhood_group == input$tab3_neighborhood) %>%
      select(id, price) 
  })
  output$area_plot_1  = renderPlotly({
    tab3_area_1 = filtered_tab3_area_1()
    a = ggplot(tab3_area_1, aes(x = id, y=price))+
      geom_bar(stat = "identity", fill = "#fd5c63") + 
      ggtitle("Top 10 expensive hourse's id") +
      coord_flip() +
      theme_classic() + 
      theme(axis.line = element_blank()) + 
      xlab("ID of house") +
      ylab("Price")
    fig_a = ggplotly(a) %>%
      layout(showlegend = FALSE, plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  filtered_tab3_area_2 = reactive({
    tab3_data_cheap %>%
      filter(neighbourhood_group == input$tab3_neighborhood) %>%
      select(id, price)
  })
  output$area_plot_2  = renderPlotly({
    tab3_area_2 = filtered_tab3_area_2()
    b = ggplot(tab3_area_2, aes(x = id, y=price))+
      geom_bar(stat = "identity", fill = "#fd5c63") + 
      ggtitle("Top 10 cheap hourse's id") +
      coord_flip() +
      theme_classic() + 
      theme(axis.line = element_blank()) + 
      xlab("ID of house") +
      ylab("Price")
    fig_b = ggplotly(b) %>% 
      layout(showlegend = FALSE, plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$price_neighborhood<- renderPlotly({
    ggplotly(priceTS) %>%
      layout(legend = list(bgcolor = "transparent",
                           bordercolor = "transparent")) %>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$price_roomtype<- renderPlotly({
    ggplotly(priceTS2) %>%
      layout(legend = list(bgcolor = "transparent",
                           bordercolor = "transparent")) %>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$restaurant_cuisine <- renderPlotly({
    restaurant_cuisine.data <-  restaurant1 %>% 
      dplyr::select(DBA, BORO, CUISINE.DESCRIPTION, GRADE) %>% 
      dplyr::filter(GRADE != "", BORO != 0) %>% 
      dplyr::select(BORO,CUISINE.DESCRIPTION) %>%
      dplyr::filter(CUISINE.DESCRIPTION %in% c("Japanese","American","Spanish","Italian","Korean",
                                               "Chinese","French","Thai","Mexican","Indian")) %>%
      dplyr::filter(CUISINE.DESCRIPTION %in% cuisine_type.hist_r()) %>%
      dplyr::group_by(CUISINE.DESCRIPTION,BORO) %>%
      dplyr::summarise(n=n())
    plot_ly(restaurant_cuisine.data, x=~BORO, y=~n,marker = list(color = "fd5c63")) %>%
      add_bars() %>%
      layout(title="The number of restaurant in each Boro",
             xaxis = list(title="BORO",zeroline=F),
             yaxis = list(title="Count",zeroline=F)) %>%
      layout(showlegend = FALSE, plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

