### build dashboard

header <- dashboardHeader(
  title = tags$img(src="airbnb logo.jpg"),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "10 people are looking at the same house",
      icon("users")
    ),
    notificationItem(
      text = "The rental price is decreasing!",
      icon("dollar-sign")
    )
  )                                                     
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Search", tabName = "search", icon = icon("search-location")),
    menuItem("Environment", tabName = "crime_resturant", icon = icon("utensils")),
    menuItem("Overview", tabName = "descriptive", icon = icon("chart-line")),
    menuItem("References", tabName = "references", icon = icon("th"))
  )
)

body <- dashboardBody(
  ## set the color of header
  tags$head(tags$style(HTML('/* logo */
                                .skin-blue .main-header .logo {
                                background-color: #fd5c63;
                                }
                            /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #fd5c63;
                                }
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #fd5c63;
                            }
                            /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #fd5c63;
                            }
                            /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #f8f9f9;
                            }
                            /* body */
                                .content-wrapper, .right-side {
                                background-color: #f8f9f9;
                            }
                            /*    Move everything below the header */
                            .content-wrapper {
                            margin-top: 100px;
                            }
                            .content {
                            padding-top: 50px;
                            }
                            /*    Format the title/subtitle text */
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 50%;
                            left: 50%;
                            transform:translate(-50%, -50%);
                            }
                            @media (max-width: 590px) {
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 10%;
                            left: 10%;
                            transform:translate(-5%, -5%);
                            }
                            }
                            @media (max-width: 767px) {
                            .primary-title {
                            font-size: 1.1em;
                            }
                            .primary-subtitle {
                            font-size: 1em;
                            }
                            }
                            /*    Make the image taller */
                            .main-header .logo {
                             height: 190px;
                              }
                            /*    Override the default media-specific settings */
                            @media (max-width: 5000px) {
                            .main-header {
                            padding: 0 0;
                            position: relative;
                            }
                            .main-header .logo,
                            .main-header .navbar {
                            width: 100%;
                            float: none;
                            }
                           .main-header .navbar {
                           margin: 0;
                           }
                           .main-header .navbar-custom-menu {
                          float: right;
                          }
                          }
                          /*    Move the sidebar down */
                          .main-sidebar {
                          position: absolute;
                          }
                          .left-side, .main-sidebar {
                          padding-top: 250px;
                          }'
  ))),
  tabItems(
    tabItem(
      tabName = "home",
      fluidPage(
        tags$style(
          HTML('
          .box.box-solid.box-primary>.box-header {
            color:#fff;
              background:#fd5c63
          }
          
          .box.box-solid.box-primary{
            border-bottom-color:#fd5c63;
              border-left-color:#fd5c63;
              border-right-color:#fd5c63;
              border-top-color:#fd5c63;
          }'
          )
        ),
        fluidRow(
          box(width = 15, title = "Introduction", status = "primary",
              solidHeader = TRUE, 
              h3("Airbnb Exploration - Help you find the ideal house"),
              h4("Airbnb has become one of the most popular platform for people to list, discover, and book accommodations around the world. People can find various types of houses through airbnb app. However, users can only see the internal pictures, the price per night, and the overall introduction of the house. They cannot clearly know the environment of the house like crime and restaurant situation. Also, users cannot search for multiple places at one time."),
              h4("Our shiny app is based on airbnb houses in NYC. It aims to help airbnb users who are totally new to NYC, and want to know more about their ideal houses and the rental situation in NYC. They can check the crime situation, restaurants around, the average price of different boroughs, the most expensive house in each area, etc."), 
              h4("Let's explore this app!"))),
        fluidRow(
          box(width = 15, title = "User Guide", status = "primary",
              solidHeader = TRUE,
              h3("How to use this app?"),
              tags$div(tags$ul(
                tags$li("Search: This part is our search map and it contains four filters: borough, price range, room type, and date range. For each house users are interested in, they can know the house id, price per night, and minimum nights by click on the house marker. Also, users can copy the ID to the second part - environment to explore the crime and restaurant of the house. On the top of the map, people can also see the overall airbnb booking situation in NYC, which could motivate users to order."),
                tags$li("Environment: Users can search nearby restaurant and crime situation based on the distance range. They can even choose their favoriate cuisine type or explore the crime details like victim age group. This can provide them with a clear understanding of around situation and help them make the decision."),
                tags$li("Overview: The tab has several graphs to give users a board overview of rental situation in NYC. Some tourists might feel shocked when they see the rental price of NYC. Therefore, we provide the price fluctuation across different boroughs and room type. Moreover, the total number of houses distribution by areas are given. Based on customers' preference, we also offer the top 10 expensive and cheap houses' ids for them to check.")
              ))
          )
        ),
        fluidRow(
          tags$img(
            src = "new york2.jpg",
            width = "100%"
          )
        )
      )
    ),
    tabItem(
      tabName = "search",
      fluidPage(
        fluidRow(
          valueBox(
            value = dim(number_houses)[1],
            subtitle = "houses are left",
            icon = icon("home"),
            color = "light-blue",
            width = 4
          ),
          valueBox(
            value = round(mean(tab1_data$price.x),0),
            subtitle = "per night on average",
            icon = icon("dollar-sign"),
            color = "aqua",
            width = 4
          ),
          valueBox(
            value = number_reviews,
            subtitle = "reviews for your reference",
            icon = icon("comment-dots"),
            color = "teal",
            width = 4
          )),
        fluidRow(
          column(2,
                 checkboxGroupInput("neighborhood", "House area",
                                    c("Bronx" = "Bronx",
                                      "Brooklyn" = "Brooklyn",
                                      "Manhattan" = "Manhattan",
                                      "Queens" = "Queens",
                                      "Staten Island" = "Staten Island")),
                 sliderInput("price", label = h3("Price range"), min = 0, max = 10000, 
                             value = c(100,500)),
                 checkboxGroupInput("roomtype", label = h3("Room type"),
                                    choices=list("Entire home/apartment" = "Entire home/apt", "Private room" = "Private room",
                                                 "Shared room" = "Shared room"), selected = "Private room"),
                 dateRangeInput('dateRange',
                                label = 'Date range',
                                start = "2020-02-28", end = "2020-06-30"),
                 submitButton("Submit",width='100%'),div()),
          column(10, leafletOutput("plot",height="800px"),div())
        )
      )
    ),
    tabItem(
      tabName = "crime_resturant",
      fluidPage(
        navbarPage(
          "The Surroundings",
          tabPanel(
            "Restaurant",
            fluidRow(
              titlePanel(h3("Do you wanna know the restaurants near your house?")),
              br(),
              br(),
              column(2,wellPanel(
                br(),
                numericInput("id", h4("House_id:"),value = 2539),
                sliderInput(inputId="dist",h4("Distance Range:"), min = 0, max=5, value=2.5,step=0.5),
                selectInput("cuisine_type",h4("Cuisine Type"),choices=c("Japanese","American","Spanish","Italian","Korean","Chinese","French","Thai","Mexican","Indian"),multiple = F, selected = "American"),
                submitButton("Submit",width='100%'),
                style="background-color: #ffffff"
              )),
              column(10,leafletOutput("rplot",height="800px"))
            )
          ),
          tabPanel(
            "Crime",
            fluidRow(
              titlePanel(
                h3("Is your house safe?")),
              br(),
              h4("Let's get familiar about the crime events near your house."),
              br(),
              br(),
              column(2,wellPanel(
                style = "overflow-y:scroll; background-color: #ffffff;",
                numericInput("c.id", h4("House_id:"),value = 2539),
                sliderInput(inputId="c.dist",h4("Distance Range:"), min = 0, max=5, value=2.5,step=0.5),
                checkboxGroupInput("added_makers","Victim Age Group:",
                                   choices=c("<18","18-24","25-44","45-64","65+","UNKNOWN"),
                                   selected = "<18"),
                actionButton("click_all_age_group","Select All"),
                actionButton("click_no_age_group","Clear Options"),
                br(),
                br(),
                submitButton("Submit",width='100%')
              )),
              column(10,leafletOutput("cplot",height="800px"))
            ),
            style="opacity = 0.8"
          )
        )
      )
    ),
    tabItem(
      tabName = "descriptive",
      tabsetPanel(type = "tabs",
                  tabPanel(
                    "Airbnb's house",
                    fluidRow(column(width =  12,h3("Price variability by time/neighborhood"), 
                                    plotlyOutput("price_neighborhood"))),
                    fluidRow(),
                    fluidRow(column(width =  12, h3("Price variability by time/room_type"), 
                                    plotlyOutput("price_roomtype"))),
                    fluidRow(),
                    fluidRow(),
                    fluidRow(column(12, 
                                    h3("Roomtype distribution across boroughs"), 
                                    "By default, the bar chart shows the distribution of a specific room type by number as the height of each bar, and pie chart shows the percentage of a specific room type in the total available houses.", 
                                    tags$div(tags$ul( 
                                      tags$li("Hover the mouse over a borough bar in histogram will modify the pie chart."), 
                                      tags$li("Hover the mouse over pie slice should change the histogram.") 
                                    )), 
                                    includeHTML("https://bl.ocks.org/mcHan18/raw/f3b0fc9627b14713d10ef261abcee03e/"))),
                    fluidRow(column(width = 12, h3("The top 10 expensive or cheap houses in each area"))),
                    fluidRow(
                      column(2,
                             radioButtons("tab3_neighborhood", "Area", 
                                          c("Manhattan" = "Manhattan",
                                            "Bronx" = "Bronx",
                                            "Brooklyn" = "Brooklyn",
                                            "Queens" = "Queens",
                                            "Staten Island" = "Staten Island")),
                             submitButton("Submit",width='100%'),div()
                      ),
                      column(5, plotlyOutput('area_plot_1'), div()),
                      column(5, plotlyOutput('area_plot_2'), div())
                    )
                  ),
                  tabPanel(
                    "NYC's Crime",
                    fluidRow(
                      h3("Crime Distribution in each Boro"),
                      plotlyOutput("crime_pie"),
                      br(),
                      h3("Crime Distribution from January to September"),
                      plotlyOutput("crime_hist",height = 300,width = "100%")
                    )
                  ),
                  tabPanel("NYC's Restaurant",
                           fluidRow(h3("Choose the Cuisine Type you wanna know:")),
                           fluidRow(selectInput("cuisine_type.hist", "Cuisine type",
                                                choices=c("Japanese","American","Spanish","Italian","Korean","Chinese","French","Thai","Mexican","Indian"),
                                                multiple = F,
                                                selected = "American",
                                                width = "20%")),
                           fluidRow(submitButton("Submit",width='20%')),
                           br(),
                           fluidRow(plotlyOutput("restaurant_cuisine")),
                           fluidRow(h3("Choose the Restaurant with High Inspection Mark")),
                           fluidRow(selectInput("grade_type","Grade type",choices=c("A","B","C"),multiple = F,selected = "A",width = "20%")),
                           fluidRow(submitButton("Submit",width='20%')),
                           br(),
                           fluidRow(plotlyOutput("grade")
                           )
                           
                  )
      )),
    tabItem(
      tabName = "references",
      fluidPage(
        fluidRow(
          box(width = 15, title = "Data Source", status = "primary",
              solidHeader = TRUE,
              "The data source of this shiny app is from",
              tags$a(href = "https://opendata.cityofnewyork.us/", "NYC Open Data"), 
              ".")
        ),
        fluidRow(
          box(width = 15, title = "Project Code", status = "primary",
              solidHeader = TRUE, 
              "The code of this project can be found at",
              actionButton(inputId='code', label="GitHub", 
                           icon = icon("github"), 
                           onclick ="window.open('https://github.com/TZstatsADS/Spring2020-Project2-group-10')"),
              ".")
        ),
        fluidRow(
          box(width = 15, title = "Contact Us", status = "primary",
              solidHeader = TRUE, 
              h4("Feel free to contact us if you're interested in this app!"),
              h5("Wang, Mengchen: mw3371@columbia.edu"),
              h5("Wang, Yuyao: yw3395@columbia.edu"),
              h5("Xie, Ruiqi: rx2171@columbia.edu"),
              h5("Zhang, Qin: qz2387@columbia.edu"), 
              h5("Zhang, Xinlin xz2863@columbia.edu")
          )
        ),
        fluidRow(
          tags$img(
            src = "airbnb_guide.jpg",
            width = "100%"
          )
        )
      )
    )
  )
)



# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)
