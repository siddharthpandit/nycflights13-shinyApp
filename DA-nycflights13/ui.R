shinyUI(fluidPage(
  theme = shinythemes::shinytheme("yeti"),
  titlePanel('Flight Data Analysis ', windowTitle = 'CEU-RLAB-HW2017'),
  navbarPage(
    "NYCFLIGHT13",
    navbarMenu(
      "Data Explorer",
      tabPanel("Flights",
               DT::dataTableOutput("data_f")),
      tabPanel("Airlines",
               DT::dataTableOutput("data_airl")),
      tabPanel("Airports",
               DT::dataTableOutput("data_airp")),
      tabPanel("Plane",
               DT::dataTableOutput("data_p")),
      tabPanel("Weather",
               DT::dataTableOutput("data_w"))
    ),
    navbarMenu(
      "Visualizations",
      tabPanel(
        "Delay Statistics",
        sidebarLayout(
          fluid = 'TRUE',
          sidebarPanel(
            h2("Delay Statistics"),
            p(
              "Visualize delays in different contexts by selecting the date and airport from the menu below and the category from the panel on the right-hand side"
            ),
            br(),
            p(
              "The Time Trend plot shows the general variation of total delay (arr_delay+dep_delay) across the year for all three airports. Changing the inputs below will not effect it."
            ),
            selectInput(
              inputId = "airport",
              'Select Airport',
              c('LGA', 'JFK', 'EWR'),
              selected = 'LGA'
            ),
            selectInput(
              inputId = "month",
              'Select Month',
              c(1:12),
              selected = 1
            ),
            selectInput(inputId = "day", 'Select Day', c(1:31), selected = 15)
            #sliderInput('poly', "Polynomial",
            #           min = 1, max = 16, value = 1),
            ##selectInput('colorby',"Color By",c("gear","am","cyl")),
            #checkboxInput('se',"Confidence Interval")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                'Time Trend',
                plotOutput('trend_line'),
                br(),
                plotOutput('trend_plot')
              ),
              tabPanel('Departure Delay', plotOutput('dep_delay_plot')),
              tabPanel('Arrival Delay', plotOutput('arr_delay_plot')),
              tabPanel(
                'Outgoing Flights',
                plotOutput('outgoing_delay_flights_plot')
              ),
              tabPanel(
                'Average delay per carrier',
                plotOutput('avg_delay_flights_plot')
              )
            )
          )
        )
      ),
      tabPanel(
        "Weather on Delay",
        sidebarLayout(
          fluid = TRUE,
          sidebarPanel(
            h2("Effect of Weather on Total Delay"),
            p(
              "Visualize the effect of differnt weather conditions on flight delays by selecting the weather element below"
            ),
            p(
              " Total Delay is obtained by summing up non-negative values of arr_delay and dep_delay"
            ),
            br(),
            p(
              " Note: The graph may take some time to load due to the size of the dataset"
            ),
            selectInput(
              inputId = "var1",
              'Select Weather Element',
              c(
                "Temperature",
                "Pressure",
                "Humidity",
                "Windspeed",
                "Precipitation"
              ),
              selected = "Temperature"
            )
          ),
          mainPanel(tabsetPanel(
            tabPanel('Weather conditions', plotOutput('weather_plot'))
          ))
        )
      ),
      tabPanel(
        "Regression Models",
        sidebarLayout(
          fluid = TRUE,
          sidebarPanel(
            h2("Regression Models"),
            p(
              "Visualize the regression of different parameters on Total Delay (arr_delay + dep_delay"
            ),
            selectInput(inputId =
                          "var2", "Select L.H.S. variable", c("arr_delay", "dep_delay")),
            selectInput(
              inputId = "var3",
              "Select R.H.S. variable",
              c("carrier", "origin", "dest", "air_time", "airline")
            )
          ),
          mainPanel(tabsetPanel(
            tabPanel('Regression Analysis', plotOutput('regression_plot'))
          ))
        )
      )
      
    ),
    tabPanel("Summary",
             sidebarLayout(fluid=TRUE,
                           sidebarPanel(
                             h2("Exploratory data analysis")
                           ),
                           mainPanel(
                           htmlOutput('markdown'))))
  )
))
