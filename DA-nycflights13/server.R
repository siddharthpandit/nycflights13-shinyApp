#Install Packages
#install.packages(c("shiny","ggplot2","dplyr","nycflights13","arm","knitr","rmarkdown","lattice"))

#Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(nycflights13)
library(arm)
library(knitr)
library(rmarkdown)
library(lattice)

#Create local dataframes
flight <- nycflights13::flights[1:50000, ]
airline <- nycflights13::airlines
airport <- nycflights13::airports
plane <- nycflights13::planes
weather_1 <- nycflights13::weather

#Changing type to factor for filter in Data Explorer
flight$tailnum <- NULL
flight$air_time <- NULL
flight$hour <- NULL
flight$minute <- NULL
flight$carrier <- as.factor(flight$carrier)
flight$year <- as.factor(flight$year)
flight$month <- as.factor(flight$month)
flight$day <- as.factor(flight$day)

airline$carrier <- as.factor(airline$carrier)
airline$name <- as.factor(airline$name)

airport$faa <- as.factor(airport$faa)
airport$name <- as.factor(airport$name)
airport$tzone <- as.factor(airport$tzone)

plane$tailnum <- as.factor(plane$tailnum)
plane$type <- as.factor(plane$type)
plane$manufacturer <- as.factor(plane$manufacturer)
plane$model <- as.factor(plane$model)
plane$engine <- as.factor(plane$engine)

weather_1$origin <- as.factor(weather$origin)

flight$origin <- as.factor(flight$origin)
flight$dest <- as.factor(flight$dest)


data <- flights %>%
  dplyr::select(origin, month, day , arr_delay, dep_delay) %>%
  filter(arr_delay >= 0, dep_delay >= 0) %>%
  group_by(origin, month, day) %>%
  summarise(avg_delay =  mean(arr_delay, na.rm = TRUE) +
              mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(-avg_delay)

data$date <- with(data, ISOdate(year = 2013, month, day))

flights_1 <- flights
weather_2 <- weather
flights_1$hour <- ifelse(flights_1$hour == 24, 0, flights_1$hour)
flights_weather <- left_join(flights_1, weather_2)
flights_weather$arr_delay <- ifelse(flights_weather$arr_delay >= 0,
                                    flights_weather$arr_delay, 0)
flights_weather$dep_delay <- ifelse(flights_weather$dep_delay >= 0,
                                    flights_weather$dep_delay, 0)
flights_weather$total_delay <-
  flights_weather$arr_delay + flights_weather$dep_delay

cor_data <-
  dplyr::select(
    flights_weather,
    total_delay,
    temp,
    dewp,
    humid,
    wind_dir,
    wind_speed,
    wind_gust,
    precip,
    pressure,
    visib
  )

cor_data$Temperature <- cor_data$temp
cor_data$Pressure <- cor_data$pressure
cor_data$Humidity <- cor_data$humid
cor_data$Windspeed <- cor_data$wind_speed
cor_data$Precipitation <- cor_data$precip

#CREATE AVERAGE DEPARTURE DELAY TABLE
avgdepdelay <- flights %>%
  group_by(month, day, carrier, origin) %>%
  filter(month < 13 & dep_delay > 0) %>%
  summarise(avgdepdelay = mean(dep_delay, na.rm = TRUE))

#Set column order for 'flight' dataframe
data.table::setcolorder(flight, c(1, 2, 3, 15, 12, 13, 10, 11, 5, 4, 6, 8, 7, 9, 14))

shinyServer(function(input, output) {
  
  # Filter flights
  datasetInput <- reactive({
    filter(flight,
           origin == input$airport &
             month == input$month & day == input$day & dep_delay > 0)
  })
  
  # Filter flights for arrival delay
  datasetInput_arr <- reactive({
    filter(flight,
           origin == input$airport &
             month == input$month & day == input$day & arr_delay > 0)
  })
  
  # Filter flights for deperature delay
  datasetInput_do <- reactive({
    filter(flight,
           origin == input$airport &
             month == input$month & day == input$day & dep_delay > 0)
  })
  
  # Filter flights for average departure delay per carrier
  datasetInput_avgdepdel <- reactive({
    filter(avgdepdelay,
           origin == input$airport & month == input$month & day == input$day)
  })
  
  #output$ggplot <- renderPlot({
  
  #ggplot(flight, aes_string(x = input$var1,
  #                 y = input$var2,color=input$colorby)) +
  #geom_histogram()
  #geom_smooth(method = 'lm',
  #            formula = y ~ poly(x,as.numeric(input$poly)),
  #           se = input$se)
  
  #})
  
  output$dep_delay_plot <- renderPlot({
    g <-
      ggplot(data = datasetInput(), aes(x = dep_delay)) + labs(x = "Departure Delay (mins)")
    g <-
      g + geom_histogram(fill = "orange", colour = "black") + ggtitle(
        paste(
          "Departure delays for flights leaving ",
          input$airport,
          "on",
          input$month,
          "-",
          input$day
        )
      ) + theme(plot.title = element_text(hjust = 0.5))
    g
  })
  
  #output$model <- renderPrint({
  # fit <- lm(flight[,input$var2] ~
  #            poly(flight[, input$var1],input$poly))
  #summary(fit)
  #})
  
  output$arr_delay_plot <- renderPlot({
    g <-
      ggplot(data = datasetInput_arr(), aes(x = arr_delay)) + labs(x = "Arrival Delay (mins)")
    g <-
      g + geom_histogram(fill = "orange", colour = "black") + ggtitle(
        paste(
          "Arrival delays in minutes for flights leaving",
          input$airport,
          "on",
          input$month,
          "-",
          input$day
        )
      ) + theme(plot.title = element_text(hjust = 0.5))
    g
    
  })
  
  output$outgoing_delay_flights_plot <- renderPlot({
    g <-
      ggplot(data = datasetInput_do(), aes(x = as.factor(carrier))) + labs(x =
                                                                             "Delayed flights per carrier")
    g <-
      g + geom_bar(fill = "orange", colour = "black") + ggtitle(
        paste(
          "Delayed outgoing flights from",
          input$airport,
          "on",
          input$month,
          "-",
          input$day
        )
      ) + theme(plot.title = element_text(hjust = 0.5))
    g
    
  })
  
  output$avg_delay_flights_plot <- renderPlot({
    g <-
      ggplot(data = datasetInput_avgdepdel(), aes(x = as.factor(carrier), y =
                                                    avgdepdelay)) + labs(x = "Carrier", y = "Average delay (mins)")
    g <-
      g + geom_col(fill = "orange", colour = "black") + ggtitle(
        paste(
          "Average delay in departure per carrier from",
          input$airport,
          "on",
          input$month,
          "-",
          input$day
        )
      ) + theme(plot.title = element_text(hjust = 0.5))
    g <- g + geom_line()
    g
    
  })
  
  output$trend_plot <- renderPlot({
    g <-
      ggplot(data, aes(x = date, y = avg_delay, title = "Seasonality Trends"))
    g <-
      g + geom_point(aes(color = data$origin)) + xlab("Date") + ylab("Average Delay (mins)") + geom_smooth(color = "blue")
    g + ggtitle(paste("Delay trend with scatterplot highlighting origin airport")) + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$trend_line <- renderPlot({
    g <-
      ggplot(data, aes(x = date, y = avg_delay, title = "Seasonality Trends"))
    g <-
      g + xlab("Date") + ylab("Average Delay (mins)") + geom_smooth(color = "blue")
    g + ggtitle(paste("Delay trend across all airports")) + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$data_f <- DT::renderDataTable(
    DT::datatable(
      flight,
      rownames = FALSE,
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        autoWidth = TRUE,
        pageLength = 15,
        scrollX = TRUE,
        colReorder = 'TRUE',
        dom = 'Brtip',
        buttons = c('colvis', 'pdf', 'print')
      ),
      filter = list(
        position = 'top',
        clear = FALSE,
        plain = TRUE
      ),
      style = 'bootstrap'
    )
  )
  
  output$data_airl <- DT::renderDataTable(
    DT::datatable(
      airline,
      rownames = FALSE,
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        autoWidth = TRUE,
        pageLength = 15,
        scrollX = TRUE,
        colReorder = 'TRUE',
        dom = 'Brtip',
        buttons = c('colvis', 'pdf', 'print')
      ),
      filter = list(
        position = 'top',
        clear = FALSE,
        plain = TRUE
      ),
      style = 'bootstrap'
    )
  )
  
  output$data_airp <- DT::renderDataTable(
    DT::datatable(
      airport,
      rownames = FALSE,
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        autoWidth = TRUE,
        pageLength = 15,
        scrollX = TRUE,
        colReorder = 'TRUE',
        dom = 'Brtip',
        buttons = c('colvis', 'pdf', 'print')
      ),
      filter = list(
        position = 'top',
        clear = FALSE,
        plain = TRUE
      ),
      style = 'bootstrap'
    )
  )
  
  output$data_p <- DT::renderDataTable(
    DT::datatable(
      plane,
      rownames = FALSE,
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        autoWidth = TRUE,
        pageLength = 15,
        scrollX = TRUE,
        colReorder = 'TRUE',
        dom = 'Brtip',
        buttons = c('colvis', 'pdf', 'print')
      ),
      filter = list(
        position = 'top',
        clear = FALSE,
        plain = TRUE
      ),
      style = 'bootstrap'
    )
  )
  
  output$data_w <- DT::renderDataTable(
    DT::datatable(
      weather_1,
      rownames = FALSE,
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        autoWidth = TRUE,
        pageLength = 15,
        scrollX = TRUE,
        colReorder = 'TRUE',
        dom = 'Brtip',
        buttons = c('colvis', 'pdf', 'print')
      ),
      filter = list(
        position = 'top',
        clear = FALSE,
        plain = TRUE
      ),
      style = 'bootstrap'
    )
  )
  
  output$weather_plot <- renderPlot({
    g <-
      ggplot(cor_data,
             aes_string(y = input$var1, x = cor_data$total_delay)) + ggtitle(paste("Total Delay v/s", input$var1)) + theme(plot.title = element_text(hjust = 0.5))
    g + geom_smooth() + ylab(input$var1) +
      xlab("Total Delay (mins)")
  })
  
  output$regression_plot <- renderPlot({
    g <-
      ggplot(flights_weather, aes_string(y = input$var2, x = input$var3)) + ggtitle(paste("Total Delay v/s", input$var1)) + theme(plot.title = element_text(hjust = 0.5))
    g + geom_smooth() + ylab(input$var2) +
      xlab(input$var3)
  })
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('nycflights13.md', quiet = TRUE)),options = c('skip_html'))
  })
  
})
