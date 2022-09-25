#####################
# INSTALL LIBRARIES #
#####################
if(!require(dplyr)) install.packages("dplyr")
if(!require(leaflet)) install.packages("leaflet")
if(!require(plotly)) install.packages("plotly")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(highcharter)) install.packages("highcharter")
if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")

###################
#  LOAD LIBRARIES #
###################
library(dplyr)
library(leaflet)
library(plotly)
library(ggplot2)
library(highcharter)
library(shiny)
library(shinydashboard)

###################
# DATA PROCESSING #
###################
data = read.delim('data/fatal-police-shootings-data.txt', sep=',')
data = na.omit(data)
data %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  na_if(" ") %>%
  # remove NAs
  na.omit
data$year = as.numeric(format(as.Date(data$date),'%Y'))
data = filter(data, year < 2022)

data$flee[data$flee == ''] = 'Other'
data$race[data$race == 'A'] = 'Asian'
data$race[data$race == ''] = 'Unknown'
data$race[data$race == 'W'] = 'White'
data$race[data$race == 'B'] = 'Black'
data$race[data$race == 'N'] = 'Native American'
data$race[data$race == 'H'] = 'Hispanic'
data$race[data$race == 'O'] = 'Other'
data$armed[!(data$armed %in% c("Gun", "Knife", "Unarmed", "Vehicle"))] = "Undetermined"
data$lat = round(data$latitude, 1)
data$lon = round(data$longitude, 1)
data$state_abbr = data$state
data$gender[data$gender == ''] = "Unknown"

##################
# USER INTERFACE #
##################

# Add title and logo
header = dashboardHeader(title = "Fatal Shootings")
header$children[[2]]$children[[2]] = header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] = tags$a(tags$img(src='https://thumbs.dreamstime.com/b/modern-occupation-people-cartoon-logo-police-76700324.jpg',height='50',width='65', align="left"))

sidebar = dashboardSidebar(
  sidebarMenu(
    id="nav",
    menuItem(
      "Dashboard", 
      tabName = "dashboard", 
      icon = icon("gauge"), 
      badgeLabel = "hot", 
      badgeColor = "red"
    ),
    menuItem(
      "Interactive Map", 
      tabName = "map", 
      icon = icon("globe")
    ),menuItem( 
      "Data Explorer", tabName = 'dataexplorer', icon = icon('database')
    ),menuItem(
      "Select Parameters",
      tabName = "selectparam",
      icon = icon("sliders"),
      menuItem(
        selectInput("year", "Select Year", choices = c('All', sort(unique(data$year), decreasing = T)), selected="All")
      )
    ),menuItem( 
      "FAQs", tabName = 'faqs', icon = icon('question')
    )
  ) 
)

type.choices = c(
  "Race" = "race",
  "Manner of Death" = "manner_of_death",
  "Arm" = "armed",
  "Gender" = "gender",
  "Threat Level" = "threat_level"
)

body = dashboardBody(
  tabItems(
    # Dashboard tab
    tabItem(
      tabName = "dashboard",
      fluidRow(
        column(valueBoxOutput("vbox", width=13), width=12, align="center")
      ),
      fluidRow(
        h2( "Historical Police Fatal Shootings Overview", align = 'center')
      ),fluidRow(
        column( 
          width = 12,
          h4("Fatal by Year", align = 'center'), 
          highchartOutput('fatalbyyearlinechart')
        )
      ),
      fluidRow(
        column( 
          width = 6,
          h4("Fatal by Gender", align = 'center'), 
          highchartOutput('fatalbygenderchart')
        ),
        column( 
          width = 6,
          h4("Fatal by Race", align = 'center'), 
          highchartOutput('fatalbyracechart')
        )
      ),
      fluidRow(
        column( 
          width = 12,
          h4("Age and race", align = 'center'), 
          highchartOutput('ageandracechart')
        )
      ),
      fluidRow(
        column( 
          width = 6,
          h4("Fatal by Threat Level", align = 'center'), 
          highchartOutput('fatalbythreatlevelchart')
        ),
        column( 
          width = 6,
          h4("Fatal by Arm", align = 'center'), 
          highchartOutput('fatalbyarmedchart')
        )
      )
    ),
    # Interactive Map tab
    tabItem(
      tabName="map",
      tags$head(
        includeScript("gomap.js")
      ),
      leafletOutput("map", height="660px"),
      tags$style("
                #controls {
                  /* Appearance */
                  background-color: white;
                  padding: 0 20px 20px 20px;
                  cursor: move;
                  /* Fade out while not hovering */
                  opacity: 0.65;
                  zoom: 0.9;
                  transition: opacity 500ms 1s;
                }
                #controls:hover {
                  /* Fade in while hovering */
                  opacity: 0.95;
                  transition-delay: 0;
                }
                       "),
    
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 400, height = "auto",
                    
                    h2("Floating Explorer"),
                    valueBoxOutput("vboxmap", width="100%"),
                    selectInput("type", label="Type", choices = type.choices, selected="race"),
                    plotlyOutput('plot_cat', height=200)
      )
    ),
    # Data Explorer Tab
    tabItem(
      tabName = 'dataexplorer',
      fluidRow(
        column(12, h1("Data Explorer by States and Cities"))
      ),
      fluidRow(
        column(3,
               selectInput("states", "States", choices = c("All states", sort(unique(data$state_abbr))), multiple=TRUE)
        ),
        column(3,
               conditionalPanel("input.states", selectInput("cities", "Cities", choices = c("All cities", sort(unique(data$city))), multiple=TRUE)
               )
        )
      ),
      hr(),
      DT::dataTableOutput("datatable")
    ),
    # Frequently Asked Questions tab
    tabItem(
      tabName = "faqs",
      fluidRow(
        column( 
          width = 12,
          h1("Frequenly Asked Questions", align = 'left')
        )
      ),
      fluidRow(
        column( 
          width = 12,
          h2("What are the data sources?", align = 'left'),
          tags$p("The dataset is provided by", 
                 tags$a( href="https://github.com/washingtonpost/data-police-shootings",
                         "https://github.com/washingtonpost/data-police-shootings"),
                 ". The Washington Post's database contains records of every fatal shooting in the United States by a police officer in the line of duty since Jan. 1, 2015."
          )
        )
      ),
      fluidRow(
        column( 
          width = 12,
          h2("How is the dataset pre-processed?", align = 'left'),
          tags$p("The dataset was processed by several steps including: ", 
                 tags$li("Dropping instances with NaN values."),
                 tags$li("Re-categorise several columns for better performance."),
                 tags$li("Ommiting records in 2022 since the data is not complete.")
          )
        )
      ),
      fluidRow(
        column( 
          width = 12,
          h2("Who should I contact if I have any sugguestions?", align = 'left'),
          tags$p("Your suggestions, feedback, complaints or compliments are highly valued and will guide me to improve the dashboard continuously. Please send an email to ", 
                 tags$a( href="mailto:ztom@student.unimelb.edu.au",
                         "ztom@student.unimelb.edu.au",
                         target = '_blank'),
                 "."
          )
        )
      )
    )
  )
) 

ui = dashboardPage(header, sidebar, body, skin = "black")


################
# SHINY SERVER #
################
server = function(input, output, session) {
  # Set color hue
  pal.race = colorFactor(rainbow(n_distinct(data$race)), data$race)
  pal.threat_level = colorFactor(rainbow(n_distinct(data$threat_level)), data$threat_level)
  pal.armed = colorFactor(rainbow(n_distinct(data$armed)), data$armed)
  pal.gender = colorFactor(c("#FF3333", "#401FFF", "#66FF47"), c("Male", "Female", "Unknown"))
  pal.manner_of_death = colorFactor(rainbow(n_distinct(data$manner_of_death)), data$manner_of_death)
  
  values = reactiveValues()
  
  # Update data with selected parameters
  filteredData = reactive({
    result = data
    if (input$year != 'All'){
      result = filter(data, year == input$year)
    }
    result
  })
  
  # Plot fatal by year line chart
  output$fatalbyyearlinechart = renderHighchart({
    highchart() %>%
      hc_add_series(name="Total Fatal", data %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'), color='black')%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 ) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Number of Fatal"))
  })
  
  # Plot fatal by gender chart
  output$fatalbygenderchart = renderHighchart({
    (data %>% count(year, gender)) %>% 
      hchart('column', hcaes(x = 'year', y = 'n', group = 'gender')) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'right', verticalAlign = 'top', floating = T) %>%
      hc_colors(c("red", "blue")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Number of Fatal"))
  })
  
  # Plot age and race chart
  output$ageandracechart = renderHighchart({
    (data %>% group_by(year, race) %>% summarise_at(vars(age), list(age_ave = mean))) %>% 
      hchart('column', hcaes(x = 'year', y = 'age_ave', group = 'race')) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y:.1f}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'right', verticalAlign = 'top', floating = T) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Average Age"))
  })
  
  # Plot fatal by race
  output$fatalbyracechart = renderHighchart({
    highchart() %>%
      hc_add_series(name="Asian", filter(data, race == "Asian") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_add_series(name="Black", filter(data, race == "Black") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_add_series(name="White", filter(data, race == "White") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_add_series(name="Hispanic", filter(data, race == "Hispanic") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_add_series(name="Native American", filter(data, race == "Native American") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_add_series(name="Other", filter(data, race == "Other") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_add_series(name="Unknown", filter(data, race == "Unknown") %>% count(year), type = "line", hcaes(x = year, y = n), marker = list(symbol = 'circle'))%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'right', verticalAlign = 'top', floating = T) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Number of Fatal"))
  })
  
  # Plot fatal by threat level
  output$fatalbythreatlevelchart = renderHighchart({
    (data %>% count(year, threat_level)) %>% 
      hchart('column', hcaes(x = 'year', y = 'n', group = 'threat_level')) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'right', verticalAlign = 'top', floating = T) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Number of Fatal"))
  })
  
  # Plot fatal by arm chart
  output$fatalbyarmedchart  = renderHighchart({
    (data %>% count(year, armed)) %>% 
      hchart('column', hcaes(x = 'year', y = 'n', group = 'armed')) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'right', verticalAlign = 'top', floating = T) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Number of Fatal"))
  })
  
  # Plot bar plot in the floating explorer
  output$plot_cat <- renderPlotly({
    
    if (input$type == "race"){
      result = filteredData() %>% count(race)
      xvar = result$race
      xlab = "Race"
      cvalues = c("Hispanic" = "#49FF00", "White" = "#FF00DB", "Unknown" = "#4900FF", "Black" = "#FFDB00", "Asian" = "#FF0000", "Native American" = "#00FF92", "Other" = "#0092FF")
    } else if (input$type == "gender"){
      result = filteredData() %>% count(gender)
      xvar = result$gender
      xlab = "Gender"
      cvalues = c("Male" = "#401FFF", "Female" = "#FF3333", "Unknown" = "#66FF47")
    } else if (input$type == "threat_level"){
      result = filteredData() %>% count(threat_level)
      xvar = result$threat_level
      xlab = "Threat Level"
      cvalues = c("Attack" = "#FF0000", "Other" = "#00FF00", "Undetermined" = "#0000FF")
    } else if (input$type == "armed"){
      result = filteredData() %>% count(armed)
      xvar = result$armed
      xlab = "Arm"
      pal = pal.armed
      cvalues = c("Gun" = "#FF0000", "Knife" = "#CCFF00", "Undetermined" = "#00FF66", "Unarmed" = "#0066FF", "Vehicle" = "#CC00FF")
    } else if (input$type == "manner_of_death"){
      result = filteredData() %>% count(manner_of_death)
      xvar = result$manner_of_death
      xlab = "Manner of Death"
      cvalues = c("Shot" = "#FF0000", "Shot and Tasered" = "#00FFFF")
    }
    
    p = ggplot(data=result, aes(x=reorder(xvar, -n), y=n, fill=xvar, text = paste("Number of Fatal: ", n))) + geom_bar(stat='identity', width=0.8) + labs(x=xlab, y="Number of People") + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) + scale_fill_manual(values = cvalues) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
    p = p + theme(text=element_text(size=8))
    values$loaded = TRUE
    ggplotly(p, source='plot_cat', tooltip = c("text")) %>% 
      hide_legend() %>% 
      layout(scene = list(xaxis = list(showgrid = F, showline = F),yaxis = list(showgrid = F, showline = F)))
  })
  
  # Update map data with selected parameters
  filteredDataMap = reactive({
    result = filteredData()
    if (!is.null(input$race)){
      result = filter(result, race == input$race)
    }
    if (!is.null(input$gender)){
      result = filter(result, gender == input$gender)
    }
    if (!is.null(input$threat_level)){
      result = filter(result, threat_level == input$threat_level)
    }
    if (!is.null(input$manner_of_death)){
      result = filter(result, manner_of_death == input$manner_of_death)
    }
    if (!is.null(input$armed)){
      result = filter(result, armed == input$armed)
    }
    result
  })
  
  # Plot value box (show total fatal)
  output$vbox = renderValueBox({
    valueBox("TOTAL FATAL", 
             value=nrow(filteredData()), 
             icon = icon("skull-crossbones"), 
             color="red")
  })
  
  # Plot value box in the floating explorer (shot total fatal)
  output$vboxmap = renderValueBox({
    valueBox(
      "TOTAL FATAL", 
      value=tags$p(nrow(filteredData()), style = "font-size: 80%;"),
      icon = icon("skull-crossbones"), 
      color="red")
  })
  
  
  # Render empty map
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # Plot map circle using selected parameters
  observe({
    result = filteredDataMap()
    r1 = result
    r1$popup = paste0('<b>Location: </b>', r1$latitude,', ', r1$longitude, '<br>',
                      '<b>Name</b>: ', r1$name, '<br>',
                      '<b>Gender</b>: ', r1$gender, '<br>',
                      '<b>Age</b>: ', r1$age, '<br>',
                      '<b>State</b>: ', r1$state, '<br>',
                      '<b>City</b>: ', r1$city, '<br>')
    
    if (input$type == "race"){
      pal = pal.race
      col_values = unique(data$race)
      col_data_fil = result$race
      output$map = renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addTiles() %>%
          setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%
          addCircleMarkers(data = r1,
                           lat = ~latitude, 
                           lng = ~longitude,
                           radius= 1.5,
                           color = pal(col_values),
                           stroke = FALSE, fillOpacity = 0.4, 
                           opacity = 0.3,
                           popup = ~popup) %>%
          addLegend("bottomleft", pal = pal, values=col_values)
      })
    } else if (input$type == "threat_level"){
      pal = pal.threat_level
      col_values = data$threat_level
      col_data_fil = result$threat_level
    } else if (input$type == "armed"){
      pal = pal.armed
      col_values = data$armed
      col_data_fil = result$armed
    } else if (input$type == "gender"){
      pal = pal.gender
      col_values = data$gender
      col_data_fil = result$gender
    } else if (input$type == "manner_of_death"){
      pal = pal.manner_of_death
      col_values = data$manner_of_death
      col_data_fil = result$manner_of_death
    }
    
    leafletProxy("map", data = result) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addCircleMarkers(data = r1,
                       lat = ~latitude, 
                       lng = ~longitude,
                       color = pal(col_data_fil),
                       radius = 1.5,
                       stroke = FALSE, 
                       fillOpacity = 0.4, 
                       opacity = 0.3,
                       popup=~popup) %>%
      addLegend(position="bottomleft", pal = pal, values=col_values)
  })
  
  showIDPopup = function(lat, lng, id) {
    result = data[data$id == id,]
    r1 = result
    r1$popup = paste0('<b>Location: </b>', r1$latitude,', ', r1$longitude, '<br>',
                      '<b>Name</b>: ', r1$name, '<br>',
                      '<b>Gender</b>: ', r1$gender, '<br>',
                      '<b>Age</b>: ', r1$age, '<br>',
                      '<b>State</b>: ', r1$state, '<br>',
                      '<b>City</b>: ', r1$city, '<br>')
    
    leafletProxy("map", data = result) %>%
      addPopups(lng, lat, r1$popup)
  }
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      dist = 0.5
      lat = input$goto$lat
      lng = input$goto$lng
      id = input$goto$id
      leafletProxy("map") %>% 
        clearPopups() %>%
        fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      showIDPopup(lat, lng, id)
      newtab =  switch(input$nav, "dataexplorer" = "map")
      updateTabItems(session, "nav", newtab)
    })
  })
  
  output$datatable <- DT::renderDataTable({
    df = filteredData() %>%
      select(id, name, date, state, city,  age, gender, race, manner_of_death, armed, signs_of_mental_illness, threat_level, flee, body_camera, latitude, longitude) %>%
      filter(
        is.null(input$states) | state %in% input$states,
        is.null(input$cities) | city %in% input$cities
      ) %>%
      # rename columns
      rename(ID = id) %>%
      rename(Date = date) %>%
      rename(Name = name) %>%
      rename(State = state) %>%
      rename(City = city) %>%
      rename(Age = age) %>%
      rename(Gender = gender) %>%
      rename(Race = race) %>%
      rename("Manner of Death" = manner_of_death) %>%
      rename(Arm = armed) %>%
      rename("Signs of Mental Illness" = signs_of_mental_illness) %>%
      rename("Threat Level" = threat_level) %>%
      rename(Flee = flee) %>%
      rename("Body Camera" = body_camera) %>%
      rename(Latitude = latitude) %>%
      rename(Longitude = longitude) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude,'" data-id="', ID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      action = DT::dataTableAjax(session, df, outputId = "datatable") 
    
    # Show dataframe
    DT::datatable(df, 
                  class="compact",
                  extensions = 'Buttons', 
                  options = list(
                    scrollX=TRUE,
                    autoWidth = TRUE,
                    dom = "Blfrtip", 
                    # Show download button
                    buttons = list("copy", 
                                   list(
                                    extend = "collection", 
                                    buttons = c("csv", "excel"), 
                                    text = "Download", 
                                    title = "Fatal Police Shootings"
                                    ) 
                                   ), 
                    lengthMenu = list( c(10, 50, 100, -1), c(10, 50, 100, "All")), 
                    pageLength = 10,
                    ajax = list(url = action)
                  ), 
                  escape = FALSE
    )
    
    
  })
}


#############
# RUN SHINY #
#############
shinyApp(ui, server)