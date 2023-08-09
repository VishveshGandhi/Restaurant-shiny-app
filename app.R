# Load packages
library(shiny)
library(plotly)
library(tidyverse)
library(leaflet)

# Create Shiny App for Restaurants and Students

# Read in Dataset
restaurant_ratings = read.csv('restaurant_ratings.csv')
student_info = read.csv('student_info.csv')
dat_dr = student_info %>% group_by(drink_level) %>% summarise(num = n())
dat_tr = student_info %>% group_by(transport) %>% summarise(num = n())
dat_bg = student_info %>% group_by(budget) %>% summarise(num = n())
dat_age = student_info %>% group_by(age) %>% summarise(num = n())

dat_merge = read.csv('dat_merge.csv')
cuisine = read.csv('./chefmozcuisine.csv')
userprofile = read.csv('./userprofile.csv')
rating = read.csv('./rating_final.csv')

dat_res = dat_merge
dat_res$name[dat_res$placeID == '132584'] = "Gorditas Dona Tota1"


cuisine = cuisine %>% filter(placeID %in% dat_merge$placeID)

cuisine_num = cuisine %>% group_by(Rcuisine) %>% 
  summarise(cuisine_num = n())

resicon = makeIcon('restaurant.png', iconWidth = 20, iconHeight = 24)
homeicon = makeIcon('home.png', iconWidth = 20, iconHeight = 24)

# For San Luis Potosi
dat_san = dat_merge %>% filter(state == 'San Luis Potosi')
rating_san = rating %>% filter(placeID %in% dat_san$placeID)
user_san = userprofile %>% filter(userID %in% rating_san$userID)
user_san = merge(user_san, rating_san, by = 'userID')


# Define UI
ui = navbarPage('Mexico Restaurant Dataset EDA',
                tabPanel('Restaurant Analysis',
                         sidebarLayout(
                           sidebarPanel(
                             selectInput('variable', 'Choose a variable',
                                         c('alcohol', 'price', 'franchise',
                                           'dist_res_uni')),
                             radioButtons(inputId = 'y',
                                         label = 'Choose a response variable:',
                                         choices = c('rating', 'reviews')),
                             width = 3),
                           mainPanel(plotlyOutput('boxPlot'))
                         )),
                tabPanel('Consumer Analysis',
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = 'variable_1', 
                                         label = 'Choose a variable',
                                         choices = c('drink_level', 'transport', 
                                                     'age', 'budget')),
                             width = 3
                           ),
                           mainPanel(plotlyOutput('barPlot'))
                         )),
                tabPanel("Map Visualization",
                         fluidPage(
                           leafletOutput("mymap"),
                           fluidRow(column(width = 4,
                                           tableOutput("click_table")),
                                    column(width = 8,
                                           plotOutput("myplot")))
                         ))
                )


# Define server function
server = function(input, output, session){
  output$boxPlot = renderPlotly({
    var = input$variable
    if(var == 'dist_uni_res'){
      plot_ly(data = restaurant_ratings, x = ~dist_res_uni, y = ~input$y) %>% 
        layout(title = paste(input$y, ' VS.', input$variable), 
               yaxis = list(title = input$y), 
               xaxis = list(title = input$variable))
    }else{
    plot_ly(data = restaurant_ratings, x = ~get(input$variable), y = ~get(input$y), 
            color = ~get(input$variable), type = 'box') %>%
      
      layout(title = paste(input$y, ' VS.', input$variable), 
             yaxis = list(title = input$y), 
             xaxis = list(title = input$variable))}
  })
  
  output$barPlot = renderPlotly({
    var_1 = input$variable_1
    if(var_1 == 'drink_level'){    
      plot_ly(data = dat_dr, x = ~reorder(get(input$variable_1), -num), y = ~num, 
                                           color = ~get(input$variable_1), type = 'bar') %>%
        add_text(
          text = ~num,
          textfont = list(color = 'red'),
          textposition = 'bottom center',
          showlegend = FALSE,
          cliponaxis = FALSE,
          hoverinfo = 'none'
        ) %>% 
        layout(yaxis = list(title = paste0('Number of ', input$variable_1)), 
               xaxis = list(title = input$variable_1))}
    else if(var_1 == 'transport'){
      plot_ly(data = dat_tr, x = ~reorder(get(input$variable_1), -num), y = ~num, 
              color = ~get(input$variable_1), type = 'bar') %>%
        add_text(
          text = ~num,
          textfont = list(color = 'red'),
          textposition = 'bottom center',
          showlegend = FALSE,
          cliponaxis = FALSE,
          hoverinfo = 'none'
        )%>%
        layout(yaxis = list(title = paste0('Number of ', input$variable_1)), 
               xaxis = list(title = input$variable_1))
    }
    else if(var_1 == 'budget'){
      plot_ly(data = dat_bg, x = ~reorder(get(input$variable_1), -num), y = ~num, 
              color = ~get(input$variable_1), type = 'bar') %>%
        add_text(
          text = ~num,
          textfont = list(color = 'red'),
          textposition = 'bottom center',
          showlegend = FALSE,
          cliponaxis = FALSE,
          hoverinfo = 'none'
        ) %>%
        layout(yaxis = list(title = paste0('Number of ', input$variable_1)), 
               xaxis = list(title = input$variable_1))
    }
    else{
      plot_ly(data = dat_age, x = ~get(input$variable_1), type = 'histogram') %>%
        layout(yaxis = list(title = paste0('Number of ', input$variable_1)), 
               xaxis = list(title = input$variable_1))
    }
  })
  
  output$mymap <- renderLeaflet({
    leaflet( ) %>% 
      addTiles() %>%
      addMarkers(data = user_san, ~longitude, ~latitude, icon = homeicon,
                 label = ~paste0('Year: ', birth_year, '; Religion: ', religion),
                 layerId = user_san$userID) %>%
      addMarkers(data = dat_san, ~longitude, ~latitude, icon = resicon,
                 label = ~paste0('Name: ', name, '; Price: ', price, '; Rating: ', rating_ave),
                 layerId = dat_san$placeID)
  })
  
  observe({
    click <- input$mymap_marker_click
    if(is.null(click))
      return()
    
    Id <- click$id
    if(substr(Id, 1, 1) == "U"){tile <- "user"}
    else{tile <- "place"}
    
    if(tile == "user"){
      
      thing <- user_san[user_san$userID == Id, ][1, -c(1,2,3,20,21,22,23)] 
      thing$birth_year <- as.character(thing$birth_year)
      thing$weight <- as.character(thing$weight)
      thing$height <- as.character(thing$height)
      
      click_info <- thing %>% pivot_longer(colnames(thing))
      
      ggp <- ggplot(user_san[user_san$userID == Id, ][,c(21,22,23)] %>% 
                      pivot_longer(rating:service_rating, 
                                   names_to = "rating_type", 
                                   values_to = "ratings_value")) + 
        geom_boxplot(aes(x = rating_type, y = ratings_value)) + 
        xlab("Rating type") + 
        ylab("Rating Value")
    }
    
    else{
      
      thing <- dat_san[dat_san$placeID == Id, ][1, -c(1,2,3,5,16,17,18)]
      thing$user_num <- as.character(thing$user_num)
      click_info <- thing %>% pivot_longer(colnames(thing))
      
      thing <- user_san[user_san$placeID == Id, ][,c(21,22,23)] 
      thing$rating <- ordered(thing$rating)
      thing$service_rating <- ordered(thing$service_rating)
      thing$food_rating <- ordered(thing$food_rating)
      
      ggp <- thing %>% 
        pivot_longer(colnames(thing), 
                     names_to = "rating_type", 
                     values_to = "ratings_value") %>%
        group_by(rating_type, ratings_value) %>%
        summarise(n = n()) %>%
        ggplot(aes(x=rating_type,y=n,fill=ratings_value,
                   label="Ratings Values",
                   color = ratings_value))+
        geom_bar(position="dodge", stat="identity") + 
        xlab("Rating type") + 
        ylab("Number of Reviews")
    }
    
    
    output$click_table <- renderTable(click_info)
    
    output$myplot <- renderPlot({
      ggp
    })
    
  })
  
}



# Create Shiny App
shinyApp(ui, server)






