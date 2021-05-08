# -------------- Razan ---------------

library(shiny)
library(ggplot2)
library(dplyr)



load("movies.Rdata")

#View(movies)


ui <- fluidPage(
  
  titlePanel("Movies Dashboard"),
 

  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "rating_type" , label = "Select the Rating Type", 
                   choices = c("Motion Picture Association film rating system" = "mpaa_rating",
                              "Critics Rating" = "critics_rating", 
                                "Audience Rating" = "audience_rating" )),
      
    ),
    
    mainPanel(
      plotOutput(outputId = "bar")
    )
  ), # end layout
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("imdb_rating",
                              "imdb_num_votes",
                              "critics_score",
                              "audience_score",
                              "runtime"), 
                  selected = "audience_score"),
      
     
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("imdb_rating",
                              "imdb_num_votes",
                              "critics_score", 
                              "audience_score",
                              "runtime"), 
                  selected = "critics_score"),
      
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("imdb_rating",
                              "imdb_num_votes", 
                              "critics_score", 
                              "audience_score", 
                              "runtime"), 
                  selected = "critics_score"),
      
      
    ),
    
  
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  ), # end layout
  
  
  # -------------- Buthina ---------------
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "cat", 
                  label = "Select For X-axis",
                  choices = c("Genre" = "genre",
                              "Type"   = "title_type",
                              "Critics Rating" = "critics_rating"), 
                  selected = "Type"),
      
      selectInput(inputId = "y_cat", 
                  label = "Select For X-axis:",
                  choices = c("Runtime" = "runtime",
                              "Year" = "dvd_rel_year"), 
                  selected = "runtime"),
      
      
    ),
    
    
    mainPanel(
      plotOutput(outputId = "boxplot")
    )
  ), # end layout
  
  

  
  
  
  checkboxGroupInput("variable", "Movies Details to show:",
                     c("Movie Title"    = "title",
                       "Movie Genre" = "genre",
                       "Year"  = "thtr_rel_year"
                     )
  ),
  tableOutput("data"),
  
)


# -------------- Waad ---------------
server <- function(input, output) {
  
  output$bar <- renderPlot({
    if (input$rating_type == "mpaa_rating") {
      ggplot(data = movies) + 
        geom_bar(mapping = aes(mpaa_rating , fill = mpaa_rating ) , na.rm = TRUE )
    } else if (input$rating_type == "critics_rating") {
      ggplot(data = movies) + 
        geom_bar(mapping = aes(critics_rating , fill = critics_rating), na.rm = TRUE)
    }
    else if (input$rating_type == "audience_rating") {
      ggplot(data = movies) + 
        geom_bar(mapping = aes(audience_rating , fill = audience_rating), na.rm = TRUE)
    }
  })
  
  
  output$data <- renderTable({
    
    movies %>%
      group_by_at(vars(input$variable)) %>%
      count()
    
  },
  rownames = TRUE)
  
  # -------------- Abdullah  ---------------
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y,
                                     color = input$z)) +
      geom_point(na.rm = TRUE)
  })
  
  
  output$boxplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$cat, y = input$y_cat , color = input$cat )) +
      geom_boxplot(na.rm = TRUE)
  })
  
  
}






shinyApp(ui, server)