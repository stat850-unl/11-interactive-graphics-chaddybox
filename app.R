# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(reactable)
library(scales)
library(plotly)

# Read in data and eliminate duplicates in cocktail categories

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

drink_category <- unique(cocktails$category)

drink_ingredient <- as.data.frame(unique(cocktails$ingredient))
colnames(drink_ingredient) = "ingredient"
drink_ingredient <- as.data.frame(sort(drink_ingredient$ingredient))
colnames(drink_ingredient) = "ingredient"

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                navbarPage(
                  "CocktailR",
                  tabPanel("Cocktail Complexity",
                           sidebarPanel(
                             inputID = "category_choice",
                             radioButtons("checkGroup",
                                                h4("Select a Category"),
                                                choices = drink_category),
                             
                           ), 
                           mainPanel(
                             h4("Complexity of Cocktail Categories"),
                             plotOutput("plot1"),
                             
                           ) 
                           
                  ), 
                  tabPanel("Cocktail Recipes by Ingredient",
                           sidebarPanel(
                             inputID = "ingredient_choice",
                             selectizeInput("select",
                                                h4("Select an Ingredient"),
                                                choices = drink_ingredient),
                           ),
                           mainPanel(
                             h4("Recipes Table"),
                             tableOutput("recipes")
                           )
                  )
                ) 
) 


# Define server function  
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    ggplot(data = subset(cocktails, cocktails$category %in% input$checkGroup), mapping = aes(x = ingredient_number), stat = "count") +
      geom_histogram() +
      ggtitle("How Many Ingredients are Required?") +
      xlab("Number of Ingredients") +
      ylab("Number of Recipes") +
      scale_x_continuous(breaks=pretty_breaks()) +
      scale_x_continuous(breaks=pretty_breaks())
  })
  
  tab <- reactive({
    
    cocktails %>%
      filter(ingredient == input$select)
    
  })
  
  output$recipes <- renderTable({
    tab()
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
