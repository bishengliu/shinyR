
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
    # Application title
      headerPanel(h1("Volcano Plot", style = "color: #4d3a7d;")),
      #titlePanel("Volcano plot"),
      fluidRow(
          column(8, 
       
          # Show a plot and a table
            mainPanel(
              plotOutput("volcano"),
              tabPanel('df',
                       dataTableOutput("table")))
          ),
          column(4, 
                 
                 selectInput(
                   inputId="pvCol", 
                   label = "Select p-value Column", 
                   choices = c(),
                   multiple = FALSE),
                 selectInput(
                   inputId="fcCol", 
                   label = "Select Fold Change Column",
                   choices = c(),
                   multiple = FALSE),
                tags$hr(), 
              selectInput(
                inputId="pvselection", 
                label = "Select p-Value", 
                choices = c("p<0.05" = 1.30103, "p<0.01" = 2), 
                selected =  1.3013, 
                multiple = FALSE),
              selectInput(
                inputId="fcselection", 
                label = "Select fold change", 
                choices = c("2" = 2, "4" = 4), 
                selected =  2, 
                multiple = FALSE)
        )
    )
  )
)
