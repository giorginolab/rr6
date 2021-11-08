#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

spleen.max <- 30

ui <- fluidPage(
    title = "RR6 Model",
    
    titlePanel("RR6 Model Calculator"),
    
    hr(),
    
    fluidRow(
        column(3,
               h4("Data at baseline"),
               sliderInput('spleen_0', 
                           label='Spleen size (cm bcm)', 
                           min=0, max=spleen.max, 
                           value=0, round=-1),
               br(),
               numericInput('rux_0', 
                            label='Daily RUX dose (mg)',
                            step=1,
                            value=0),
               br(),
               radioButtons("rbc_0", 
                            label = "RBC transfusion",
                            choices = list("Not necessary" = 0,
                                           "Necessary" = 1), 
                            selected = 0),
        ),
        column(3,
               h4("Data at 3 months"),
               sliderInput('spleen_3', 
                           label='Spleen size (cm bcm)', 
                           min=0, max=spleen.max, 
                           value=0, round=-1),
               br(),
               numericInput('rux_3', 
                            label='RUX dose per day',
                            step=20,
                            value=0),
               br(),
               checkboxInput('rbc_3', 
                             label='RBC transfusion')
        ),
        column(3,
               h4("Data at 6 months"),
               sliderInput('spleen_6', 
                           label='Spleen size (cm bcm)', 
                           min=0, max=spleen.max, 
                           value=0, round=-1),
               br(),
               numericInput('rux_6', 
                            label='RUX dose per day',
                            step=20,
                            value=0),
               br(),
               checkboxInput('rbc_6', 
                             label='RBC transfusion')
        ),
        column(2,offset = 1,
               h4("Score composition"),
               p("Spleen: ",textOutput("spl_score", inline=T), " points")
       ),
    ),
    
    hr(),
    
    h4("RR6 score"),
    textOutput("rr6_score_text"),
    
    hr(),
    img(src="km-curve.jpeg")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    spl.score <- function(input) {
        low.spl.resp.3 <- (input$spleen_0-input$spleen_3)/input$spleen_0 <= .3
        low.spl.resp.6 <- (input$spleen_0-input$spleen_6)/input$spleen_0 <= .3
        ifelse(low.spl.resp.3 && low.spl.resp.6, 1.5, 0)
    }

    output$spl_score <- renderText(spl.score(input))
       
    
}

# Run the application 
shinyApp(ui = ui, server = server)
