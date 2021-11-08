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
    
    titlePanel("RR6 Model Calculator - Preview"),
    p("The RR6 model predicts survival in myelofibrosis based on clinical response after 6 months of ruxolitinib."),
    p("Reference: M. Maffioli et al., A Prognostic Model to Predict Survival After 6 Months of Ruxolitinib in Patients with Myelofibrosis. (Under review)."),
    p(em("The model is for research use only. It does not constitute medical advice.")),
    
    hr(),
    
    fluidRow(
        column(4,wellPanel(
            h4("Data at baseline"),
            sliderInput('spleen_0', 
                        label='Spleen size (cm below l.c.m.)', 
                        min=5, max=spleen.max, 
                        value=5, round=-1),
            br(),
            sliderInput('rux_0', 
                        label='Daily RUX dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            br(),
            radioButtons("rbc_0", 
                         label = "RBC transfusion",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
            "...in the previous 3 months."
        )
        ),
        column(4,inputPanel(
            h4("Data at 3 months"),
            sliderInput('spleen_3', 
                        label='Spleen size (cm below l.c.m.)', 
                        min=0, max=spleen.max, 
                        value=0, round=-1),
            br(),
            sliderInput('rux_3', 
                        label='Daily RUX dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            br(),
            radioButtons("rbc_3", 
                         label = "RBC transfusion",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
            "...since baseline."
        )),
        column(4,inputPanel(
            h4("Data at 6 months"),
            sliderInput('spleen_6', 
                        label='Spleen size (cm below l.c.m.)', 
                        min=0, max=spleen.max, 
                        value=0, round=-1),
            br(),
            sliderInput('rux_6', 
                        label='Daily RUX dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            br(),
            radioButtons("rbc_6", 
                         label = "RBC transfusion",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
            "...since the 3 mo visit."
        )),
        
    ),
    
    hr(),
    
    fluidRow(
        column(5,
               wellPanel(
                   h3("Results"),
                   p("Risk stratum:", strong(textOutput("total.score.class.text", inline=T), "-",  
                                             strong(textOutput("total.score.label.text", inline=T)))),
                   p("Median survival:", strong("FIXME") ),
               )),
        column(3,
                   h4("Calculation"),
                   span("Risk points for spleen: ", strong(textOutput("spleen.score.text", inline=T))),
                   br(),
                   span("Risk points for rux dose: ", strong(textOutput("dose.score.text", inline=T))),
                   br(),
                   span("Risk points for transfusion: ", strong(textOutput("transfusion.score.text", inline=T))),
                   p("Total: ", strong(textOutput("total.score.score.text", inline=T))),
               ),
        column(3,offset=1,
               h4("Legend"),
               span(strong("RBC"),"- red blood cells"),
               br(),
               span(strong("l.c.m."), "- left costal margin")
               )
    ),
    
    hr(),
    h4("Survival curves"),
    "Actuarial survival curves of the 3 risk groups of patients according to the",
    em("Response to Ruxolitinib after 6 months"),
    "(RR6) developed in ruxolitinib-treated myelofibrosis patients (training cohort).  From Maffioli et al., xxxx",
    br(),
    img(src="km-curve.jpeg")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    spl.score <- function() {
        low.spl.resp.3 <- (input$spleen_0-input$spleen_3)/input$spleen_0 <= .3
        low.spl.resp.6 <- (input$spleen_0-input$spleen_6)/input$spleen_0 <= .3
        s<-ifelse(low.spl.resp.3 && low.spl.resp.6, 1.5, 0)
        s <- ifelse(is.na(s),0,s)
        return(s)
    }
    
    dose.score <- function() {
        s <- ifelse( input$rux_0<40 && input$rux_3<40 && input$rux_6<40, 1, 0)
        return(s)
    }
    
    transfusion.score <- function() {
        if(input$rbc_0==1 && input$rbc_3==1 && input$rbc_6==1) {
            s<-1.5
        } else if (input$rbc_3==1 || input$rbc_6==1) {
            s<-1
        } else {
            s<-0
        }
        return(s)
    }
    
    total.score <- function() {
        s <- spl.score()+dose.score()+transfusion.score()
        if(s==0) {
            gs <- "LR"
            gl <- "Low"
        } else if(s <= 2 ) {
            gs <- "IR"
            gl <- "Intermediate"
        } else {
            gs <- "HR"
            gl <- "High"
        }
        list(score=s, class=gs, label=gl)
    }
    
    output$spleen.score.text <- renderText(spl.score())
    output$dose.score.text <- renderText(dose.score())
    output$transfusion.score.text <- renderText(transfusion.score())
    
    output$total.score.score.text <- renderText(total.score()$score)
    output$total.score.class.text <- renderText(total.score()$class)
    output$total.score.label.text <- renderText(total.score()$label)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
