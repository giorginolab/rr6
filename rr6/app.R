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
    
    titlePanel("RR6 Calculator — Preview"),
    p("The RR6 model predicts survival in myelofibrosis based on clinical response after 6 months of ruxolitinib."),
    p("Reference: M. Maffioli et al., A Prognostic Model to Predict Survival After 6 Months of Ruxolitinib in Patients with Myelofibrosis. (Under review)."),
    p(em("The model is for research use only. It does not constitute medical advice.")),
    
    hr(),
    
    fluidRow(
        column(4,wellPanel(
            h4("Data at baseline"),
            hr(),
            numericInput('spleen_0', 
                         label='Spleen size (cm below LCM)', 
                         min=5, max=spleen.max, 
                         step=0.1,
                         value=5),
            br(),
            sliderInput('rux_0', 
                        label='Total daily ruxolitinib dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            helpText("E.g.: 20 mg bis in die (BID) = 40 mg total daily dose"),
            br(),
            radioButtons("rbc_0", 
                         label = "RBC transfusions",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
            "...in the previous 3 months."
        )),
        column(4,wellPanel(
            h4("Data at 3 months"),
            hr(),
            numericInput('spleen_3', 
                         label='Spleen size (cm below LCM)', 
                         min=0, max=spleen.max, 
                         step=0.1,
                         value=0),
            br(),
            sliderInput('rux_3', 
                        label='Total daily ruxolitinib dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            helpText("E.g.: 20 mg bis in die (BID) = 40 mg total daily dose"),
            br(),
            radioButtons("rbc_3", 
                         label = "RBC transfusions",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
            "...since baseline."
        )),
        column(4,wellPanel(
            h4("Data at 6 months"),
            hr(),
            numericInput('spleen_6', 
                         label='Spleen size (cm below LCM)', 
                         min=0, max=spleen.max, 
                         step=0.1,
                         value=0),
            br(),
            sliderInput('rux_6', 
                        label='Total daily ruxolitinib dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            helpText("E.g.: 20 mg bis in die (BID) = 40 mg total daily dose"),
            br(),
            radioButtons("rbc_6", 
                         label = "RBC transfusions",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
            "...since the 3-month visit.",
        )),
        
    ),
    
    hr(),
    
    fluidRow(
        column(5,
               wellPanel(
                   h3("RR6 model results"),
                   p("Risk stratum:", strong(textOutput("total.score.class.text", inline=T), "-",  
                                             strong(textOutput("total.score.label.text", inline=T)))),
                   p("Median survival:", 
                     strong(textOutput("total.score.medianos.text", inline=T), "months")),
                   p("Median survival 95% CI:",
                     strong(textOutput("total.score.medianci.text", inline=T), "months")), 
               )),
        column(3,
               h4("Calculation"),
               span("Risk points for spleen: ", strong(textOutput("spleen.score.text", inline=T))),
               br(),
               span("Risk points for rux dose: ", strong(textOutput("dose.score.text", inline=T))),
               br(),
               span("Risk points for transfusion: ", strong(textOutput("transfusion.score.text", inline=T))),
               p("Sum of risk points: ", strong(textOutput("total.score.score.text", inline=T))),
        ),
        column(3,offset=1,
               h4("Legend"),
               span(strong("RBC"),"- red blood cell"),
               br(),
               span(strong("LCM"), "- left costal margin")
        )
    ),
    
    hr(),
    h4("Survival curves"),
    p("Actuarial survival curves of the 3 risk groups of patients according to the",
    em("Response to Ruxolitinib after 6 months"),
    "(RR6) developed in ruxolitinib-treated myelofibrosis patients (training cohort)."),
    p("Source: Maffioli et al., A Prognostic Model to Predict Survival After 6 Months of Ruxolitinib in Patients with Myelofibrosis. (Under review)"),
    br(),
    img(src="km-curve.jpeg")
    
)


# LR  Median NR
# IR  61 mo   43-80 mo
# HR  33 mo   21-50 mo


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    spl.score <- function() {
        low.spl.resp.3 <- (input$spleen_0-input$spleen_3)/input$spleen_0 < .3
        low.spl.resp.6 <- (input$spleen_0-input$spleen_6)/input$spleen_0 < .3
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
            ms <- "NR"
            ci <- "NA"
        } else if(s <= 2 ) {
            gs <- "IR"
            gl <- "Intermediate"
            ms <- "61"
            ci <- "43-80"
        } else {
            gs <- "HR"
            gl <- "High"
            ms <- "33"
            ci <- "21-50"
        }
        list(score=s, class=gs, label=gl,
             medianos=ms, medianci=ci)
    }
    
    output$spleen.score.text <- renderText(spl.score())
    output$dose.score.text <- renderText(dose.score())
    output$transfusion.score.text <- renderText(transfusion.score())
    
    output$total.score.score.text <- renderText(total.score()$score)
    output$total.score.class.text <- renderText(total.score()$class)
    output$total.score.label.text <- renderText(total.score()$label)
    
    output$total.score.medianos.text <- renderText(total.score()$medianos)
    output$total.score.medianci.text <- renderText(total.score()$medianci)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
