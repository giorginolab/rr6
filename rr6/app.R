

library(shiny)

spleen.max <- 30
rux.help <- "E.g.: 20 mg BID = 40 mg total daily dose"

ui <- fluidPage(
    title = "RR6 Model",
    
    titlePanel("RR6 Calculator"),
    p("The RR6 model predicts survival in myelofibrosis based on clinical response after 6 months of ruxolitinib."),
    p("Reference: M. Maffioli et al., A Prognostic Model to Predict Survival After 6 Months of Ruxolitinib in Patients with Myelofibrosis. (Under review)."),
    div(em("IMPORTANT: This tool is for educational use only. It does not constitute medical advice. It should not be used for medical diagnosis and/or medical treatment."),style="font-size: smaller"),
    
    hr(),
    
    fluidRow(
        column(4,wellPanel(
            h4("Data at baseline"),
            helpText("Evaluation at ruxolitinib start \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0"),
            hr(),
            numericInput('spleen_0', 
                         label='Spleen length (cm below LCM)', 
                         min=5, max=spleen.max, 
                         step=0.1,
                         value=5),
            br(),
            sliderInput('rux_0', 
                        label='Total daily ruxolitinib dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            helpText(rux.help),
            br(),
            radioButtons("rbc_0", 
                         label = "RBC transfusions in the previous 3 months",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
        )),
        column(4,wellPanel(
            h4("Data at 3 months"),
            helpText("Evaluation 3 months after ruxolitinib start"),
            hr(),
            numericInput('spleen_3', 
                         label='Spleen length (cm below LCM)', 
                         min=0, max=spleen.max, 
                         step=0.1,
                         value=0),
            br(),
            sliderInput('rux_3', 
                        label='Total daily ruxolitinib dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            helpText(rux.help),
            br(),
            radioButtons("rbc_3", 
                         label = "RBC transfusions since baseline",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
        )),
        column(4,wellPanel(
            h4("Data at 6 months"),
            helpText("Evaluation 6 months after ruxolitinib start"),
            hr(),
            numericInput('spleen_6', 
                         label='Spleen length (cm below LCM)', 
                         min=0, max=spleen.max, 
                         step=0.1,
                         value=0),
            br(),
            sliderInput('rux_6', 
                        label='Total daily ruxolitinib dose (mg)',
                        min=0, max=50, step=5,
                        value=0),
            helpText(rux.help),
            br(),
            radioButtons("rbc_6", 
                         label = "RBC transfusions since the 3-month visit",
                         choices = list("not performed" = 0,
                                        "performed" = 1), 
                         selected = 0),
        )),
        
    ),
    
    hr(),
    
    fluidRow(
        column(5,
               wellPanel(
                   h3("RR6 model results"),
                   p("Risk stratum:", strong(textOutput("total.score.class.text", inline=T), "-",  
                                             strong(textOutput("total.score.label.text", inline=T)))),
                   p("Median overall survival*:", 
                     strong(textOutput("total.score.medianos.text", inline=T), "months")),
                   p("Median o. survival* 95% CI:",
                     strong(textOutput("total.score.medianci.text", inline=T), "months")), 
                   helpText("*counted from 6 months post ruxolitinib start.")
               )),
        column(3,
               h4("Calculation"),
               span("Risk points:"),
               tag("ul", list(
                   tag("li",list(
                       "spleen length reduction ≤30%: ", strong(textOutput("spleen.score.text", inline=T))),
                   ),
                   tag("li",list(
                       "low ruxolitinib dose: ", strong(textOutput("dose.score.text", inline=T))),
                   ),
                   tag("li",list(
                       "transfusion status: ", strong(textOutput("transfusion.score.text", inline=T))),
                   ))),
               p("Total risk points: ", strong(textOutput("total.score.score.text", inline=T))),
        ),
        column(3,offset=1,
               h4("Legend"),
               span(strong("LCM"), "- left costal margin"),
               br(),
               span(strong("BID"), "- bis in die, twice daily"),
               br(),
               span(strong("RBC"),"- red blood cell"),
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
