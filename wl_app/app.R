library(shiny)
library(lubridate)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
 
    shinyjs::useShinyjs(),
    tags$style(".fa-arrow-circle-left {color:#c20000}"),
    HTML("<font size='12'>Weakest Link</font>"),
    br(),
    br(),
    column(width = 3,
           fluidRow(
               shinyjs::hidden(
                   div(
                       id = "one",
                       HTML("<font size='14'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><b>2p</b></font>"), br(),
                       HTML("<font size='14'>", "<font color='#c20000'><b>1p</b></font>"))),
               shinyjs::hidden(
                   div(
                       id = "two",
                       HTML("<font size='14'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><font color='#c20000'><b>2p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>1p</b></font>"))),
               shinyjs::hidden(
                   div(
                       id = "three",
                       HTML("<font size='14'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><font color='#c20000'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>2p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>1p</b></font>"))),
               shinyjs::hidden(
                   div(
                       id = "four",
                       HTML("<font size='14'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><font color='#c20000'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>2p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>1p</b></font>"))),
               shinyjs::hidden(
                   div(
                       id = "five",
                       HTML("<font size='14'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><font color='#c20000'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>2p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>1p</b></font>"))),
               shinyjs::hidden(
                   div(
                       id = "six",
                       HTML("<font size='14'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><font color='#c20000'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>2p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>1p</b></font>"))),
               shinyjs::hidden(
                   div(
                       id = "seven",
                       HTML("<font size='14'><font color='#c20000'><b>£1.00</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>50p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>20p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>10p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>5p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>2p</b></font>"), br(),
                       HTML("<font size='14'><font color='#a6a6a6'><b>1p</b></font>")))
           ),
           fluidRow(
               hr(),
               HTML("<font size='14'>Total: £</font>"),
               textOutput("total", inline = TRUE),
               br(),
               actionButton("ad", "Correct!"),
               br(),
               actionButton("bk", "Bank"),
               br(),
               actionButton("inc", "Incorrect"),
               br(),
               actionButton("reset", "Reset")
               
           )
    ),
    column(width = 2,
           shinyjs::hidden(
               div(
                   id = "state1",
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"))),
           shinyjs::hidden(
               div(
                   id = "state2",
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"),
                   br(),
               )),
           shinyjs::hidden(
               div(
                   id = "state3",
                   br(),
                   br(),
                   br(),
                   br(),
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"),
                   br(),
                   br(),
               )),
           shinyjs::hidden(
               div(
                   id = "state4",
                   br(),
                   br(),
                   br(),
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"),
                   br(),
                   br(),
                   br(),
               )),
           shinyjs::hidden(
               div(
                   id = "state5",
                   br(),
                   br(),
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"),
                   br(),
                   br(),
                   br(),
                   br(),
               )),
           shinyjs::hidden(
               div(
                   id = "state6",
                   br(),
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
               )),
           shinyjs::hidden(
               div(
                   id = "state7",
                   icon("arrow-circle-left", class = "fa-1x", "font-awesome"),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
               ))
           
    ),
    column(width = 7,
           HTML("<font size='5'>"),
           selectizeInput(inputId = "select_round", label = "Round Timer",
                          choices = c("Round 1" = 220, "Round 2" = 200, "Round 3" = 180,  "Round 4" = 160, "Round 5" = 140, "Round 6" = 120, "Round 7" = 90, "Voting" = 10),
                          selected = "Round 1"),
           actionButton('start','Start'),
           actionButton('stop','Stop'),
           actionButton('reset_timer','Set'),
           textOutput('timeleft')
           
           
    )
    # column(width = 8,
    #        HTML("<font size='4'</font>"),
    #        DT::DTOutput('tbl'),
    #        br(),
    #        textInput("text", h3("Weakest Link"),
    #                  value = NULL),
    #        actionButton("gb", "Goodbye!")
    # )
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    values <- reactiveValues(total = 0, state = 1)
    
    observeEvent(input$bk, {
        
        add <- dplyr::case_when(
            values$state == 1 ~ 0.01,
            values$state == 2 ~ 0.03,
            values$state == 3 ~ 0.08,
            values$state == 4 ~ 0.18,
            values$state == 5 ~ 0.38,
            values$state == 6 ~ 0.88,
            values$state == 7 ~ 1.88,
            TRUE ~ NA_real_
        )
        
        values$total <- values$total + add
        
        values$state <- 1
    })
    
    observeEvent(input$ad, {
        if(values$state < 7){
            values$state <- values$state + 1
        }
    })
    
    observeEvent(values$state, {
        shinyjs::hide(id = "one")
        shinyjs::hide(id = "two")
        shinyjs::hide(id = "three")
        shinyjs::hide(id = "four")
        shinyjs::hide(id = "five")
        shinyjs::hide(id = "six")
        shinyjs::hide(id = "seven")
        shinyjs::hide(id = "state1")
        shinyjs::hide(id = "state2")
        shinyjs::hide(id = "state3")
        shinyjs::hide(id = "state4")
        shinyjs::hide(id = "state5")
        shinyjs::hide(id = "state6")
        shinyjs::hide(id = "state7")
        
        if(values$state == 1){
            shinyjs::show(id = "one")
            shinyjs::show(id = "state1")}
        
        if(values$state == 2){
            shinyjs::show(id = "two")
            shinyjs::show(id = "state2")}
        
        if(values$state == 3){
            shinyjs::show(id = "three")
            shinyjs::show(id = "state3")}
        
        if(values$state == 4){
            shinyjs::show(id = "four")
            shinyjs::show(id = "state4")}
        
        if(values$state == 5){
            shinyjs::show(id = "five")
            shinyjs::show(id = "state5")}
        
        if(values$state == 6){
            shinyjs::show(id = "six")
            shinyjs::show(id = "state6")}
        
        if(values$state == 7){
            shinyjs::show(id = "seven")
            shinyjs::show(id = "state7")}
    })
    
    observeEvent(input$inc, {
        
        values$state <- 1
    })
    
    observeEvent(input$reset, {
        
        values$state <- 1
        values$total <- 0
    })
    
    output$total <- renderText({
        values$total
    })
    output$state <- renderText({
        values$state
    })
    
    # output$tbl = DT::renderDT(
    #     iris, options = list(lengthChange = FALSE)
    #     )
    
    # Initialize the timer, 180 seconds, not active.
    timer <- reactiveVal(180)
    active <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft <- renderText({
        paste("Time left: ", seconds_to_period(timer()))
    })
    
    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
        invalidateLater(1000, session)
        isolate({
            if(active())
            {
                timer(timer()-1)
                if(timer()<1)
                {
                    active(FALSE)
                    showModal(modalDialog(
                        if(input$select_round == "Voting"){
                            "Voting Over!"
                            
                        }
                        else{
                            "Round Over!"},
                        title = "Time's Up!",
                        size = "l"
                        
                            
                        
                        
                    ))
                }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start, {active(TRUE)})
    observeEvent(input$stop, {active(FALSE)})
    observeEvent(input$reset_timer, {timer(as.numeric(input$select_round))})
}

# Run the application 
shinyApp(ui = ui, server = server)
