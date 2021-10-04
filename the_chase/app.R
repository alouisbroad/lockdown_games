library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(pool)
library(DT)
library(lubridate)
library(plotly)
`%>%` <- magrittr::`%>%`    # Used for piping.
`%m-%` <- lubridate::`%m-%` # For subtracting dates
`%!in%` <- purrr::negate(`%in%`)



# User interface ####
ui <- dashboardPage(
    
    #### Header ####
    # dashboardHeader and dashboardSidebar UI's are defined in user_menu/_user_menu.R.
    # dashboardBody is defined in user_menu/dashboard_body.R.
    # they conditionally appear depending on if the user is logged in
    dashboardHeader(
        title="Games With Friends",
        dropdownMenuOutput("user_menu")
    ),
    
    
    #### Side Bar ####
    dashboardSidebar(
        sidebarMenu(
            menuItem("Questions", tabName = "page1", icon = icon("pound-sign")),
            menuItem("Challenge", icon = icon("question-circle"), tabName = "page2"),
            menuItem("Showdown", icon = icon("money-bill-wave"), tabName = "page3")
        )
    ),
    
    
                                    
    #### Dashboard Body ####
    dashboardBody(useShinyjs(),
                  # adding mod purple colour to dashboard taken from this Stack Overflow answer:
                  # https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
                  tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #6ba4ff;
                              }
                             
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #6ba4ff;
                              }
                             
                              .myClass {
                              font-size: 20px;
                              line-height: 50px;
                              text-align: right;
                              font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                              padding: 0 15px;
                              overflow: hidden;
                              color: white;
                              }
                             
                              '))),
                  
                  tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass"> The Chase </span>\');
                   })
                   ')),
                  
                  tabItems(
                      #### Page 1 ####
                      tabItem(tabName = "page1",
                              fluidPage(
                                h1("Questions Round", style = "font-size: 60px; font-style: bold;"),
                                fluidRow(div(style = "height:50px;"),
                                    column(width = 3,
                                           tags$head(tags$style("#timeleft_timer1{color: black; font-size: 50px; font-style: bold;}")),
                                           textOutput('timeleft_timer1')),
                                    column(width = 3,
                                           actionButton('start_timer1','Start', 
                                                        style="width:200px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                    column(width = 3,
                                           actionButton('stop_timer1','Stop', 
                                                        style="width:200px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                    column(width = 3,
                                           actionButton('reset_timer1','Reset Timer', 
                                                        style="width:200px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                
                                br(),
                                fluidRow(
                                    column(width = 4,
                                           tags$head(tags$style("#counter_page1{color: black; font-size: 50px; font-style: bold;}")),
                                           textOutput('counter_page1')),
                                    column(width = 4,
                                           actionButton('correct_counter_page1','Correct',
                                                        style="width:200px; height:80px; color: #fff; background-color: #00a82a; border-color: #00d134")),
                                    column(width = 4,
                                           actionButton('reset_counter_page1','Reset Counter',
                                                        style="width:200px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4"))))
                              
                              
                              
                      ),
                      #### Page 2 ####
                      tabItem(tabName = "page2",
                              h1("Challenge the Chaser", style = "font-size: 60px; font-style: bold;"),
                              br(),
                              fluidPage(
                                  fluidRow(
                                      column(width = 4,
                                             actionButton('op1','Option 1', 
                                                          style="width:400px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                      ),
                                      column(width = 4,
                                             actionButton('op2','Option 2', 
                                                          style="width:400px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                      ),
                                      column(width = 4,
                                             actionButton('op3','Option 3', 
                                                          style="width:400px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                      )
                                  ),
                                  fluidRow(
                                    column(width = 10,
                                      plotlyOutput("board1"),
                                      plotlyOutput("board2"),
                                      plotlyOutput("board3")),
                                    column(width = 2,
                                           fluidRow(
                                               column(width = 2,
                                                      div(style = "height:200px;"),
                                                      actionButton('con_correct','Contestant Correct',
                                                                   style="width:200px; height:400px; color: #fff; background-color: #00a82a; border-color: #00d134")
                                                      )),
                                           br(),
                                           fluidRow(
                                               column(width = 2,
                                                      actionButton('ch_correct','Chaser Correct',
                                                                   style="width:200px; height:400px; color: #fff; background-color: #d41a0d; border-color: #f21e0f")))
                                      )
                                  ),
                                  fluidRow(
                                    actionButton('board_reset','Reset', 
                                                 style="width:400px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  )
                              )
                              
                              
                      ),
                      #### Page 3 ####
                      tabItem(tabName = "page3",
                              fluidPage(
                                  h1("Showdown", style = "font-size: 60px; font-style: bold;"),
                                  fluidRow(
                                      column(width = 4,
                                             align="center",
                                             actionButton(inputId = 'start_timer2','Start', 
                                                          style="width:400px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                             ),
                                      column(width = 4,
                                             align="center",
                                             actionButton('correct_counter_page3','Correct',
                                                          style="width:400px; height:80px; color: #fff; background-color: #00a82a; border-color: #00d134"),
                                      ),
                                      column(width = 4,
                                             align="center",
                                             actionButton(inputId = 'stop_timer2','Incorrect',
                                                          style="width:400px; height:80px; color: #fff; background-color: #d41a0d; border-color: #f21e0f"),
                                      )
                                  ),
                                  br(),
                                  br(),
                                  fluidRow(
                                      column(width = 12,
                                             align="center",
                                             actionButton('pushback_counter_page3','Push Back', 
                                                          style="width:400px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      ),
                                  ),
                                  br(),
                                  br(),
                                  fluidRow(
                                      column(width = 6,
                                             align="center",
                                             tags$head(tags$style("#timeleft_timer2{color: black; font-size: 100px; font-style: bold;}")),
                                             textOutput('timeleft_timer2'),
                                      ),
                                      column(width = 6,
                                             align="center",
                                             tags$head(tags$style("#counter_page3{color: black; font-size: 100px; font-style: bold;}")),
                                             textOutput('counter_page3'),
                                      )
                                  ),
                                  br(),
                                  br(),
                                  fluidRow(
                                      column(width = 2,
                                             div(style = "height:200px;"),
                                             actionButton(inputId = 'reset_timer2','Reset Timer', 
                                                          style="width:200px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      ),
                                      column(width = 2,
                                             div(style = "height:200px;"),
                                             actionButton('reset_counter_page3','Reset Counter', 
                                                          style="width:200px; height:80px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                      )
                                  )
                              
                              )
                      )
                  )
    )
)



# Server code ####

server <- function(input, output, session) {
    
    #### Page 1 ####
    # Initialize the timer, 60 seconds, not active.
    timer <- reactiveVal(60)
    active <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft_timer1 <- renderText({
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
                        "Round Over!",
                        title = "Time's Up!",
                        size = "l"
                        
                        
                        
                        
                    ))
                }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start_timer1, {active(TRUE)})
    observeEvent(input$stop_timer1, {active(FALSE)})
    observeEvent(input$reset_timer1, {timer(as.numeric(60))})
    
    correct_question_counter <- reactiveValues(countervalue = 0) 
    observeEvent(input$correct_counter_page1, {correct_question_counter$countervalue <- correct_question_counter$countervalue + 1})
    observeEvent(input$reset_counter_page1, {correct_question_counter$countervalue <- 0})
    
    output$counter_page1 <- renderText({
        paste("Counter: ", correct_question_counter$countervalue)
    })
    
    #### Page 2 ####
    
    game_board <- reactiveValues(picked = 0,
                                 player = 4,
                                 empty_space = 2,
                                 chaser = 1,
                                 win = 0,
                                 lose = 0)
    
    
    
    observeEvent(game_board$picked, {
        if(game_board$picked == 1){
            game_board$player <- 4
            game_board$empty_space <- 3
            game_board$chaser <- 1
        }
        else if(game_board$picked == 2){
            game_board$player <- 5
            game_board$empty_space <- 2
            game_board$chaser <- 1
        }
        else if(game_board$picked == 3){
            game_board$player <- 6
            game_board$empty_space <- 1
            game_board$chaser <- 1
        }


    })
    
    observeEvent(input$con_correct, {
        game_board$player  <-  game_board$player - 1
        game_board$empty_space <- game_board$empty_space + 1
        if(game_board$player == 0){
            game_board$empty_space <- 0
            game_board$chaser <- 0
            game_board$win <- 1
            game_board$lose <- 0
            
            showModal(modalDialog(
                title = div("You Made It!", style="font-size:500%; font-style: bold;"),
                size = "l"
            ))
        }

        
    })
    
    observeEvent(input$ch_correct, {
        game_board$empty_space <- game_board$empty_space - 1
        game_board$chaser  <-  game_board$chaser + 1
        if(game_board$empty_space == -1){
            game_board$player <- 0
            game_board$empty_space <- 0
            game_board$chaser <- 0
            game_board$lose <- 1
            
            showModal(modalDialog(
                title = div("You’ve Been Caught!", style="font-size:500%; font-style: bold;"),
                size = "l"
            ))
        }
        
    })
    
    observeEvent(input$op1, {
        game_board$picked  <-  1
    })
    
    observeEvent(input$op2, {
        game_board$picked  <-  2
    })
    
    observeEvent(input$op3, {
        game_board$picked  <-  3
    })
    
    observeEvent(input$board_reset, {
        game_board$picked  <-  0
        game_board$win <- 0
        game_board$lose <- 0
    })
    
    ay <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
    )
    
    ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        gridcolor = toRGB("black"),
        gridwidth = 2
    )

    
    output$board1<-renderPlotly({
        if(game_board$picked == 0){
            y <- "game board"
            player <- 4
            empty_space <- 3
            chaser <- 1
            data <- data.frame(y, player, empty_space, chaser)
            
            fig <- plot_ly(data, x = ~player, y = ~y, type = 'bar', orientation = 'h', name = 'player',
                           marker = list(color = 'rgba(128, 223, 255, 0.6)',
                                         line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                     width = 3)))
            fig <- fig %>% add_trace(x = ~empty_space, name = 'empty space',
                                     marker = list(color = 'rgba(255, 255, 255, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~chaser, name = 'Chaser',
                                     marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = ""),
                                  yaxis = list(title ="")) %>% 
                style(hoverinfo = 'none') %>% 
                layout(showlegend = FALSE,
                       title = NULL,
                       grid = FALSE,
                       xaxis = ax, 
                       yaxis = ay)
            
            fig}
        else if(game_board$picked == 1){
            y <- "game board"
            data <- data.frame(y, game_board$player, game_board$empty_space, game_board$chaser, game_board$win, game_board$lose)
            
            fig <- plot_ly(data, x = ~game_board$player, y = ~y, type = 'bar', orientation = 'h', name = 'player',
                           marker = list(color = 'rgba(128, 223, 255, 0.6)',
                                         line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                     width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$empty_space, name = 'empty space',
                                     marker = list(color = 'rgba(255, 255, 255, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$chaser, name = 'Chaser',
                                     marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$win, name = 'Win',
                                     marker = list(color = 'rgba(0, 214, 53, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$lose, name = 'Lose',
                                     marker = list(color = 'rgba(219, 4, 0, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = ""),
                                  yaxis = list(title ="")) %>% 
                style(hoverinfo = 'none') %>% 
                layout(showlegend = FALSE,
                       title = NULL,
                       grid = FALSE,
                       xaxis = ax, 
                       yaxis = ay)
            
            fig}
        else if(game_board$picked == 2){}
        else if(game_board$picked == 3){}
    })
    
    output$board2<-renderPlotly({
        if(game_board$picked == 0){
            y <- "game board"
            player <- 5
            empty_space <- 2
            chaser <- 1
            data <- data.frame(y, player, empty_space, chaser)
            
            fig <- plot_ly(data, x = ~player, y = ~y, type = 'bar', orientation = 'h', name = 'player',
                           marker = list(color = 'rgba(128, 223, 255, 0.6)',
                                         line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                     width = 3)))
            fig <- fig %>% add_trace(x = ~empty_space, name = 'empty space',
                                     marker = list(color = 'rgba(255, 255, 255, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~chaser, name = 'Chaser',
                                     marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = ""),
                                  yaxis = list(title ="")) %>% 
                style(hoverinfo = 'none') %>% 
                layout(showlegend = FALSE,
                       title = NULL,
                       grid = FALSE,
                       xaxis = ax, 
                       yaxis = ay)
            
            fig}
        else if(game_board$picked == 1){}
        else if(game_board$picked == 2){
            y <- "game board"
            data <- data.frame(y, game_board$player, game_board$empty_space, game_board$chaser, game_board$win, game_board$lose)
            
            fig <- plot_ly(data, x = ~game_board$player, y = ~y, type = 'bar', orientation = 'h', name = 'player',
                           marker = list(color = 'rgba(128, 223, 255, 0.6)',
                                         line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                     width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$empty_space, name = 'empty space',
                                     marker = list(color = 'rgba(255, 255, 255, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$chaser, name = 'Chaser',
                                     marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$win, name = 'Win',
                                     marker = list(color = 'rgba(0, 214, 53, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$lose, name = 'Lose',
                                     marker = list(color = 'rgba(219, 4, 0, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = ""),
                                  yaxis = list(title ="")) %>% 
                style(hoverinfo = 'none') %>% 
                layout(showlegend = FALSE,
                       title = NULL,
                       grid = FALSE,
                       xaxis = ax, 
                       yaxis = ay)
            
            fig
        }
        else if(game_board$picked == 3){}
    })
    
    output$board3<-renderPlotly({
        if(game_board$picked == 0){
            y <- "game board"
            player <- 6
            empty_space <- 1
            chaser <- 1
            data <- data.frame(y, player, empty_space, chaser)
            
            fig <- plot_ly(data, x = ~player, y = ~y, type = 'bar', orientation = 'h', name = 'player',
                           marker = list(color = 'rgba(128, 223, 255, 0.6)',
                                         line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                     width = 3)))
            fig <- fig %>% add_trace(x = ~empty_space, name = 'empty space',
                                     marker = list(color = 'rgba(255, 255, 255, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~chaser, name = 'Chaser',
                                     marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = ""),
                                  yaxis = list(title ="")) %>% 
                style(hoverinfo = 'none') %>% 
                layout(showlegend = FALSE,
                       title = NULL,
                       grid = FALSE,
                       xaxis = ax, 
                       yaxis = ay)
            
            fig}
        else if(game_board$picked == 1){}
        else if(game_board$picked == 2){}
        else if(game_board$picked == 3){
            y <- "game board"
            data <- data.frame(y, game_board$player, game_board$empty_space, game_board$chaser, game_board$win, game_board$lose)
            
            fig <- plot_ly(data, x = ~game_board$player, y = ~y, type = 'bar', orientation = 'h', name = 'player',
                           marker = list(color = 'rgba(128, 223, 255, 0.6)',
                                         line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                     width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$empty_space, name = 'empty space',
                                     marker = list(color = 'rgba(255, 255, 255, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$chaser, name = 'Chaser',
                                     marker = list(color = 'rgba(246, 78, 139, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$win, name = 'Win',
                                     marker = list(color = 'rgba(0, 214, 53, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% add_trace(x = ~game_board$lose, name = 'Lose',
                                     marker = list(color = 'rgba(219, 4, 0, 0.6)',
                                                   line = list(color = 'rgba(0, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = ""),
                                  yaxis = list(title ="")) %>% 
                style(hoverinfo = 'none') %>% 
                layout(showlegend = FALSE,
                       title = NULL,
                       grid = FALSE,
                       xaxis = ax, 
                       yaxis = ay)
            
            fig
        }
        
    })
    

    
    #### Page 3 ####
    # Initialize the timer, 120 seconds, not active.
    timer_fin <- reactiveVal(120)
    active_fin <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft_timer2 <- renderText({
        paste("Time left: ", seconds_to_period(timer_fin()))
    })
    
    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
        invalidateLater(1000, session)
        isolate({
            if(active_fin())
            {
                timer_fin(timer_fin()-1)
                if(timer_fin()<1)
                {
                    active_fin(FALSE)
                    showModal(modalDialog(
                        "Round Over!",
                        title = "Time's Up!",
                        size = "l"
                        
                        
                        
                        
                    ))
                }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start_timer2, {active_fin(TRUE)})
    observeEvent(input$stop_timer2, {active_fin(FALSE)})
    observeEvent(input$reset_timer2, {timer_fin(as.numeric(120))})

    correct_showdown_counter <- reactiveValues(countervalue = 0) 
    observeEvent(input$correct_counter_page3, {correct_showdown_counter$countervalue <- correct_showdown_counter$countervalue + 1})
    observeEvent(input$pushback_counter_page3, {correct_showdown_counter$countervalue <- correct_showdown_counter$countervalue - 1})
    observeEvent(input$reset_counter_page3, {correct_showdown_counter$countervalue <- 0})
    
    output$counter_page3 <- renderText({
        paste("Counter: ", correct_showdown_counter$countervalue)
    })
}



shinyApp(ui, server)