acUI <- function(id){
  
  ns <- NS(id)
  
  
  
  fluidPage(fluidRow(
    
    dgUI(ns("dg"))
    
  ))
  
  
  
}



ac <- function(input, output, session){
  
  callModule(dg, "dg")
  
  
  
}