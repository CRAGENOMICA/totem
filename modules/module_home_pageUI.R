##======================##
## MODULE:              ##
## Home page UI         ##
##======================##


## DESCRIPTION ##
# User Interface for variable selection
# Reactive UI
# New experiment tab
# Module 1

homeUI <- function(id) {
    
    fluidPage(
        
      htmlOutput(NS(id, "home_info")),
        
        hr(),
      
    )
}


homeServer <- function(id) {
  
  moduleServer(id, function(input,output,session) {
    getPage<-function() {
      return(includeHTML("./www/home_tab.html"))
    }
    output$home_info<-renderUI({getPage()})

    
  })
}