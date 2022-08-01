##======================##
## MODULE:              ##
## ABOUT page UI         ##
##======================##


## DESCRIPTION ##
# Module for about tab: two functions: UI and server

aboutUI <- function(id) {
    
    fluidPage(
        
        htmlOutput(NS(id, "about_info"))
    )
    
}



aboutServer <- function(id) {
  
  moduleServer(id, function(input,output,session) {
    getPage<-function() {
      return(includeHTML("./www/about_tab.html"))
    }
    output$about_info<-renderUI({getPage()})
    
  })
}

about_App <- function(id) {
  
  ui <- fluidPage(
    
    aboutUI(id = "ab")
  )
  
  server<-function(input,output,session) {
    
    aboutServer(id = "ab")
  }
  
  shinyApp(ui, server)
}
about_App()
