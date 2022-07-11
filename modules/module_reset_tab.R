##======================##
## MODULE:              ##
## Reset tab            ##
##======================##

## DESCRIPTION ##
# When pressed again new search tab
# Display a warning message
# If OK, reset dynamic tabs

reset_tabServer <- function(id) {
    
    moduleServer(id, function(input,output,session) {
        
        shinyalert(title = "NEW SEARCH",
                   text = "Runing a new search will discard current results\n Do you want to continue?",
                   type = "warning",
                   showCancelButton = TRUE,showConfirmButton = TRUE,confirmButtonCol = "#09C3A2",
                   callbackR = function(x) {
                       if (x != FALSE) {
                           updateTabItems(session = session,
                                          inputId = "tabs",
                                          selected = "results")
                       } else {
                           
                           output$tabs <- renderMenu({
                               sidebarMenu(id = "tabs",
                                           menuItem(text = "Home", tabName = "home", icon = icon("home",lib = "font-awesome")),
                                           menuItem(text = "New search",tabName = "new_search",icon = icon("database", lib = "font-awesome")),
                                           menuItem("About",tabName = "about",icon = icon("info", lib = "font-awesome")),
                                           sidebarMenuOutput(outputId = "dynamic_tabs")
                               )
                           })
                           updateTabItems(session = session,
                                          inputId = "tabs",
                                          selected = "new_search")
                       }
                   }
        )
    })
}