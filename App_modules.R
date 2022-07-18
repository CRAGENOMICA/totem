## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: App modules
##
## Purpose of script:
##
## Author: Fidel Lozano-Elena
##
## Date Created: 2022-05-25
##
## Copyright (c) Fidel Lozano, 2022
## Email: fidel.lozano@cragenomica.es
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Notes:
##   
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%


library(shinydashboard)
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinycssloaders)
library(xml2)
library(rsvg) 
library(DT)

source("modules/module_experiment_selector.R")
source("modules/module_enrichment_results.R")
source("modules/module_colorSVG.R")
source("modules/module_enrichment_pageUI.R")
source("modules/module_gene_classifier.R")
source("modules/module_home_pageUI.R")
source("modules/module_reset_tab.R")

ui <- dashboardPage(
    
    # HEADER
    dashboardHeader(title = tags$img(alt="TOTEM Logo", src="totem_banner_trans.png",
                                     height="100%", width="100%", align="left")
    ),
    
    # SIDEBAR
    dashboardSidebar(
        # Dynamic Sidebar menu
        sidebarMenu(
            id = "tabs",
            menuItem(text = "Home", tabName = "home", icon = icon("home",lib = "font-awesome")),
            menuItem(text = "New search",tabName = "new_search",icon = icon("database", lib = "font-awesome")),
            menuItem("About",tabName = "about",icon = icon("info", lib = "font-awesome")),
            sidebarMenuOutput(outputId = "dynamic_tabs")
        )
    ),
    
    # BODY
    dashboardBody(
        # Include the custom styling
        tags$head(
            includeCSS(path = "www/my_style2.css")
        ),
        tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
        
        #== TABS
        tabItems(
            
            #== HOME TAB
            tabItem(tabName = "home",
                    
                    column(width = 8, offset = 1,
                           
                           homeUI("x")
                           ),
                    column(width = 2, offset = 1)
            ),
            
            #== NEW SEARCH TAB
            tabItem(tabName = "new_search",
                    
                    experiment_selectorUI("x")
                    
                    ),
            
            #== RESULTS TAB
            tabItem(tabName = "results",
                    
                    enrichment_pageUI("ui")
                    
                    ),
            
            #== FUNCTIONAL CHARACTERIZATION TAB
            tabItem(tabName = "functional_char",

                    functional_characterizationUI("fc")

                    )
            )
        )
    )

server<-function(input,output,session) {
    
    #== NEW SEARCH TAB
    x<-experiment_selectorServer("x")

    #== PRESSING SUBMIT BUTTON:
    observeEvent(x$submit(),
                 {
                     # Update the tabs menu and redirect to results page
                     output$dynamic_tabs <- renderMenu({
                         sidebarMenu(
                             # Separator and identifier -> IF not description is provided, change for date-time
                             h4("   Enrichment Results"),
                             # Menu item
                             menuItem(text = "Enrichment results",tabName = "results",icon = icon("seedling", lib = "font-awesome"))
                         )
                     })
                     # Move to Enrichment Results tab
                     updateTabItems(session = session,
                                    inputId = "tabs",
                                    selected = "results")
                     
                     # EXECUTE MODULE ENRICHMENT
                     y<-enrichment_resultsServer(id = "ui",
                                                 experiment_path = x$experiment_path(),
                                                 user_genelist = x$user_genelist())
                     
                     # EXECUTE MODULE COLOR SVG
                     z<-colorSVG_Server(id = "ui",
                                       experiment_path = x$experiment_path(),
                                       experiment_id = x$experiment_id(),
                                       enrichment_values = y$enrichment_values())
                     
                     # Module gene classifier
                     gene_classifierServer(id = "ui",
                                           experiment_path = x$experiment_path(),
                                           user_genelist = x$user_genelist())
                     
                 }
    )
    
    
    #== PRESSING NEW SEARCH AGAIN
    previous_experiment <<-FALSE
    observeEvent(input$tabs, {
        
        if (input$tabs == "results") {
            
            previous_experiment <<-TRUE
            
        } else if (previous_experiment == TRUE & input$tabs == "new_search") {
            
            shinyalert(title = "NEW SEARCH",
                       text = "Runing a new search will discard current results\n Do you want to continue?",
                       type = "warning",
                       showCancelButton = TRUE,showConfirmButton = TRUE,confirmButtonCol = "#09C3A2",
                       callbackR = function(x) {
                           
                           if (x == FALSE) {
                               updateTabItems(session = session,
                                              inputId = "tabs",
                                              selected = "results")
                           } else {
                               
                               removeTab(inputId = "tabs",target = "results",session = session)
                               
                               updateTabItems(session = session,
                                              inputId = "tabs",
                                              selected = "new_search")
                               
                               previous_experiment <<- FALSE
                           }
                       }
            )
        }
    })
    
    #== PRESSING NEW SEARCH AGAIN
    
   
}

shinyApp(ui, server)
