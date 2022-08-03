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
library(gprofiler2)
library(clusterProfiler)
library(enrichplot)
library(ggplot2)

source("modules/module_experiment_selector.R")
source("modules/module_enrichment_results.R")
source("modules/module_colorSVG.R")
source("modules/module_enrichment_pageUI.R")
source("modules/module_gene_classifier.R")
source("modules/module_home_pageUI.R")
source("modules/module_reset_tab.R")
source("modules/module_functional_characterization_pageUI.R")
source("modules/module_functional_characterization.R")
source("modules/module_single_cell_pageUI.R")
source("modules/module_single_cell.R")
source("modules/module_about.R")

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
            menuItem(text = "About",tabName = "about",icon = icon("info", lib = "font-awesome")),
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

                    ),
            
            #== SINGLE CELL TAB
            tabItem(tabName = "single_cell",
                    
                    single_cellUI("sc")
                    
                   ),
            
            #== ABOUT TAB
            tabItem(tabName = "about",
                    
                    aboutUI("ab")
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
                     
                     ## Create reactiveValues for functional characterization and single cell buttons
                     fc_button <- reactiveValues(func_char_tiss = NULL) # initialise reactiveValues
                     sc_button <- reactiveValues(single_cell_atlas = NULL) # initialise reactiveValues
                     
                     ## Calculate enrichments
                     y<-enrichment_resultsServer(id = "ui",
                                                 experiment_path = x$experiment_path(),
                                                 user_genelist = x$user_genelist(),
                                                 user_description = x$user_description(),
                                                 fc_button = fc_button,
                                                 sc_button = sc_button)
                     
                  
                     # EXECUTE MODULE COLOR SVG
                     z<-colorSVG_Server(id = "ui",
                                       experiment_path = x$experiment_path(),
                                       user_description = x$user_description(),
                                       experiment_id = x$experiment_id(),
                                       enrichment_values = y$enrichment_values())
                     
                     # Module gene classifier
                     zz<-gene_classifierServer(id = "ui",
                                           experiment_path = x$experiment_path(),
                                           user_genelist = x$user_genelist())
                     
                     
                     #== PRESSING FUNCTIONAL CHARACTERIZATION BUTTON
                     observeEvent(fc_button$func_char_tiss,
                                  {
                                    # Update the tabs menu and redirect to funct. char. page
                                    output$dynamic_tabs <- renderMenu({
                                      sidebarMenu(
                                        # Separator and identifier -> IF not description is provided, change for date-time
                                        h4("   Functional characterization"),
                                        # Menu item
                                        menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table", lib = "font-awesome"))
                                      )
                                    })
                                    # Move to Functional characterization tab
                                    updateTabItems(session = session,
                                                   inputId = "tabs",
                                                   selected = "functional_char")

                                    functional_characterizationServer("fc",
                                                                      experiment_path = x$experiment_path(),
                                                                      user_description = x$user_description(),
                                                                      specie = x$specie(),
                                                                      gene_set = zz$gene_set(),
                                                                      tissue = zz$selected_tissue())
                                  }
                     )
                     
                     #== PRESSING SINGLE CELL BUTTON -> it only works when a SingleCell experiment is selected, if not, it returns a warning
                     observeEvent(sc_button$single_cell_atlas,
                                      {
                                        # Update the tabs menu and redirect to single cell page
                                        output$dynamic_tabs <- renderMenu({
                                          sidebarMenu(
                                            # Separator and identifier -> IF not description is provided, change for date-time
                                            h4("   Single cell atlas"),
                                            # Menu item
                                            menuItem(text = "Single cell atlas",tabName = "single_cell",icon = icon("bar-chart-o", lib = "font-awesome"))
                                          )
                                        })
                                        # Move to single cell tab
                                        updateTabItems(session = session,
                                                       inputId = "tabs",
                                                       selected = "single_cell")
                                        
                                        
                                        single_cellServer(id="sc",
                                                          user_description = x$user_description(),
                                                          experiment_id=x$experiment_id(),
                                                          specie = x$specie(),
                                                          gene_set = zz$gene_set(),
                                                          tissue = zz$selected_tissue())
                                      
                      
                     })

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
    
    #== PRESSING ABOUT BUTTON
    aboutServer("ab")
}

shinyApp(ui, server)
