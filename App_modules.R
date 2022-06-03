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
source("modules/module_functional_characterization.R")

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
        
        #= TABS
        tabItems(
            
            #== HOME TAB
            tabItem(tabName = "home",
                    
                    column(width = 8),
                    column(width = 4)
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
                     # Module sincle cell plotter
                     #print(rv$experiment_id)
                    
                     
                 }
    )
    
    # Parse the genes for functional characterization and load data from selected experiment
    a<-functional_characterizationServer("fc",
                                          experiment_path = x$experiment_path(),
                                          user_genelist = x$user_genelist(),
                                          specie = x$specie())
    
    #== PRESSING FUNCTIONAL CHARACTERIZATION BUTTON -> Depending on the gene seet selected (not enriched genes, not found genes or genes enriched in any tissue)
    ## Not enriched button
    observeEvent(ui$func_char_notenr(),
                 {
                   # Update the tabs menu and redirect to results page
                   output$dynamic_tabs <- renderMenu({
                     sidebarMenu(
                       # Menu item
                       menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table", lib = "font-awesome"))
                     )
                   })
                   # Move to Enrichment Results tab
                   updateTabItems(session = session,
                                  inputId = "tabs",
                                  selected = "functional_char")
                   # Genes not enriched
                   b<-not_enriched_Server("fc",
                                          parsed_genelist = a$parsed_genelist,
                                          tissue_atlas = a$tissue_atlas)
                   # Table and plots
                   c<-table_plots_funct_charact("fc",
                                                gene_set = b$gene_set,
                                                specie = x$specie())

                 }
    )
    ## Not found button
    observeEvent(ui$func_char_notfound(),
                 {
                   # Update the tabs menu and redirect to results page
                   output$dynamic_tabs <- renderMenu({
                     sidebarMenu(
                       # Menu item
                       menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table", lib = "font-awesome"))
                     )
                   })
                   # Move to Enrichment Results tab
                   updateTabItems(session = session,
                                  inputId = "tabs",
                                  selected = "functional_char")
                   # Genes not found
                   b<-not_found_Server("fc",
                                       parsed_genelist = a$parsed_genelist,
                                       geneuniverse = a$geneuniverse)
                   # Table and plots
                   c<-table_plots_funct_charact("fc",
                                                gene_set = b$gene_set,
                                                specie = x$specie())
                   
                 }
    )
    ## Enriched in any tissue button
    observeEvent(ui$func_char_tiss(),
                 {
                   # Update the tabs menu and redirect to results page
                   output$dynamic_tabs <- renderMenu({
                     sidebarMenu(
                       # Menu item
                       menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table", lib = "font-awesome"))
                     )
                   })
                   # Move to Enrichment Results tab
                   updateTabItems(session = session,
                                  inputId = "tabs",
                                  selected = "functional_char")
                   # Genes enriched
                   b<-tissue_enr_Server("fc",
                                        parsed_genelist = a$parsed_genelist,
                                        tissue_atlas = a$tissue_atlas,
                                        tissue_finder = tissue)  ## For functional characterization of genes enriched in a given tissue, tissue_finder needs to be in y !!!!!
                   # Table and plots
                   c<-table_plots_funct_charact("fc",
                                                gene_set = b$gene_set,
                                                specie = x$specie())
                   
                 }
    )
   
}

shinyApp(ui, server)
