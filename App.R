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
## Email: fidel.lozano@cragenomica.es / veredas.coleto@cragenomica.es
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
library(viridis)

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

# == UI PART ====
ui <- dashboardPage(
  #= HEADER ====
  dashboardHeader(
    title = tags$img(
      alt = "TOTEM Logo",
      src = "totem_banner_name_2.png",
      height = "100%",
      width = "100%",
      align = "left"
    )
  ),
  
  #= SIDEBAR ====
  dashboardSidebar(
    br(),
    # Dynamic Sidebar menu
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Home",
        tabName = "home",
        icon = icon("home", lib = "font-awesome")
      ),
      menuItem(
        text = "New search",
        tabName = "new_search",
        icon = icon("database", lib = "font-awesome")
      ),
      menuItem(
        text = "About",
        tabName = "about",
        icon = icon("info", lib = "font-awesome")
      ),
      # 1st tier of dynamic tabs:
      sidebarMenuOutput(outputId = "dynamic_tabs"),
      # 2nd tier of dynamic tabs:
      sidebarMenuOutput(outputId = "dynamic_tabs2")
    )
  ),
  
  #= BODY ====
  dashboardBody(
    # Include the custom styling
    tags$head(includeCSS(path = "www/my_style2_footer.css")),
    tags$head(tags$style(HTML(
      '* {font-family: "Arial";
          font-size: 18px;}'
    ))),
    
    #== TABS
    tabItems(
      
      #== HOME TAB
      tabItem(
        tabName = "home",
        
        homeUI("h"),
        
        # -hiddenbox
        useShinyjs(), # MUST include this line
        shinyjs::hidden(
          div(
            id ="hiddenbox", 
            
            box(
              title = NULL, 
              status = "success", 
              solidHeader = F,
              collapsible = T,
              width = "50%",
              column(6, 
                     box(
                       title = "Introduction tour", 
                       width = "50%",
                       HTML('<iframe width="700" height="400" src="home_video/images_home_help.mp4" frameborder="0" allow="accelerometer; 
                   autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                     )
              ),
              column(6,
                     box(
                       title = "TOTEM usage example", 
                       width = "50%",
                       HTML('<iframe width="700" height="400" src="home_video/TOTEMusage.mp4" frameborder="0" allow="accelerometer; 
                   autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                     )
              )
              
              
              
            )
          )
        ),
        div(
          class = "footer",
          includeHTML("www/footer.html")
        )
      ),
      
      #== NEW SEARCH TAB
      tabItem(tabName = "new_search",
              
              experiment_selectorUI("x"),
              div(
                class = "footer",
                includeHTML("www/footer.html")
              )
              ),
      
      #== RESULTS TAB
      tabItem(tabName = "results",
              
              enrichment_pageUI("ui"),
              div(
                class = "footer",
                includeHTML("www/footer.html")
              )
              ),
      
      #== FUNCTIONAL CHARACTERIZATION TAB
      tabItem(tabName = "functional_char",
              
              functional_characterizationUI("fc"),
              div(
                class = "footer",
                includeHTML("www/footer.html")
              )
              ),
      
      #== SINGLE CELL TAB
      tabItem(tabName = "sc_atlas",
              
              single_cellUI("sc"),
              div(
                class = "footer",
                includeHTML("www/footer.html")
              )),
      
      #== ABOUT TAB
      tabItem(tabName = "about",
              
              aboutUI("ab"),
              div(
                class = "footer",
                includeHTML("www/footer.html")
              )
              )
      
    )
  )
)



#= SERVER PART ====

server <- function(input, output, session) {
  
  #== HOME TAB ====
  homeServer("h")
  
  ## start search button
  observeEvent(input$start_search, {
    newtab <- switch(input$tabs, "home" = "new_search")
    updateTabItems(session, "tabs", newtab)
  })
  
  ## more info button
  observeEvent(input$more_info, {
    shinyjs::show(id = "hiddenbox", asis = TRUE)
  })
  
  
  #== NEW SEARCH TAB ====
  observeEvent(input$tabs, {
    if (input$tabs == "new_search") {
      # Execute module experiment selector (every time back in new search page)
      # Take out from the observer. Within this module, the input$submit is ourside its namespace
      x <<- experiment_selectorServer("x")
      
      ## Check if a single cell experiment is selected
      observeEvent(x$experiment_id(), {
        if (length(grep("SingleCell", x$experiment_id())) > 0) {
          single_cell_experiment <<- TRUE
        } else {
          single_cell_experiment <<- FALSE
        }
      })
    }
  })
  
  
  #== PRESSING SUBMIT BUTTON ====
  observeEvent(input$submit,
               {
                 # Update the tabs menu and redirect to results page
                 output$dynamic_tabs <- renderMenu({
                   sidebarMenu(
                     # Separator and identifier -> IF not description is provided, change for date-time
                     hr(),
                     h4("   "),
                     h4("   "),
                     # Menu item results
                     menuItem(
                       text = "Enrichment results",
                       tabName = "results",
                       icon = icon("seedling", lib = "font-awesome")
                     ),
                     
                     # Menu item single cell atlas (need to put if statement)
                     if (single_cell_experiment) {
                       menuItem(
                         text = "Single Cell Atlas",
                         tabName = "sc_atlas",
                         icon = icon("braille", lib = "font-awesome")
                       )
                       
                     }
                   )
                 })
                 
                 # Move to Enrichment Results tab
                 updateTabItems(session = session,
                                inputId = "tabs",
                                selected = "results")
                 
                 # EXECUTE MODULE ENRICHMENT
                 
                 ## Create reactiveValues for functional characterization button
                 fc_button <-
                   reactiveValues(func_char_tiss = NULL) # initialise reactiveValues
                 
                 ## Calculate enrichments
                 y <- enrichment_resultsServer(
                   id = "ui",
                   experiment_path = x$experiment_path(),
                   user_description = x$user_description(),
                   experiment_id = x$experiment_id(),
                   specie = x$specie(),
                   user_genelist = x$user_genelist(),
                   fc_button = fc_button
                 )
                 
                 
                 # EXECUTE MODULE COLOR SVG
                 z <- colorSVG_Server(
                   id = "ui",
                   experiment_path = x$experiment_path(),
                   user_description = x$user_description(),
                   experiment_id = x$experiment_id(),
                   specie = x$specie(),
                   enrichment_values = y$enrichment_values()
                 )
                 
                 # Module gene classifier
                 zz <<- gene_classifierServer(
                   id = "ui",
                   experiment_path = x$experiment_path(),
                   user_genelist = x$user_genelist()
                 )
                 
                 
                 #== PRESSING FUNCTIONAL CHARACTERIZATION BUTTON ====
                 observeEvent(fc_button$func_char_tiss,
                              {
                                # Update the tabs menu and redirect to funct. char. page
                                output$dynamic_tabs2 <- renderMenu({
                                  sidebarMenu(
                                    # Separator and identifier -> IF not description is provided, change for date-time
                                    hr(),
                                    h4("   "),
                                    h4("   "),
                                    # Menu item
                                    menuItem(
                                      text = "Functional characterization",
                                      tabName = "functional_char",
                                      icon = icon("table", lib = "font-awesome")
                                    )
                                  )
                                })
                                # Move to Functional characterization tab
                                updateTabItems(session = session,
                                               inputId = "tabs",
                                               selected = "functional_char")
                                
                                functional_characterizationServer(
                                  "fc",
                                  experiment_path = x$experiment_path(),
                                  user_description = x$user_description(),
                                  experiment_id = x$experiment_id(),
                                  specie = x$specie(),
                                  gene_set = zz$gene_set(),
                                  tissue = zz$selected_tissue()
                                )
                              })
                 
               })
  
  #== PRESSING SINGLE CELL ATLAS TAB ====
  observeEvent(input$tabs, {
    if (input$tabs == "sc_atlas") {
      # Move to Functional characterization tab
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "sc_atlas")
      
      single_cellServer(
        id = "sc",
        experiment_path = x$experiment_path(),
        user_description = x$user_description(),
        experiment_id = x$experiment_id(),
        specie = x$specie(),
        gene_set = x$user_genelist()
      )
    }
  })
  
  
  #== PRESSING RESULTS AGAIN WHEN FUNCT CHARACT IS ACTIVE ====
  current_fc <<- F
  observeEvent(input$tabs, {
    if (input$tabs == "functional_char") {
      current_fc <<- T
    }
    else if (current_fc == T & input$tabs == "results"){
      zz <<- gene_classifierServer(
        id = "ui",
        experiment_path = x$experiment_path(),
        user_genelist = x$user_genelist()
      )
    }
  })
  
  
  
  #== PRESSING NEW SEARCH AGAIN ====
  
  # Delete all previous variables
  existing_experiment <<- FALSE
  
  observeEvent(input$tabs, {
    if (input$tabs == "results") {
      existing_experiment <<- TRUE
      current_tab <<- "r"
    } 
    else if (input$tabs == "sc_atlas") {
      existing_experiment <<- TRUE
      current_tab <<- "sc"
    } 
    else if (input$tabs == "functional_char") {
      existing_experiment <<- TRUE
      current_tab <<- "fc"
    } 
    
    else if (existing_experiment == TRUE &
             input$tabs == "new_search") {
      # Raise alert
      shinyalert(
        title = "NEW SEARCH",
        text = "Runing a new search will discard current results\n Do you want to continue?",
        type = "warning",
        showCancelButton = TRUE,
        showConfirmButton = TRUE,
        confirmButtonCol = "#09C3A2",
        callbackR = function(x) {
          if (x != FALSE) {
            # if cancel button is clicked
            # Update tab to new search
            updateTabsetPanel(session = session,
                              inputId = "tabs",
                              selected = "new_search")
            # Remove generate tabs
            removeTab(inputId = "tabs",
                      target = "results",
                      session = session)
            removeTab(inputId = "tabs",
                      target = "sc_atlas",
                      session = session)
            output$dynamic_tabs = NULL
            removeTab(inputId = "tabs",
                      target = "functional_char",
                      session = session)
            output$dynamic_tabs2 = NULL
            
            ## Erase generated PNGs (optional)
            unlink("./*.png")
            ## Reset indicator
            existing_experiment <<- FALSE
            session$reload()
          }
          
          else {
            if (input$tabs == "new_search" & current_tab == "r"){
              updateTabItems(
                session = session,
                inputId = "tabs",
                selected = "results"
              )
            }
            else if (input$tabs == "new_search" & current_tab == "sc") {
              updateTabItems(
                session = session,
                inputId = "tabs",
                selected = "sc_atlas"
              )
            }
            else if (input$tabs == "new_search" & current_tab == "fc") {
              updateTabItems(
                session = session,
                inputId = "tabs",
                selected = "functional_char"
              )
            }
          }
          
          
        }
      )
    }
  })
  
  #== PRESSING ABOUT BUTTON  ====
  aboutServer("ab")
}

shinyApp(ui, server)
