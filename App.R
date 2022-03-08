## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: App.R
##
## Purpose of script: Proto-TOTEM app
##
## Author: Fidel Lozano-Elena
##
## Date Created: 2022-01-12
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

ui <-dashboardPage(
    
    #dashboardHeader(title = "TOTEM"),
    #dashboardHeader(disable = TRUE),
    dashboardHeader(title = "TOTEM",
                    tags$li(class = "dropdown",
                            #tags$img(alt="TOTEM Logo", src="totem_banner_trans.png"),
                            
                            )
    ),
    
    dashboardSidebar(
        # Dynamic Sidebar menu
        sidebarMenu(
            id = "tabs",
            menuItem(text = "New search",tabName = "new_search",icon = icon("dashboard")),
            sidebarMenuOutput(outputId = "tabs"),
            menuItem("Functional char.",tabName = "functional_char",icon = icon("th")),
            menuItem("About",tabName = "about",icon = icon("th"))
        )
    ),
    
    dashboardBody(
        # Include the custom styling
        tags$head(
            includeCSS(path = "www/my_style.css")
            ),
        
        tabItems(
            
            # Tab of new search
            tabItem(tabName = "new_search",
                    
                    column(width = 6,
                           
                           ## Select specie
                           selectInput(
                               inputId = "specie",label = "Select specie",
                               choices = list.dirs(path = normalizePath("./experiments"), full.names = FALSE, recursive = FALSE),
                               selected = "Arabidopsis",
                               multiple = FALSE),
                           
                           
                           ## Select experiment
                           # Here input a list of experiments, which should be the names of the experiments
                           uiOutput(outputId = "experiment"),
                           
                           ## User description
                           textAreaInput(inputId = "user_description",
                                         label = "User gene list name",
                                         value = "Enter a description for your gene list (optional)"
                           ),
                           
                           ## User gene list
                           textAreaInput(inputId = "user_genelist",
                                         label = "Gene list",
                                         value = "Paste you gene list here e.g:\nAT1G012032",
                                         width = "200px",height = "400px"
                           ),
                           
                           # Calculate enrichment button
                           actionButton(inputId = "submit",
                                        label = "Calculate enrichment")
                    ),
                    
                    column(width = 6,
                           
                           ## A Submit buttom
                           #submitButton(text = "SUBMIT", icon = NULL, width = "200px",),
                           
                           # Output functions:
                           
                           ## A text with the experiment description
                           tags$label(class = "control-label","Selected experiment details"),
                           htmlOutput(outputId = "experiment_description"),
                           
                           ## A image
                           imageOutput(outputId = "blank_image",)
                    )
            ),
            
            
            ## Tab results
            tabItem(tabName = "results",
                    
                    column(6,
                           
                           # Output image
                           imageOutput(outputId = "colored_svg"),
                           downloadButton(outputId = "download_colored_svg"),
                           
                           # Select color
                           selectInput(inputId = "color",
                                       label = "Change color",
                                       choices = c("salmon","steelblue","olivedrab"),
                                       multiple = FALSE)
                           ),
                    
                    column(6,
                           
                           # Output barplot
                           imageOutput(outputId = "barplot"),
                           downloadButton(outputId = "download_barplot"),
                           
                           # Tissue finder selector
                           uiOutput(outputId = "tissue_finder"),
                           
                           # Finder text box
                           verbatimTextOutput(outputId = "genes_in_tissue",placeholder = TRUE),
                           
                           # Not enriched in any tissue box
                           box(title="Genes (yours) not enriched in any tissue", 
                               solidHeader=FALSE, collapsible=TRUE, width = 12,
                               verbatimTextOutput(outputId = "not_enriched")),
                           
                           # Not found box
                           box(title="Genes (yours) not found in the experiment", 
                               solidHeader=FALSE, collapsible=TRUE, width = 12,
                               verbatimTextOutput(outputId = "not_found")),
                           
                           )
                    
            ),
            
            ## Tab Functional characterization
            tabItem(tabName = "functional_char",
                    
                    # Here functions to be implemented
                    
            ),
            
            ## Tab About
            tabItem(tabName = "about",
                    
                    h3("Here all information about the tool will be displayed")
                    
            )
        )
        
    )
    
)


server <- function(input, output, session) {
    
    ## NEW EXPERIMENT PAGE
    current_tab<-"new_search"
    
    # Create a list of available experiment within each organism
    output$experiment<-renderUI({
        selectInput(inputId = "experiment_id",
                    label = "Select an experiment",
                    choices = list.dirs(normalizePath(paste("./experiments",input$specie,sep = "/")),full.names = FALSE,recursive = FALSE),
                    multiple = FALSE
        )
    })
    
    # Observe any change in specie
    observeEvent(input$specie, {
        updateSelectInput(session = session,
                          inputId = "experiment_id",
                          label = "Select an experiment",
                          choices = list.dirs(normalizePath(paste("./experiments",input$specie,sep = "/")),full.names = FALSE,recursive = FALSE)
                          )
    })
    
    ## Selected experiment path
    observeEvent(input$experiment_id, {
        
    # Get experiment path    
    experiment_path<<-normalizePath(paste("./experiments",input$specie,input$experiment_id,sep = "/"))

    ## Text box with description of the experiment
    #output$experiment_description <- renderText({
        
        # Get description from text file
    #    description <- readLines(normalizePath(paste(experiment_path,"experiment_description.txt",sep = "/")))
    #    expr = description
    #    })
    
    output$experiment_description <- renderUI({
        description <- readLines(normalizePath(paste(experiment_path,"experiment_description.txt",sep = "/")))
        HTML(description)
    })

    ## Blank Image
    
    output$blank_image <- renderImage(
        {
            # Read image
            filename <- normalizePath(paste(experiment_path,"blank_image.png",sep = "/"))
            
            # Read myImage's width and height. These are reactive values, so this
            # expression will re-run whenever they change.
            width  <- session$clientData$output_blank_image_width
            height <- session$clientData$output_blank_image_height
            
            list(src=filename,
                 width=width,
                 height=height)
            }, deleteFile = FALSE
        )
    })
    
    ## GO TO RESULTS PAGE
    # Send button
    observeEvent(input$submit, {
        
        # Parse gene list
        source("./functions/parse_input_genes.R")
        parsed_user_genelist<-parse_input_genes(input = input$user_genelist)
        
        # Observe first if genes input are OK. If not do not create results tab and show error mssg
        {}
        
        # Generate results menu item
        output$tabs <- renderMenu({
            sidebarMenu(
                menuItem(text = "Results",tabName = "results",icon = icon("calendar"))
            )
            })
        
        # Redirect to results page
        updateTabItems(session = session,
                       inputId = "tabs",
                       selected = "results")
        
        # 1. Load RData of the selected experiment
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        # Create a list of available tissues for function finder
        output$tissue_finder<-renderUI({
            selectInput(inputId = "tissue_finder",
                        label = "Select a tissue to deploy genes",
                        choices = names(tissue_atlas),
                        multiple = FALSE
                        )
        })
        
        
        # Run enrichment
        source("./functions/tissue_enrichment.R")
        enrich_values<-tissue_enrichment(user_genelist=parsed_user_genelist,
                                         tissue_atlas=tissue_atlas,
                                         geneuniverse = geneuniverse)
        
        
        # Color SVG
        source("./functions/generate_color_scale.R")
        source("./functions/color_svg.R")
        colored_svg<-reactive({
            ## Generate color scale
            colors<-generate_color_scale(input = enrich_values,color = input$color)
            # color_svg
            color_svg(input_svg=normalizePath(paste(experiment_path,paste(input$experiment_id,"svg",sep="."),sep = "/")),
                      tissue_color=colors,
                      output_file="first_TOTEM_test.png")
        })
        
        ## Generate a barplot
        colored_barplot<-reactive({
            mybarplot2(myvector = enrich_values,
                       color = input$color,
                       outputfile = "enrichment_result_barplot.png")
            })
        
        ## Output colored SVG 
        output$colored_svg <- renderImage(
            {
                # Generate image
                colored_svg()
                # Read image
                filename <- normalizePath("first_TOTEM_test.png")
                list(src=filename,
                     width=400,
                     height=600)
            }, deleteFile = FALSE
        )
        
        ## Download button for SVG
        output$download_colored_svg <- downloadHandler(filename = "draw.png",content = normalizePath("first_TOTEM_test.png"))
        
        ## Output barplot
        output$barplot <- renderImage(
            {
                # Generate image
                colored_barplot()
                # Read image
                filename <-normalizePath("enrichment_result_barplot.png")
                list(src=filename,
                     width=400,
                     height=400)
            }, deleteFile = FALSE
        )
        
        ## Download button for barplot
        output$download_barplot <- downloadHandler(filename = "barplot.png",content = normalizePath("enrichment_result_barplot.png"))
        
        # Tissue-specific genes box
        source("./functions/tissue_gene_finder.R")
        output$genes_in_tissue <- renderText({
            
            ## Finder function
            expr = tissue_gene_finder(user_genes = parsed_user_genelist,
                                      tissue = input$tissue_finder,
                                      tissue_atlas = tissue_atlas)
            })
        
        # Not enriched in any tissue box
        source("./functions/not_enriched.R")
        output$not_enriched <- renderText({
            
            ## Not enriched function
            expr = not_enriched(user_genes = parsed_user_genelist,
                                tissue_atlas = tissue_atlas)
        })
        
        # Not found box
        output$not_found <- renderText({
            
            ## Not enriched function
            expr = paste(setdiff(parsed_user_genelist,geneuniverse),collapse = "\n")
        })
        
        # Modal: Show warning message when trying to return to new search page
        # New search warning pop up
        
        
        })
    
    ## WHILE IN RESULT PAGE OBSERVE IF THE USER WANT A NEW SEARCH
    
    # Observer
    observeEvent(input$tabs, {
        if (input$tabs == "results") {
            current_tab<<-"results"
        }
        })
    
    observeEvent(input$tabs, {
        if (current_tab=="results" && input$tabs=="new_search") {
            shinyalert(title = "NEW SEARCH",
                       text="A new search will destroy current results",
                       showCancelButton = TRUE,showConfirmButton = TRUE,
                       callbackR = function(x) {
                           if (x != FALSE) {
                               updateTabItems(session = session,
                                              inputId = "tabs",
                                              selected = "new_search")
                               current_tab<<-"new_search"
                               output$tabs <- renderMenu({
                                   sidebarMenu(
                                       #menuItem(text = "Results",tabName = "results",icon = icon("calendar"))
                                   )
                                   })
                                   
                               # Reset and close results tab
                           } else {
                               updateTabItems(session = session,
                                              inputId = "tabs",
                                              selected = "results")
                           }
                       })
        }
    })
    
    ## ADDITIONAL TAB
    ## ABOUT TAB
    
}

shinyApp(ui = ui, server = server)

