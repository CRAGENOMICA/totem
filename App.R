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
            menuItem(text = "Enrichment results",tabName = "results",icon = icon("bar-chart-o")),
            menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table")),
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
                                         rows = 12
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
                           tags$head(tags$style(### adjust image to the windows size
                             type = "text/css",
                             "#blank_image img {max-width: 100%; width: auto; max-height: 200%; height: auto}" 
                           )),
                           imageOutput(outputId = "blank_image",)
                    )
            ),
            
            
            ## Tab results
            tabItem(tabName = "results",
                    
                    column(width = 5,
                           downloadButton(outputId = "download_colored_svg"), ### I think these buttons better in the upper part -> do not overlap with the image
                           # Output image
                           tags$head(tags$style(### adjust image to the windows size
                             type = "text/css",
                             "#colored_svg img {max-width: 100%; width: auto; max-height: 200%; height: auto}" 
                           )),
                           # Select color
                           selectInput(inputId = "color",
                                       label = "Change color",
                                       choices = c("salmon","steelblue","olivedrab"),
                                       multiple = FALSE),
                           # Output image
                           imageOutput(outputId = "colored_svg"),
                           
                           ),
                    
                    column(width = 7,
                           # Output barplot
                           imageOutput(outputId = "barplot", inline = TRUE),
                           downloadButton(outputId = "download_barplot"),
                           
                           hr(),
                           
                           column(5,
                                  # Tissue finder selector
                                  uiOutput(outputId = "tissue_finder"),
                                  # Finder text box
                                  box(style = 'height:80px;overflow-y: scroll;',### add a scroll bar
                                      solidHeader = TRUE,width = 12,
                                      verbatimTextOutput(outputId = "genes_in_tissue",placeholder = TRUE)),
                                  ),
                                 
                          hr(),
                           
                           
                           column(6,
                                  # Not enriched in any tissue box
                                   box(title="Genes (yours) not enriched in any tissue", 
                                     solidHeader=F, collapsible=TRUE, width = 12, 
                                     style = 'height:80px;overflow-y: scroll;', ### add a scroll bar
                                     verbatimTextOutput(outputId = "not_enriched"),),
                                  
                                  hr(),
                           
                                  # Not found box
                                  box(title="Genes (yours) not found in the experiment", 
                                     solidHeader=FALSE, collapsible=TRUE, width = 12, 
                                     style = 'height:80px;overflow-y: scroll;', ### add a scroll bar
                                     verbatimTextOutput(outputId = "not_found"),),
                                  ),
                           
                           hr(),
                           actionButton(inputId = "FunctCharact",
                                        label = "Functional characterization of gene sets")
                          )
            ),
            
            ## Tab Functional characterization
            tabItem(tabName = "functional_char",
                    
                    # Here functions to be implemented
                    h3("Select set of genes for functional characterization: "),
                    
                    column(width = 5, align = "center",
                           uiOutput(outputId = "select_tissue"),
                    ),
                    column(width = 12, align = "center",     
                           actionButton(inputId = "func_char_tiss", label = "Tissue-specific genes", width = "30%"),
                           actionButton(inputId = "func_char_notenr", label = "Not enriched genes", width = "30%"),
                           actionButton(inputId = "func_char_notfound", label = "Not found genes", width = "30%"),
                           
                    ),
                    
                    column(12,
                           box(title = "Annotation of selected genes",solidHeader=FALSE, collapsible=TRUE, width = 12, 
                               style = 'height:150px;overflow-y: scroll;overflow-x: scroll;', ### add a scroll bar
                               tableOutput('ann_table')
                           )
                    ),
                    
                    hr(),
                    
                    column(12,
                           box(title = "Gene Ontology analysis of selected genes",solidHeader=FALSE, collapsible=TRUE,width = 12,
                               style = 'overflow-y: scroll;overflow-x: scroll;',
                               column(3,
                                      selectInput(inputId = "select_ontology",
                                                  label = "Select the ontology",
                                                  choices = c("Biological process" = "BP",
                                                              "Molecular function" = "MF",
                                                              "Celular component" = "CC", 
                                                              "All" = "ALL"),
                                                  selected = "BP", multiple = FALSE),
                                      selectInput(inputId = "select_padjmethod",
                                                  label = "Select the method for p-value adjustment",
                                                  choices = c("Bonferroni" = "bonferroni", "Benjamini & Hochberg" = "BH", 
                                                              "Holm" = "holm", "Hochberg" = "hochberg", "Hommel" = "hommel", 
                                                              "Benjamini & Yekutieli" = "BY", "None" = "none"),
                                                  selected = "BH", multiple = FALSE),
                                      numericInput("select_pvalcutoff", "p-value cut-off", 0.05),
                                      numericInput("select_qvalcutoff", "q-value cut-off", 0.05),
                                      selectInput(inputId = "select_color",
                                                  label = "Select color for GO/KEGG plots",
                                                  choices = c("tomato","steelblue","olivedrab")),
                                      
                                      hr(),
                                      # downloadButton(outputId = "download_GOEA", label = "Download GOEA plots"),
                                      
                                      hr(),
                                      # downloadButton(outputId = "download_KEGG", label = "Download KEGG plots"),
                               ),
                               column(4,
                                      plotOutput(outputId = "dotplotGO", width = "50%")
                               ),
                               column(4,
                                      plotOutput(outputId = "dotplotKEGG", width = "50%")
                               ),
                               column(6,
                                      plotOutput(outputId = "netGO", width = "50%")
                               ),
                               column(6,
                                      plotOutput(outputId = "netgenesGO", width = "50%")
                               ),
                               column(12,
                                      plotOutput(outputId = "heatmapKEGG", width = "100%")
                               )
                               
                               
                           )
                    )
                    
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
        
        # # Generate results menu item
        # output$tabs <- renderMenu({
        #     sidebarMenu(
        #         menuItem(text = "Results",tabName = "results",icon = icon("calendar"))
        #     )
        #     })
        
        # Redirect to results page
        updateTabItems(session = session,
                       inputId = "tabs",
                       selected = "results")
        
        # 1. Load RData of the selected experiment
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        # Create a list of available tissues for function finder
        output$tissue_finder<-renderUI({
            selectInput(inputId = "tissue_finder",
                        label = "Select a tissue to deploy tissue-specific genes",
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
                     width="50%",
                     height="50%")
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
                         #Remove plots from the results tab
                         output$colored_svg <- NULL
                         output$barplot <- NULL
                         updateTabItems(session = session,
                                        inputId = "tabs",
                                        selected = "new_search")
                         current_tab<<-"new_search"
                         output$tabs <- renderMenu({
                           sidebarMenu(
                                       #menuItem(text = "Results",tabName = "results",icon = icon("calendar"))
                           )
                         })
                         ## remove the plots and gene lists
                         
                                   
                               # Reset and close results tab
                        } else {
                               updateTabItems(session = session,
                                              inputId = "tabs",
                                              selected = "results")
                           }
                       })
      }
      
    })
    
    ## FUNCTIONAL CHARACTERIZATION TAB
    ## Functional characterization of genes
    observeEvent(input$FunctCharact, {
      
      # Parse gene list
      source("./functions/parse_input_genes.R")
      parsed_user_genelist<-parse_input_genes(input = input$user_genelist)
      
      # Observe first if genes input are OK. If not do not create results tab and show error mssg
      {}
      
      # ## Generate func char menu item
      # output$tabs <- renderMenu({
      #   sidebarMenu(
      #     menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table"))
      #   )
      # })
      
      # Redirect to results page
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "functional_char")
      
      # 1. Load RData of the selected experiment
      load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
      
      # Create reactive value for plots
      v = reactiveValues(data = NULL)
      
      observeEvent(input$tabset, {
        v$data <- FALSE
      }) 
      
      ## Not enriched in any tissue box
      observeEvent(input$func_char_notenr,{
        
        source("./functions/not_enriched.R")
        ## Not enriched function
        v$data = not_enriched(user_genes = parsed_user_genelist,
                              tissue_atlas = tissue_atlas)
        
      })
      ## Not found box
      observeEvent(input$func_char_notfound,{
        # Not found box
        v$data = paste(setdiff(parsed_user_genelist,geneuniverse),collapse = "\n")
      })
      
      ## tissue specific box
      # Create a list of available tissues for function finder
      output$select_tissue<-renderUI({
        selectInput(inputId = "select_tissue",
                    label = "Select a tissue",
                    choices = names(tissue_atlas),
                    multiple = FALSE
        )
      })
      
      observeEvent(input$func_char_tiss,{
        # Run enrichment
        source("./functions/tissue_enrichment.R")
        enrich_values<-tissue_enrichment(user_genelist=parsed_user_genelist,
                                         tissue_atlas=tissue_atlas,
                                         geneuniverse = geneuniverse)
        
        # Tissue-specific genes box
        source("./functions/tissue_gene_finder.R")
        v$data = tissue_gene_finder(user_genes = parsed_user_genelist,
                                    tissue = input$select_tissue,
                                    tissue_atlas = tissue_atlas)
        
      })
      
      ## Annotation of genes
      source("./functions/functional_characterization.R")
      annotation <- reactive({functional_characterization(input_genes=parse_input_genes(input=v$data),
                                                          annotation_file = annotation_file)
        
      })
      
      output$ann_table <- renderTable({
        if(is.null(v$data)) return()
        annotation()
      })
      
      
      ##GO plots
      source("./functions/GO_plots.R")
      dotplotGO <- reactive({dotplotGO(input_genes=parse_input_genes(input=v$data),
                                       specie = input$specie,
                                        annotation_file = annotation_file, #from RData
                                        ontology= input$select_ontology,
                                        padjmethod = input$select_padjmethod,
                                        pvalcutoff = input$select_pvalcutoff,
                                        qvalcutoff = input$select_qvalcutoff,
                                        color = input$select_color)
      })
      netGO <- reactive({netGO(input_genes=parse_input_genes(input=v$data),
                               specie = input$specie,
                                   annotation_file = annotation_file, #from RData
                                   ontology= input$select_ontology,
                                   padjmethod = input$select_padjmethod,
                                   pvalcutoff = input$select_pvalcutoff,
                                   qvalcutoff = input$select_qvalcutoff,
                                   color = input$select_color)
      })
      netgenesGO <- reactive({netgenesGO(input_genes=parse_input_genes(input=v$data),
                                         specie = input$specie,
                                   annotation_file = annotation_file, #from RData
                                   ontology= input$select_ontology,
                                   padjmethod = input$select_padjmethod,
                                   pvalcutoff = input$select_pvalcutoff,
                                   qvalcutoff = input$select_qvalcutoff,
                                   color = input$select_color)
      })
      
      dotplotKEGG <- reactive({dotplotKEGG(input_genes=parse_input_genes(input=v$data),
                                           specie = input$specie,
                                   annotation_file = annotation_file, #from RData
                                   padjmethod = input$select_padjmethod,
                                   pvalcutoff = input$select_pvalcutoff,
                                   qvalcutoff = input$select_qvalcutoff,
                                   color = input$select_color)
      })
      heatmapKEGG <- reactive({heatmapKEGG(input_genes=parse_input_genes(input=v$data),
                                           specie = input$specie,
                                       annotation_file = annotation_file, #from RData
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       qvalcutoff = input$select_qvalcutoff,
                                       color = input$select_color)
      })
      output$dotplotGO <- renderPlot({
        # Progress bar
        # withProgress(message = 'Analysis in progress\n',
        #              detail = 'This may take a while...', value = 0, {
        #                for (i in 1:30) {
        #                  incProgress(1/30)
        #                  Sys.sleep(2)
        #                }
        #              })
        #Generate plot
        dotplotGO()
      })
      output$netGO <- renderPlot({
        # Progress bar
        # withProgress(message = 'Analysis in progress\n',
        #              detail = 'This may take a while...', value = 0, {
        #                for (i in 1:30) {
        #                  incProgress(1/30)
        #                  Sys.sleep(2)
        #                }
        #              })
        #Generate plot
        netGO()
      })
      output$netgenesGO <- renderPlot({
        # Progress bar
        # withProgress(message = 'Analysis in progress\n',
        #              detail = 'This may take a while...', value = 0, {
        #                for (i in 1:30) {
        #                  incProgress(1/30)
        #                  Sys.sleep(2)
        #                }
        #              })
        #Generate plot
        netgenesGO()
      })
      output$dotplotKEGG <- renderPlot({
        # Progress bar
        # withProgress(message = 'Analysis in progress\n',
        #              detail = 'This may take a while...', value = 0, {
        #                for (i in 1:30) {
        #                  incProgress(1/30)
        #                  Sys.sleep(2)
        #                }
        #              })
        #Generate plot
        dotplotKEGG()
      })
      output$heatmapKEGG <- renderPlot({
        # Progress bar
        # withProgress(message = 'Analysis in progress\n',
        #              detail = 'This may take a while...', value = 0, {
        #                for (i in 1:30) {
        #                  incProgress(1/30)
        #                  Sys.sleep(2)
        #                }
        #              })
        #Generate plot
        heatmapKEGG()
      })
      
      # Download button for plot
      # output$download_GOEA <- downloadHandler(filename = "GOplots.png",content = function(file){
      #                                                                                       png(file)
      #                                                                                       print(plotGO())
      #                                                                                       dev.off()
      #                                                                                       })
      # output$download_KEGG <- downloadHandler(filename = "KEGGplots.png",content = function(file){
      #                                                                                         png(file)
      #                                                                                         print(plotKEGG())
      #                                                                                         dev.off()
      #                                                                                         })
      
      # Observer func charac 
      observeEvent(input$tabs, {
        if (input$tabs == "functional_char") {
          current_tab<<-"functional_char"
        }
      })
      
      observeEvent(input$tabs, {
        if (current_tab=="functional_char" && input$tabs=="new_search") {
          shinyalert(title = "NEW SEARCH",
                     text="A new search will destroy current results",
                     showCancelButton = TRUE,showConfirmButton = TRUE,
                     callbackR = function(x) {
                       if (x != FALSE) {
                         output$ann_table <- NULL
                         output$GOplot <- NULL
                         output$KEGGplot <- NULL
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
                                        selected = "functional_char")
                       }
                     })
        }
        
      })
      
    })
    
    
    
    ## ABOUT TAB
    
}

shinyApp(ui = ui, server = server)

