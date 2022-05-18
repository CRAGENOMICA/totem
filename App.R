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
library(DT)

ui <-dashboardPage(
  
  #dashboardHeader(title = "TOTEM"),
  #dashboardHeader(disable = TRUE),
  dashboardHeader(title = tags$img(alt="TOTEM Logo", src="totem_banner_trans.png",
                                   height="100%", width="100%", align="left")
                  
  ),
  
  dashboardSidebar(
    # Dynamic Sidebar menu
    sidebarMenu(
      id = "tabs",
      menuItem(text = "New search",tabName = "new_search",icon = icon("database", lib = "font-awesome")),
      sidebarMenuOutput(outputId = "tabs"),
      menuItem(text = "Enrichment results",tabName = "results",icon = icon("seedling", lib = "font-awesome")),
      menuItem(text = "Functional characterization",tabName = "functional_char",icon = icon("table", lib = "font-awesome")),
      menuItem(text = "Single Cell atlas",tabName = "single_cell",icon = icon("bar-chart-o", lib = "font-awesome")),
      menuItem("About",tabName = "about",icon = icon("info", lib = "font-awesome"))
    )
  ),
  
  dashboardBody(
    # Include the custom styling
    tags$head(
      includeCSS(path = "www/my_style2.css")
    ),
    tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
    
    
    tabItems(
      
      # Tab of new search
      tabItem(tabName = "new_search",
              
              column(width = 4,
                     
                     ## Select specie
                     selectInput(
                       inputId = "specie",label = "Select specie",
                       choices = list.dirs(path = normalizePath("./experiments"), full.names = FALSE, recursive = FALSE),
                       selected = "Arabidopsis",
                       multiple = FALSE,
                       width = "100%"),
                     
                     
                     ## Select experiment
                     # Here input a list of experiments, which should be the names of the experiments
                     uiOutput("experiment"),
                     # uiOutput(outputId = "experiment"),
                     
                     ## User description
                     textAreaInput(inputId = "user_description",
                                   label = "User gene list name",
                                   value = "Enter a description for your gene list (optional)",
                                   width = "100%"
                     ),
                     
                     ## User gene list
                     textAreaInput(inputId = "user_genelist",
                                   label = "Gene list",
                                   value = "Paste you gene list here e.g:\nAT1G012032 for Arabidopsis, Sobic.001G000700 or Sb01g000230 for Sorghum and Solyc00g011670 for Tomato",
                                   rows = 12,
                                   width = "100%"
                     ),
                     
                     #Clear gene list button
                     actionButton(inputId = "clear",
                                  label = "Clear"),
                     
                     # Calculate enrichment button
                     actionButton(inputId = "submit",
                                  label = "Calculate enrichment")
              ),
              
              column(width = 7,offset = 1,
                     
                     ## A Submit buttom
                     #submitButton(text = "SUBMIT", icon = NULL, width = "200px",),
                     
                     # Output functions:
                     
                     ## A text with the experiment description
                     tags$label(class = "control-label","Selected experiment details"),
                     htmlOutput(outputId = "experiment_description"),
                     
                     ## A image
                     tags$head(tags$style(### adjust image to the windows size
                       type = "text/css",
                       "#blank_image img {max-width: 100%; width: auto; max-height: 200%; height: auto; text-align: center}" 
                     )),
                     imageOutput(outputId = "blank_image",)
              )
      ),
      
      
      ## Tab results
      tabItem(tabName = "results",
              
              verbatimTextOutput(outputId = "description"),
              
              br(),
              
              column(width = 5,
                     
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
                     downloadButton(outputId = "download_colored_svg"), 
                     # Output image
                     imageOutput(outputId = "colored_svg"),
                     
              ),
              
              column(width = 7,align = "center",
                     # Output barplot
                     imageOutput(outputId = "barplot", inline = TRUE),
                     downloadButton(outputId = "download_barplot"),
                     
                     hr(),
                     
                     column(6,
                             box(title="Genes enriched in a specific tissue",
                                solidHeader = FALSE, collapsible=TRUE, width = 12,
                                # Tissue finder selector
                                uiOutput(outputId = "tissue_finder"),
                                # Finder text box
                                column(3,
                                       h6("Number of genes enriched"),
                                       verbatimTextOutput(outputId = "number_genes_in_tissue",placeholder = TRUE)
                                       ),
                                column(9,
                                       verbatimTextOutput(outputId = "genes_in_tissue",placeholder = TRUE)
                                       ), 
                                actionButton(inputId = "func_char_tiss", label = "Functional characterization", align = "center"),
                                actionButton(inputId = "atlas", label = "Single cell resolution", align = "center"))
                     ),
                     
                     column(6,
                            # Not enriched in any tissue box
                            box(title="Genes not enriched in any tissue", 
                                solidHeader=FALSE, collapsible=TRUE, width = 12, 
                                column(3,
                                       h6("Number of genes not enriched"),
                                       verbatimTextOutput(outputId = "number_not_enriched",placeholder = TRUE)
                                ),
                                column(9,
                                       verbatimTextOutput(outputId = "not_enriched"),
                                ),
                                actionButton(inputId = "func_char_notenr", label = "Functional characterization", align = "center")), 

                            hr(),
                            
                            # Not found box
                            box(title="Genes not found in the experiment", 
                                solidHeader=FALSE, collapsible=TRUE, width = 12, 
                                column(3,
                                       h6("Number of genes not found"),
                                       verbatimTextOutput(outputId = "number_not_found",placeholder = TRUE)
                                ),
                                column(9,
                                       verbatimTextOutput(outputId = "not_found"),
                                ),
                                actionButton(inputId = "func_char_notfound", label = "Functional characterization", align = "center")), 
                     ),
                     hr(),
              )
      ),
      
      ## Tab Functional characterization
      tabItem(tabName = "functional_char",
              
              verbatimTextOutput(outputId = "geneset"),
              
              br(),
          
              column(12,
                     box(title = "Annotation of selected genes",solidHeader=FALSE, collapsible=TRUE, width = 12, 
                         style = 'overflow-x: scroll;', ### add a scroll bar
                         DT::dataTableOutput('ann_table'),
                         downloadButton(outputId = "download_annotation", label = "Download table")
                     )
              ),
              
              hr(),
              
              column(12,
                     box(title = "Gene Ontology analysis of selected genes",solidHeader=FALSE, collapsible=TRUE,width = 12,
                         style = "width: 100%;height: 60em;",
                         sidebarLayout(
                           # side panel for inputs
                           sidebarPanel(width = 3,
                                        
                                        selectInput(inputId = "select_ontology",
                                                    label = "Select the ontology",
                                                    choices = c("Biological process" = "GO:BP",
                                                                "Molecular function" = "GO:MF",
                                                                "Celular component" = "GO:CC"),
                                                    selected = "GO:BP", multiple = FALSE),
                                        br(),# br() element to introduce extra vertical spacing
                                        selectInput(inputId = "select_padjmethod",
                                                    label = "Select the method for p-value adjustment",
                                                    choices = c("Bonferroni" = "bonferroni", "Benjamini & Hochberg (FDR)" = "fdr", 
                                                                "gSCS" = "gSCS"),
                                                                # "Holm" = "holm", "Hochberg" = "hochberg", "Hommel" = "hommel", 
                                                                # "Benjamini & Yekutieli" = "BY", "None" = "none"),
                                                    selected = "fdr", multiple = FALSE),
                                        br(),
                                        numericInput("select_pvalcutoff", "p-value cut-off", 0.05, step = 0.01),
                                        br(),
                                        # numericInput("select_qvalcutoff", "q-value cut-off", 0.05, step = 0.01),
                                        # br(),
                                        # selectInput(inputId = "select_color",
                                        #             label = "Select color for GO/KEGG plots",
                                        #             choices = c("salmon","steelblue","olivedrab"),
                                        #             selected = "salmon", multiple = FALSE),
                                        # br(),
                                        sliderInput("select_nCategory", "Maximum number of GO categories to show", value = 20, min = 1, max = 50)
                                        
                           ),
                           
                           # Main panel for displaying outputs
                           mainPanel( width = 9,
                                      
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Dotplot: GO terms", 
                                                           br(),
                                                           downloadButton(outputId = "download_dotGO", label = "Download GO term plot"),
                                                           plotOutput(outputId = "dotGO", width = "100%"),),
                                                  tabPanel("Net: GO terms", 
                                                           br(),
                                                           downloadButton(outputId = "download_net", label = "Download GO term net"),
                                                           plotOutput(outputId = "net", width = "100%")),
                                                  tabPanel("Heatmap: Genes in GO terms", 
                                                           br(),
                                                           downloadButton(outputId = "download_heatGO", label = "Download GO term and genes net"),
                                                           plotOutput(outputId = "heatGO", width = "100%")),
                                                  tabPanel("Dotplot: KEGG pathways", 
                                                           br(),
                                                           downloadButton(outputId = "download_dotKEGG", label = "Download KEGG pathway plot"),
                                                           plotOutput(outputId = "dotKEGG", width = "100%")),
                                                  tabPanel("Heatmap: Genes in KEGG pathways", 
                                                           br(),
                                                           downloadButton(outputId = "download_heatmap", label = "Download KEGG pathway heatKEGG"),
                                                           plotOutput(outputId = "heatKEGG", width = "100%"))
                                                  
                                      )
                           )
                         )
                     )
              )
              
      ),
      
      #### Tab single cell visualization ####
      tabItem(tabName = "single_cell",
              
              # In  this column, the complete atlas
              column(4,
                     ## Add the atlas image
                     tags$head(tags$style(### adjust image to the windows size
                       type = "text/css",
                       "#umap_atlas img {max-width: 200%; width: auto; max-height: 200%; height: auto; text-align: center}" 
                     )),
                     imageOutput(outputId = "umap_atlas",)
                     ),
              
              column(4,
                     verbatimTextOutput(outputId = "geneset_sc"),
                     br(),
                     column(6,
                            selectizeInput(
                              inputId = "gene_expr",label = "Select a gene",
                              choices = NULL,
                                # read.delim(paste0("./single_cell_visualization/", "Leaf_SingleCell/", "geneindex.txt"), header = F)$V1,
                              selected = "AT1G01010",
                              multiple = FALSE,
                              width = "100%"),
                            
                     ),
                     column(6,
                            selectInput(
                              inputId = "color_expr",label = "Select a color",
                              choices = c("darkblue", "darkred", "darkgreen"),
                              selected = "darkblue",
                              multiple = FALSE,
                              width = "100%"),
                     ),
                     column(12,
                            # downloadButton(outputId = "download_expr", label = "Download expression plot"),
                            plotOutput(outputId = "expr_umap", width = "130%")
                     )
              ),
              
              column(4,
                     box(title = "Tissue-specific expression",solidHeader=FALSE, collapsible=FALSE, width = 12,
                         style = 'overflow-x: scroll;', ### add a scroll bar
                         # downloadButton(outputId = "download_cluster_info", label = "Download expression summary"),
                         DT::dataTableOutput('expr_table'),
                     ),
                     br(),
                     plotOutput(outputId = "expr_umap_zoom", width = "130%")
                     
              )
              
      ),
      
      ## Tab About
      tabItem(tabName = "about",
              
              # h3("Here all information about the tool will be displayed"),
              fluidPage(
                htmlOutput("about")
              )
              
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
                multiple = FALSE,
                width = "100%"
    )
  })
  
  # Observe any change in specie
  observeEvent(input$specie, {
    updateSelectInput(session = session,
                      inputId = "experiment_id",
                      label = "Select an experiment",
                      choices = list.dirs(normalizePath(paste("./experiments",input$specie,sep = "/")),full.names = FALSE,recursive = FALSE)
    )
    # Get the specie
    selected_specie<<-as.character(input$specie)
  })
  
  ## Selected experiment path
  observeEvent(input$experiment_id, {
    
    # Get experiment path    
    experiment_path<<-normalizePath(paste("./experiments",input$specie,input$experiment_id,sep = "/"))
    
    ## Text box with description of the experiment
    
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
  
  # Clear button
  observeEvent(input$clear, {
    updateTextInput(session, "user_genelist", value = "")
  })
  
  ## GO TO RESULTS PAGE
  # Send button
  observeEvent(input$submit, {
    
    # add description
    output$description <- renderText({
      return(input$user_description)
    })

    
    # 1. Load RData of the selected experiment
    load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
    
    
    # Parse gene list
    source("./functions/parse_input_genes.R")
    parsed_user_genelist<-parse_input_genes(input = input$user_genelist, input_specie = selected_specie, annotation_file = annotation_file)
    
    # Observe first if genes input are OK. If not do not create results tab and show error mssg
    {}
    
    # Redirect to results page
    updateTabItems(session = session,
                   inputId = "tabs",
                   selected = "results")

    # Create a list of available tissues for function finder
    output$tissue_finder<-renderUI({
      selectInput(inputId = "tissue_finder",
                  label = paste0("Select a tissue to deploy genes"),
                  choices = names(tissue_atlas),
                  multiple = FALSE
      )
    })
    
    # Run enrichment
    source("./functions/tissue_enrichment.R")
    enrich_values<-tissue_enrichment(user_genelist=parsed_user_genelist,
                                     tissue_atlas=tissue_atlas,
                                     geneuniverse=geneuniverse)
    
    # Run drawing vector to solve overlapping between tissues in the SVG image
    svg_enrich_values<-drawing_vector(enrich_values)
    
    # Color SVG
    source("./functions/generate_color_scale.R")
    source("./functions/color_svg.R")
    colored_svg<-reactive({
      ## Generate color scale
      colors<-generate_color_scale(input = svg_enrich_values,color = input$color) #use the new enrich values for overlapping avoidance
      # color_svg
      color_svg(input_svg=normalizePath(paste(experiment_path,paste(input$experiment_id,"svg",sep="."),sep = "/")),
                tissue_colors=colors,
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
             width="50%",
             height="100%")
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
             width="70%",
             height="70%")
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

    # add number of genes enriched
    output$number_genes_in_tissue <- renderText({
      genes = strsplit(x = tissue_gene_finder(user_genes = parsed_user_genelist,
                                               tissue = input$tissue_finder,
                                               tissue_atlas = tissue_atlas),
                        split = "\n")[[1]]
      if("none" %in% genes){
        return(0)
      }
      else{
        return(length(genes))
      }
    })
    
    
    # Not enriched in any tissue box
    source("./functions/not_enriched.R")
    output$not_enriched <- renderText({
      
      ## Not enriched function
      expr = not_enriched(user_genes = parsed_user_genelist,
                          tissue_atlas = tissue_atlas)
    })
    # add number of genes not enriched
    output$number_not_enriched <- renderText({
      genes = strsplit(x = not_enriched(user_genes = parsed_user_genelist,
                                         tissue_atlas = tissue_atlas),
                       split = "\n")[[1]]
      if("none" %in% genes){
        return(0)
      }
      else{
        return(length(genes))
      }
    })
    
    # Not found box
    source("./functions/not_found.R")
    output$not_found <- renderText({
      
      expr = not_found(user_genes = parsed_user_genelist,
                       geneuniverse=geneuniverse)
      
    })
    # add number of genes not enriched
    output$number_not_found <- renderText({
      genes = strsplit(x = not_found(user_genes = parsed_user_genelist,
                                     geneuniverse=geneuniverse),
                       split = "\n")[[1]]
      if("none" %in% genes){
        return(0)
      }
      else{
        return(length(genes))
      }
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
                 showCancelButton = TRUE,showConfirmButton = TRUE,confirmButtonCol = "#09C3A2",
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
  
    ## Not enriched in any tissue box
    observeEvent(input$func_char_notenr,{
      # 1. Load RData of the selected experiment
      load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
      
      
      # Parse gene list
      source("./functions/parse_input_genes.R")
      parsed_user_genelist<-parse_input_genes(input = input$user_genelist, input_specie = selected_specie, annotation_file = annotation_file)
      
      # Observe first if genes input are OK. If not do not create results tab and show error mssg
      {}
      
      # Redirect to results page
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "functional_char")
      
      output$geneset <- renderText({
        return(paste("Characterization of genes not enriched in any tissue"))
      })
      
      
      # Create reactive value for characterization
      v = reactiveValues(data = NULL)
      
      observeEvent(input$tabset, {
        v$data <- FALSE
      }) 
      
      source("./functions/not_enriched.R")
      v$data = not_enriched(user_genes = parsed_user_genelist,
                            tissue_atlas = tissue_atlas)
      
      
      # Annotation of genes
      source("./functions/functional_characterization.R")
      annotation <- reactive({functional_characterization(input_genes=strsplit(x = v$data,split = "\n")[[1]],
                                                          annotation_file = annotation_file)
      })
      output$ann_table <- DT::renderDataTable({
        if(is.null(v$data)) return()
        withProgress(
          DT::datatable(annotation(),options = list(lengthMenu = c(3, 5, 10, 20, 50), pageLength = 3)),
          message = "Retrieving annotation...")
      })
      # download table
      output$download_annotation <- downloadHandler(
        filename = function(){"gene_set_functional_information.csv"}, 
        content = function(fname){
          write.csv(annotation(), fname)
        }
      )
      
      ##GO plots
      source("./functions/GO_plots_new.R")
      dotGO <- reactive({dotplotGO(input_genes=strsplit(x = v$data,split = "\n")[[1]],
                                   specie = input$specie,
                                   ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                   pvalcutoff = input$select_pvalcutoff,
                                   nCategory = input$select_nCategory)})
      net <- reactive({netGO(input_genes=strsplit(x = v$data,split = "\n")[[1]],
                             specie = input$specie,
                             ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                             pvalcutoff = input$select_pvalcutoff,
                             nCategory = input$select_nCategory)})
      heatGO <- reactive({heatmapGO(input_genes=strsplit(x = v$data,split = "\n")[[1]],
                                       specie = input$specie,
                                       ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      dotKEGG <- reactive({dotplotKEGG(input_genes=strsplit(x = v$data,split = "\n")[[1]],
                                       specie = input$specie,
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      heatKEGG <- reactive({heatmapKEGG(input_genes=strsplit(x = v$data,split = "\n")[[1]],
                                       specie = input$specie,
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      output$dotGO <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { dotGO() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=600,height=600)
      output$net <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { net() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=1000, height=600)
      output$heatGO <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { heatGO() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=1000,height=600)
      output$dotKEGG <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { dotKEGG() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=600,height=600)
      output$heatKEGG <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { heatKEGG() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=1000,height=600)
      
      # Download button for plots
      output$download_dotGO <- downloadHandler(filename = function(){
        paste("GOterm", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- dotGO()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_net <- downloadHandler(filename = function(){
        paste("GOtermNet", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- net()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_heatGO <- downloadHandler(filename = function(){
        paste("GOtermGenesNet", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- heatGO()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_dotKEGG <- downloadHandler(filename = function(){
        paste("KEGGpathway", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- dotKEGG()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_heatmap <- downloadHandler(filename = function(){
        paste("KEGGheatmap", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- heatKEGG()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      
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
                     showCancelButton = TRUE,showConfirmButton = TRUE,confirmButtonCol = "#09C3A2",
                     callbackR = function(x) {
                       if (x != FALSE) {
                         output$ann_table <- NULL
                         output$dotGO = output$net = output$heatGO = output$dotKEGG = output$heatKEGG = NULL
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
    
    ## Not found box
    observeEvent(input$func_char_notfound,{
      # 1. Load RData of the selected experiment
      load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
      
      # Parse gene list
      source("./functions/parse_input_genes.R")
      parsed_user_genelist<-parse_input_genes(input = input$user_genelist, input_specie = selected_specie, annotation_file = annotation_file)
      
      # Observe first if genes input are OK. If not do not create results tab and show error mssg
      {}
      
      
      # Redirect to results page
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "functional_char")
      
      output$geneset <- renderText({
        return(paste("Characterization of genes not found in the experiment"))
      })
      
    
      # Create reactive value for characterization
      v = reactiveValues(data = NULL)
      
      observeEvent(input$tabset, {
        v$data <- FALSE
      }) 
      
      source("./functions/not_found.R")
      v$data = not_found(user_genes = parsed_user_genelist,
                         geneuniverse = geneuniverse)

      # Annotation of genes
      source("./functions/functional_characterization.R")
      annotation <- reactive({functional_characterization(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                                          annotation_file = annotation_file)
      })
      output$ann_table <- DT::renderDataTable({
        if(is.null(v$data)) return()
        withProgress(
          DT::datatable(annotation(),options = list(lengthMenu = c(3, 5, 10, 20, 50), pageLength = 3)),
          message = "Retrieving annotation...")
      })
      # download table
      output$download_annotation <- downloadHandler(
        filename = function(){"gene_set_functional_information.csv"}, 
        content = function(fname){
          write.csv(annotation(), fname)
        }
      )
      
      ##GO plots
      source("./functions/GO_plots_new.R")
      dotGO <- reactive({dotplotGO(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                   specie = input$specie,
                                   ontology= input$select_ontology, padjmethod = input$select_padjmethod,
                                   pvalcutoff = input$select_pvalcutoff,
                                   nCategory = input$select_nCategory)})
      net <- reactive({netGO(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                             specie = input$specie,
                             ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                             pvalcutoff = input$select_pvalcutoff,
                             nCategory = input$select_nCategory)})
      heatGO <- reactive({heatmapGO(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                       specie = input$specie,
                                       ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      dotKEGG <- reactive({dotplotKEGG(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                       specie = input$specie,
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      heatKEGG <- reactive({heatmapKEGG(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                       specie = input$specie,
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      output$dotGO <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { dotGO() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=600,height=600)
      output$net <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { net() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=1000, height=600)
      output$heatGO <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { heatGO() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=1000,height=600)
      output$dotKEGG <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { dotKEGG() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=600,height=600)
      output$heatKEGG <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { heatKEGG() }, #Generate plot
            error = function(e) {""}),message = "Plotting...")},width=1000,height=600)
      
      # Download button for plots
      output$download_dotGO <- downloadHandler(filename = function(){
        paste("GOterm", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- dotGO()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_net <- downloadHandler(filename = function(){
        paste("GOtermNet", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- net()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_heatGO <- downloadHandler(filename = function(){
        paste("GOtermGenesNet", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- heatGO()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_dotKEGG <- downloadHandler(filename = function(){
        paste("KEGGpathway", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- dotKEGG()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_heatmap <- downloadHandler(filename = function(){
        paste("KEGGheatmap", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- heatKEGG()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      
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
                     showCancelButton = TRUE,showConfirmButton = TRUE,confirmButtonCol = "#09C3A2",
                     callbackR = function(x) {
                       if (x != FALSE) {
                         output$ann_table <- NULL
                         output$dotGO = output$net = output$heatGO = output$dotKEGG = output$heatKEGG = NULL
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
    
    ## tissue specific box
    # Create a list of available tissues for function finder
    observeEvent(input$func_char_tiss,{
      
      # 1. Load RData of the selected experiment
      load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
      

      # Parse gene list
      source("./functions/parse_input_genes.R")
      parsed_user_genelist<-parse_input_genes(input = input$user_genelist, input_specie = selected_specie, annotation_file = annotation_file)
      
      # Observe first if genes input are OK. If not do not create results tab and show error mssg
      {}
      
      # Redirect to results page
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "functional_char")
      
     
      # Create reactive value for characterization
      v = reactiveValues(data = NULL)
      
      observeEvent(input$tabset, {
        v$data <- FALSE
      }) 
      
      output$geneset <- renderText({
        return(paste("Characterization of tissue-enriched genes in", input$tissue_finder, sep = " "))
      })
    
      # Run enrichment
      source("./functions/tissue_enrichment.R")
      enrich_values<-tissue_enrichment(user_genelist=parsed_user_genelist,
                                       tissue_atlas=tissue_atlas,
                                       geneuniverse = geneuniverse)
      
      # Tissue-specific genes box
      source("./functions/tissue_gene_finder.R")
      v$data = tissue_gene_finder(user_genes = parsed_user_genelist,
                                  tissue = input$tissue_finder,
                                  tissue_atlas = tissue_atlas)
      
      # Annotation of genes
      source("./functions/functional_characterization.R")
      annotation <- reactive({functional_characterization(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                                          annotation_file = annotation_file)
      })
      output$ann_table <- DT::renderDataTable({
        if(is.null(v$data)) return()
        withProgress(
          DT::datatable(annotation(),options = list(lengthMenu = c(3, 5, 10, 20, 50), pageLength = 3)),
          message = "Retrieving annotation...")
      })
      # download table
      output$download_annotation <- downloadHandler(
        filename = function(){"gene_set_functional_information.csv"}, 
        content = function(fname){
          write.csv(annotation(), fname)
        }
      )
      
      ##GO plots
      source("./functions/GO_plots_new.R")
      dotGO <- reactive({dotplotGO(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                   specie = input$specie,
                                   ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                   pvalcutoff = input$select_pvalcutoff,
                                   nCategory = input$select_nCategory)})
      net <- reactive({netGO(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                             specie = input$specie,
                             ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                             pvalcutoff = input$select_pvalcutoff,
                             nCategory = input$select_nCategory)})
      heatGO <- reactive({heatmapGO(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                       specie = input$specie,
                                       ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      dotKEGG <- reactive({dotplotKEGG(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                       specie = input$specie,
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      heatKEGG <- reactive({heatmapKEGG(input_genes=parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file),
                                       specie = input$specie,
                                       padjmethod = input$select_padjmethod,
                                       pvalcutoff = input$select_pvalcutoff,
                                       nCategory = input$select_nCategory)})
      output$dotGO <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { dotGO() }, #Generate plot
            error = function(e) {""}),message = "Plotting...") },width=600,height=600)
      output$net <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { net() }, #Generate plot
            error = function(e) {""}),message = "Plotting...") },width=1000, height=600)
      output$heatGO <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { heatGO() }, #Generate plot
            error = function(e) {""}),message = "Plotting...") },width=1000,height=600)
      output$dotKEGG <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { dotKEGG() }, #Generate plot
            error = function(e) {""}),message = "Plotting...") },width=600,height=600)
      output$heatKEGG <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { heatKEGG() }, #Generate plot
            error = function(e) {""}),message = "Plotting...") },width=1000,height=600)
      
      # Download button for plots
      output$download_dotGO <- downloadHandler(filename = function(){
        paste("GOterm", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- dotGO()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_net <- downloadHandler(filename = function(){
        paste("GOtermNet", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- net()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_heatGO <- downloadHandler(filename = function(){
        paste("GOtermGenesNet", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- heatGO()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_dotKEGG <- downloadHandler(filename = function(){
        paste("KEGGpathway", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- dotKEGG()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      output$download_heatmap <- downloadHandler(filename = function(){
        paste("KEGGheatmap", "png", sep = ".")
      },
      content = function(file){
        png(file)
        plot <- heatKEGG()
        print(plot)
        dev.off()
      }, contentType = "image/png")
      
      
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
                     showCancelButton = TRUE,showConfirmButton = TRUE,confirmButtonCol = "#09C3A2",
                     callbackR = function(x) {
                       if (x != FALSE) {
                         output$ann_table <- NULL
                         output$dotGO = output$net = output$heatGO = output$dotKEGG = output$heatKEGG = NULL
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
    
    #### SERVER: single cell tab #####
    observeEvent(input$atlas, {
      
      # Redirect to single_cell page
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "single_cell")
      
      
      
      output$umap_atlas <- renderImage(
        {
          # Read image
          filename <- normalizePath(paste(experiment_path,"UMAP_CellPopulationColor.png",sep = "/"))
          
          # Read myImage's width and height. These are reactive values, so this
          # expression will re-run whenever they change.
          width  <- session$clientData$output_umap_atlas_width
          height <- session$clientData$output_umap_atlas_height
          
          list(src=filename,
               width=width,
               height=height)
        }, deleteFile = FALSE
      )
      
      ### load all the needed gene finder functions for update the choices in the text input bar for plotting expression
      # 1. Load RData of the selected experiment
      load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
      # Parse gene list
      source("./functions/parse_input_genes.R")
      parsed_user_genelist<-parse_input_genes(input = input$user_genelist, input_specie = selected_specie, annotation_file = annotation_file)
      
      # Observe first if genes input are OK. If not do not create results tab and show error mssg
      {}
      # Create reactive value for characterization
      v = reactiveValues(data = NULL)
      
      observeEvent(input$tabset, {
        v$data <- FALSE
      }) 
      
      output$geneset_sc <- renderText({
        return(paste("Select a tissue-enriched gene in", input$tissue_finder, "to check expression", sep = " "))
      })
      
      # Tissue-specific genes box
      source("./functions/tissue_gene_finder.R")
      v$data = tissue_gene_finder(user_genes = parsed_user_genelist,
                                  tissue = input$tissue_finder,
                                  tissue_atlas = tissue_atlas)
      
      # Annotation of genes
      genes_selected <- parse_input_genes(input=v$data, input_specie = selected_specie, annotation_file = annotation_file)

      updateSelectizeInput(session, 'gene_expr', choices = genes_selected) 
      updateTabItems(session = session,
                     inputId = "tabs",
                     selected = "single_cell")
      
      # create expression plot
      source("./functions/expression_sc_atlas.R")
      output$expr_umap <- renderPlot({
        withProgress(
          tryCatch( # avoid error text
            { plot_expression(experiment_id = input$experiment_id, gene = input$gene_expr, color = input$color_expr)[[2]] }, #Generate plot
            error = function(e) {""}),message = "Plotting single cell atlas...")},width=500,height=700)
      
      output$expr_table <- DT::renderDataTable({
        DT::datatable(plot_expression(experiment_id = input$experiment_id, gene = input$gene_expr, color = input$color_expr)[[1]],
                      options = list(lengthMenu = c(3,5), pageLength = 3))
      })
      output$expr_umap_zoom <- renderPlot({
        s = input$expr_table_rows_selected
        if (length(s)){
          plot_expression(experiment_id = input$experiment_id, gene = input$gene_expr, color = input$color_expr, cellpopulation = s)[[3]]
        }
        else{
          return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
                   text(x = 0.5, y = 0.5, paste("Click over a row to check \n tissue-specific expression"), cex = 1.6, col = "black"))
        }
      },width=400,height=600)
      
      
    })
    
  ## ABOUT TAB
    getPage<-function() {
      return(includeHTML("www/about_tab.html"))
    }
    output$about<-renderUI({getPage()})
  
}

shinyApp(ui = ui, server = server)


