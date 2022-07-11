##==============================##
## MODULE:                      ##
## Functional characterization  ##
##==============================##

## DESCRIPTION ##
# Server functions
# Funtional characterization tab
# Module 3

functional_characterizationUI <- function(id) {
  
  # Layout
  fluidPage(
    verbatimTextOutput(outputId = NS(id,"geneset")),
    
    br(),
    
    column(12,
           # Annotation table
           box(title = "Annotation of selected genes",solidHeader=FALSE, collapsible=TRUE, width = 12, 
               style = 'overflow-x: scroll;', ### add a scroll bar
               DT::dataTableOutput(NS(id,'ann_table')),
               downloadButton(outputId = NS(id,"download_annotation"), label = "Download table")
           )
    ),
    
    hr(),
    
    column(12,
           box(title = "Gene Ontology analysis of selected genes",solidHeader=FALSE, collapsible=TRUE,width = 12,
               style = "width: 100%;height: 60em;",
               sidebarLayout(
                 # side panel for GO parameters reactive inputs
                 sidebarPanel(width = 3,
                              
                              selectInput(inputId = NS(id,"select_ontology"),
                                          label = "Select the ontology",
                                          choices = c("Biological process" = "GO:BP",
                                                      "Molecular function" = "GO:MF",
                                                      "Celular component" = "GO:CC"),
                                          selected = "GO:BP", multiple = FALSE),
                              br(),# br() element to introduce extra vertical spacing
                              selectInput(inputId = NS(id,"select_padjmethod"),
                                          label = "Select the method for p-value adjustment",
                                          choices = c("Bonferroni" = "bonferroni", "Benjamini & Hochberg (FDR)" = "fdr", 
                                                      "gProfiler algorithm (g:SCS)" = "gSCS"),
                                          selected = "fdr", multiple = FALSE),
                              br(),
                              numericInput(NS(id,"select_pvalcutoff"), "p-value cut-off", 0.05, step = 0.01),
                              br(),
                              
                              sliderInput(NS(id,"select_nCategory"), "Maximum number of GO categories to show", value = 20, min = 1, max = 50)
                              
                 ),
                 
                 # Main panel for displaying GO plot outputs
                 mainPanel( width = 9,
                            
                            tabsetPanel(type = "tabs",
                                        tabPanel("Dotplot: GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_dotGO"), label = "Download GO term plot"),
                                                 plotOutput(outputId = NS(id,"dotGO"), width = "100%")),
                                        tabPanel("Net: GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_net"), label = "Download GO term net"),
                                                 plotOutput(outputId = NS(id,"net"), width = "100%")),
                                        tabPanel("Heatmap: Genes in GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_heatGO"), label = "Download GO term heatmap"),
                                                 plotOutput(outputId = NS(id,"heatGO"), width = "100%")),
                                        tabPanel("Dotplot: KEGG pathways", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_dotKEGG"), label = "Download KEGG pathway plot"),
                                                 plotOutput(outputId = NS(id,"dotKEGG"), width = "100%")),
                                        tabPanel("Heatmap: Genes in KEGG pathways", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_heatmap"), label = "Download KEGG pathway heatmap"),
                                                 plotOutput(outputId = NS(id,"heatKEGG"), width = "100%"))
                                        
                            )
                 )
               )
           )
    )
  )
}

not_enriched_Server <- function(id, parsed_genelist, tissue_atlas) {
    
    moduleServer(id, function(input,output,session) {
      
      # description of selected gene list 
      output$geneset <- renderText({
        return(paste("Characterization of genes not enriched in any tissue"))
      })
      
      # extract gene list of not enriched genes
      source("./functions/not_enriched.R")
      genes = not_enriched(user_genes = parsed_genelist,
                            tissue_atlas = tissue_atlas)
      
      genes = strsplit(x = genes,split = "\n")[[1]]
      
      #return the gene list
      list(
        gene_set = genes
      )
    })
}

not_found_Server <- function(id, parsed_genelist,geneuniverse) {
  
  moduleServer(id, function(input,output,session) {
    
    
    # description of selected gene list 
    output$geneset <- renderText({
      return(paste("Characterization of genes not found in the experiment"))
    })
    
    source("./functions/not_found.R")
    genes = not_found(user_genes = parsed_genelist,
                       geneuniverse = geneuniverse)
    genes = strsplit(x = genes,split = "\n")[[1]]
    
    #return the gene list
    list(
      gene_set = genes
    )
    
  })
  
}

tissue_enr_Server <- function(id, parsed_genelist,tissue_atlas,tissue_finder) {
  
  moduleServer(id, function(input,output,session) {

    # description of selected gene list 
    output$geneset <- renderText({
      return(paste("Characterization of tissue-enriched genes in", tissue_finder, sep = " "))
    })
  
    # Tissue-specific genes box
    source("./functions/tissue_gene_finder.R")
    
    genes = tissue_gene_finder(user_genes = parsed_genelist,
                                tissue = tissue_finder,
                                tissue_atlas = tissue_atlas)
    genes = strsplit(x = genes,split = "\n")[[1]]
    
    #return the gene list
    list(
      gene_set = genes
    )
  })
}

table_plots_funct_charact<-function(id, gene_set, specie){
  moduleServer(id, function(input, output, session){

    annotation_file = read.delim(normalizePath(paste("./experiments",specie,"annotation_file.txt",sep = "/")),header = TRUE)

    # Annotation of genes
    source("./functions/functional_characterization.R")
    annotation <- reactive(functional_characterization(input_genes=gene_set,
                                                       annotation_file = annotation_file,
                                                       specie = specie))
                         
    
    output$ann_table <- DT::renderDataTable({
      if(is.null(gene_set)) return()
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
    dotGO <- reactive({dotplotGO(input_genes=gene_set,
                                 specie = specie,
                                 ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                 pvalcutoff = input$select_pvalcutoff,
                                 nCategory = input$select_nCategory)})
    net <- reactive({netGO(input_genes=gene_set,
                           specie = specie,
                           ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                           pvalcutoff = input$select_pvalcutoff,
                           nCategory = input$select_nCategory)})
    heatGO <- reactive({heatmapGO(input_genes=gene_set,
                                  specie = specie,
                                  ontology= input$select_ontology,padjmethod = input$select_padjmethod,
                                  pvalcutoff = input$select_pvalcutoff,
                                  nCategory = input$select_nCategory)})
    dotKEGG <- reactive({dotplotKEGG(input_genes=gene_set,
                                     specie = specie,
                                     padjmethod = input$select_padjmethod,
                                     pvalcutoff = input$select_pvalcutoff,
                                     nCategory = input$select_nCategory)})
    heatKEGG <- reactive({heatmapKEGG(input_genes=gene_set,
                                      specie = specie,
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

  })
}


functional_characterizationServer<-function(id,experiment_path,user_genelist, specie){
  moduleServer(id, function(input, output, session){
    
    # Load RData of the selected experiment
    load(paste(experiment_path,"data.RData",sep = "/"))
    
    # Load annotation file of the selected specie
    annotation_file = read.delim(normalizePath(paste("./experiments",specie,"annotation_file.txt",sep = "/")),header = TRUE)
    
    # Parse gene list
    source("./functions/parse_input_genes.R")
    parsed_user_genelist<-parse_input_genes(input = user_genelist, input_specie = specie, annotation_file = annotation_file)
    
    # Observe first if genes input are OK. If not do not create results tab and show error mssg
    {}
    
    # Redirect to results page
    updateTabItems(session = session,
                   inputId = "tabs",
                   selected = "functional_char")
    
    # return the list with the parsed genes
    list(
      parsed_genelist=parsed_user_genelist,
      tissue_atlas = tissue_atlas,
      geneuniverse = geneuniverse
    )
  })
}


# Testing purposes:
# x<-"C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigen?mica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM/experiments/Sorghum/Inflorescence_Davidson2012/"
# y=c("Sb01g000230
# Sb01g000240
# Sb01g000250
# Sb01g000255
# Sb01g000260
# Sb01g000270
# Sb01g000290
# Sb01g000300
# Sb01g000310
# Sb01g000330
# Sb01g000340
# ")
x<-"C:/Users/vered/OneDrive - CRAG - Centre de Recerca en Agrigenòmica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM/experiments/Arabidopsis/Root_SingleCell"
y<-c("AT2G01430,AT3G13380,AT4G39400,AT2G27550,AT5G59220,AT5G62420,AT3G20810,AT5G25610,AT1G11600")
# z = "Sorghum"
z = "Arabidopsis"
list_genes = "Not enriched"
tissue = "CC_B"

# load(paste(x,"data.RData",sep = "/"))

functional_characterizationApp <- function(id) {
    
    ui <- fluidPage(
        functional_characterizationUI("fc")
    )
    
    server<-function(input,output,session) {
        
        a<-functional_characterizationServer("fc",
                                 experiment_path = x,
                                 user_genelist = y,
                                 specie = z)
        
        if(list_genes == "Not enriched"){
        b = not_enriched_Server("fc",
                                parsed_genelist = a$parsed_genelist,
                                tissue_atlas = a$tissue_atlas)
        }
        else if(list_genes == "Not found"){
        b<-not_found_Server("fc",
                             parsed_genelist = a$parsed_genelist,
                             geneuniverse = a$geneuniverse)

        }
        else if(list_genes == "Enriched"){
          b<-tissue_enr_Server("fc",
                                parsed_genelist = a$parsed_genelist,
                                tissue_atlas = a$tissue_atlas,
                                tissue_finder = tissue)

        }
        c<-table_plots_funct_charact("fc",
                                     gene_set = b$gene_set,
                                     specie = z)
    }
    
    shinyApp(ui, server)
}
