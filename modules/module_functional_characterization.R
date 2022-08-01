##==============================##
## MODULE:                      ##
## Functional characterization  ##
##==============================##

## DESCRIPTION ##
# Server functions
# Funtional characterization tab


functional_characterizationServer<-function(id,experiment_path,user_description,specie,gene_set,tissue){
  moduleServer(id, function(input, output, session){
    
    # Load RData of the selected experiment
    load(paste(experiment_path,"data.RData",sep = "/"))
    
    # Parse gene list
    gene_set = strsplit(gene_set, "\n")[[1]]
    
    # USER DESCRIPTION
    ## Save the experiment description provided by the user. If not provided, save experiment ID and date for file name in downloads
    if(user_description == "Enter a description for your gene list (optional)"){
      # description<<-"CHANGE"
      specie_experiment = strsplit(strsplit(experiment_path, "experiments")[[1]][2], "\\", fixed=T)[[1]]
      description<<-as.character(paste(specie_experiment[2], specie_experiment[3], "experiment", sep = " "))
    }
    else(
      description<<-as.character(user_description)
    )
    # Selected tissue information
    output$description_fc<-renderText({
      return(paste("Characterization of genes enriched in", tissue, "cell population \n", description, sep = " "))
    })
    
    
    
    ## Table output ##
    # Load annotation file of the selected specie
    annotation_file = read.delim(normalizePath(paste("./experiments",specie,"annotation_file.txt",sep = "/")),header = TRUE)
    source("./functions/functional_characterization.R")
    annotation <- reactive(functional_characterization(input_genes=gene_set,
                                                       annotation_file = annotation_file,
                                                       specie = specie))
    output$ann_table <- DT::renderDataTable({
      if(is.null(gene_set)) return()
      withProgress(
        DT::datatable(annotation(),options = list(lengthMenu = c(3, 5, 10, 20, 50), pageLength = 3),rownames= FALSE),
        message = "Retrieving annotation...")
    })
    # download table
    output$download_annotation <- downloadHandler(
      filename = function(){"gene_set_functional_information.csv"},
      content = function(fname){
        write.csv(annotation(), fname)
      }
    )
    
    ## GO plots ##
    source("./functions/GO_plots_new.R")
    dotGO <- reactive({dotplotGO(input_genes=gene_set,
                                 specie = specie,
                                 ontology= input$select_ontology,
                                 padjmethod = input$select_padjmethod,
                                 pvalcutoff = input$select_pvalcutoff)})
    netGO <- reactive({netgenesGO(input_genes=gene_set,
                           specie = specie,
                           ontology= input$select_ontology,
                           padjmethod = input$select_padjmethod,
                           pvalcutoff = input$select_pvalcutoff)})
    heatGO <- reactive({heatmapGO(input_genes=gene_set,
                                  specie = specie,
                                  ontology= input$select_ontology,
                                  padjmethod = input$select_padjmethod,
                                  pvalcutoff = input$select_pvalcutoff)})
    dotKEGG <- reactive({dotplotKEGG(input_genes=gene_set,
                                     specie = specie,
                                     padjmethod = input$select_padjmethod,
                                     pvalcutoff = input$select_pvalcutoff)})
    heatKEGG <- reactive({heatmapKEGG(input_genes=gene_set,
                                      specie = specie,
                                      padjmethod = input$select_padjmethod,
                                      pvalcutoff = input$select_pvalcutoff)})
    output$dotGO <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { dotGO() }, #Generate plot
          error = function(e) {""}),message = "Plotting...")},width=600,height=600)
    output$netGO <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { netGO() }, #Generate plot
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
    ##Filename
    filename = c(gsub(" ", "_", description, fixed = TRUE), # User description / Specie_Experiment
                 "Plot",  #Plot -> to be replaced
                 gsub(" ", "", tissue, fixed = TRUE), #Tissue
                 gsub(" ", "_", gsub(":",";",Sys.time()), fixed = TRUE) # Date (replace : by ; -> invalid filename)
    )
    
    output$download_dotGO <- downloadHandler(filename = function(){
      paste(paste("GOenrichmentDotPlot", filename[1], filename[3], filename[4], sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- dotGO()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_netGO <- downloadHandler(filename = function(){
      paste(paste("GOenrichmentNet", filename[1], filename[3], filename[4], sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- netGO()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_heatGO <- downloadHandler(filename = function(){
      paste(paste("GOenrichmentGenesNet", filename[1], filename[3], filename[4], sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- heatGO()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_dotKEGG <- downloadHandler(filename = function(){
      paste(paste("KEGGpathwayDotPlot", filename[1], filename[3], filename[4], sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- dotKEGG()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_heatKEGG <- downloadHandler(filename = function(){
      paste(paste("GenesKEGGpathway", filename[1], filename[3], filename[4], sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- heatKEGG()
      print(plot)
      dev.off()
    }, contentType = "image/png")
  })
}


# Testing purposes:
# x<-"C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigenomica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM/experiments/Arabidopsis/Root_SingleCell"
# y<-c("AT2G41650\nAT4G39400\nAT1G04560\nAT1G65484\nAT2G21400\nAT5G59310\nAT5G02020")
# z = "Arabidopsis"
# tissue = "CC_B"
# 
# # load(paste(x,"data.RData",sep = "/"))
# 
# functional_characterizationApp <- function(id) {
#     
#     ui <- fluidPage(
#         functional_characterizationUI("fc")
#     )
#     
#     server<-function(input,output,session) {
#         
#         functional_characterizationServer("fc",
#                                  experiment_path = x,
#                                  specie = z,
#                                  gene_set = y,
#                                  tissue = tissue)
#     }
#     
#     shinyApp(ui, server)
# }
# functional_characterizationApp()

