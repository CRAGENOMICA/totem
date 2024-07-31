##==============================##
## MODULE:                      ##
## Functional characterization  ##
##==============================##

## DESCRIPTION ##
# Server functions
# Funtional characterization tab


functional_characterizationServer<-function(id,experiment_path,user_description,experiment_id,specie,gene_set,tissue){
  moduleServer(id, function(input, output, session){
    
    # Load RData of the selected experiment
    load(paste(experiment_path,"data.RData",sep = "/"))
    
    # Parse gene list
    gene_set = strsplit(gene_set, "\n")[[1]]
    
    # USER DESCRIPTION
    ## Save the experiment description provided by the user. If not provided, save experiment ID and date for file name in downloads
    if(user_description == "Enter a description for your gene list (optional)"){
      # description<<-"CHANGE"
      description_exp<-paste(specie, experiment_id, "experiment \n", Sys.time(), sep = " ")
    }
    else(
      description_exp<-user_description
    )
    
    # Selected tissue information
    output$description_fc<-renderText({
      return(paste("Characterization of genes enriched in", tissue, "cell population \n", description_exp, sep = " "))
    })
    
    
    
    ## Table output ##
    # Load annotation file of the selected specie
    annotation_file = read.delim(normalizePath(paste("./experiments",specie,"annotation_file.txt",sep = "/")),header = TRUE)
    source("./functions/functional_characterization.R")
    annotation <- reactive({functional_characterization(input_genes=gene_set,
                                                       annotation_file = annotation_file)})
    
    # Render table
      output$ann_table = DT::renderDataTable({
        if(is.null(gene_set)) return()
        withProgress(
          datatable(
            annotation(), rownames = FALSE,
            extensions = 'Buttons', # Use buttons extesion for colvis (column visualization) plugin
            options = list(lengthMenu = c(3, 5, 10, 20, 50), pageLength = 3,
                           dom = 'Blfrtip', 
                           buttons = list(list(extend = 'colvis', text = "Select columns to display",
                                               columns = c(3:(ncol(annotation_file)-1)))), ## column index start at 0 -> show only the first 3 columns
                           columnDefs = list( list(targets = c(3:(ncol(annotation_file)-1)), visible = FALSE))) ## only the first 3 columns are visible by default
          ),
          message = "Retrieving annotation...")
      })

    
    # download table
  
    filename_table = c(gsub(" ", "_", gsub("\n", "",gsub(":",".",description_exp)), fixed = TRUE), # User description / Specie_Experiment / Date (replace : by ; -> invalid filename)
                 gsub(" ", "", tissue, fixed = TRUE) #Tissue
    )
    output$download_annotation <- downloadHandler(
      filename = function(){paste(paste("AnnotationTable", paste(filename, collapse = "_"), sep = "_"), "csv", sep = ".")},
      content = function(fname){
        write.csv(annotation(), fname) ## The entire table is downloaded
      }
    )
    
    ## GO plots ##
    source("./functions/GO_plots_new.R")
    dotGO <- reactive({dotplotGO(genes=gene_list_org(input_genes=gene_set,specie = specie)[[1]],
                                 org = gene_list_org(input_genes=gene_set,specie = specie)[[2]],
                                 ontology= input$select_ontology,
                                 padjmethod = input$select_padjmethod,
                                 pvalcutoff = input$select_pvalcutoff)})
    netGO <- reactive({netgenesGO(genes=gene_list_org(input_genes=gene_set,specie = specie)[[1]],
                                  org = gene_list_org(input_genes=gene_set,specie = specie)[[2]],
                                 ontology= input$select_ontology,
                                 padjmethod = input$select_padjmethod,
                                 pvalcutoff = input$select_pvalcutoff)})
    heatGO <- reactive({heatmapGO(genes=gene_list_org(input_genes=gene_set,specie = specie)[[1]],
                                  org = gene_list_org(input_genes=gene_set,specie = specie)[[2]],
                                  ontology= input$select_ontology,
                                  padjmethod = input$select_padjmethod,
                                  pvalcutoff = input$select_pvalcutoff)})
    dotKEGG <- reactive({dotplotKEGG(genes=gene_list_org(input_genes=gene_set,specie = specie)[[1]],
                                     org = gene_list_org(input_genes=gene_set,specie = specie)[[2]],
                                     padjmethod = input$select_padjmethod,
                                     pvalcutoff = input$select_pvalcutoff)})
    heatKEGG <- reactive({heatmapKEGG(genes=gene_list_org(input_genes=gene_set,specie = specie)[[1]],
                                      org = gene_list_org(input_genes=gene_set,specie = specie)[[2]],
                                      padjmethod = input$select_padjmethod,
                                      pvalcutoff = input$select_pvalcutoff)})
    output$dotGO <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { dotGO() }, #Generate plot
          error = function(e) {""}),message = "Plotting...")},width=600,height=800)
    output$netGO <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { netGO() }, #Generate plot
          error = function(e) {""}),message = "Plotting...")},width=1000, height=800)
    output$heatGO <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { heatGO() }, #Generate plot
          error = function(e) {""}),message = "Plotting...")},width=1000,height=800)
    output$dotKEGG <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { dotKEGG() }, #Generate plot
          error = function(e) {""}),message = "Plotting...")},width=600,height=800)
    output$heatKEGG <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { heatKEGG() }, #Generate plot
          error = function(e) {""}),message = "Plotting...")},width=1000,height=800)
    
    ontology = ""
    observeEvent(input$select_ontology,{
      ontology<-as.character(input$select_ontology)
    })
    # Download button for plots
    ##Filename
    filename = c(gsub(" ", "_", gsub("\n", "",gsub(":",".",description_exp)), fixed = TRUE), # User description / Specie_Experiment / Date (replace : by ; -> invalid filename)
                 gsub(" ", "", tissue, fixed = TRUE), #Tissue
                 gsub("GO:","", ontology, fixed = TRUE) # ontology
    )
    
    output$download_dotGO <- downloadHandler(filename = function(){
      paste(paste("GOenrichmentDotPlot_", paste(filename, collapse = "_")), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- dotGO()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_netGO <- downloadHandler(filename = function(){
      paste(paste("GOenrichmentNet", paste(filename, collapse = "_"), sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- netGO()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_heatGO <- downloadHandler(filename = function(){
      paste(paste("GenesGOenrichment", paste(filename, collapse = "_"), sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- heatGO()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_dotKEGG <- downloadHandler(filename = function(){
      paste(paste("KEGGpathwayDotPlot", paste(filename, collapse = "_"), sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- dotKEGG()
      print(plot)
      dev.off()
    }, contentType = "image/png")
    
    output$download_heatKEGG <- downloadHandler(filename = function(){
      paste(paste("GenesKEGGpathway", paste(filename, collapse = "_"), sep = "_"), "png", sep = ".")
    },
    content = function(file){
      png(file)
      plot <- heatKEGG()
      print(plot)
      dev.off()
    }, contentType = "image/png")
  })
}


# # Testing purposes:
# x<-"C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigen?mica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM_actual/shinyTOTEM/experiments/Arabidopsis/Root_SingleCell"
# z<-c("AT2G41650\nAT4G39400\nAT1G04560\nAT1G65484\nAT2G21400\nAT5G59310\nAT5G02020")
# sp = "Arabidopsis"
# y = "Root_SingleCell"
# desc<-"Enter a description for your gene list (optional)"
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
#                                  user_description = desc,
#                                  experiment_id = y,
#                                  specie = sp,
#                                  gene_set = z,
#                                  tissue = tissue)
#     }
# 
#     shinyApp(ui, server)
# }
# functional_characterizationApp()

