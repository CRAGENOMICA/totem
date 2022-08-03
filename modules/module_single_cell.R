##====================================##
## MODULE:                            ##
## Single cell atlas server module    ##
##====================================##

## DESCRIPTION ##
# Server module for single cell atlas
# single cell tab

single_cellServer<-function(id,user_description,experiment_id,specie,gene_set,tissue){
  moduleServer(id, function(input, output, session){
    
    ## Creation experiment path
    experiment_path = paste("./experiments", specie, experiment_id, sep = "/")
    
    # USER DESCRIPTION
    ## Save the experiment description provided by the user. If not provided, save experiment ID and date for file name in downloads
    if(user_description == "Enter a description for your gene list (optional)"){
      # description<<-"CHANGE"
      description<<-as.character(paste(specie, experiment_id, "experiment", sep = " "))
    }
    else(
      description<<-as.character(user_description)
    )
    # Selected tissue information
    output$description_sc<-renderText({
      return(paste("Single cell expression of genes enriched in", tissue, "cell population \n", description, sep = " "))
    })
    
    # First column: atlas plot
    ## Atlas seurat plot
    output$umap_atlas <- renderImage(
      {
        # Read image
        filename <- normalizePath(paste(experiment_path,"/UMAP_CellPopulationColor.png",sep = "/"))
        list(src=filename,
             width="100%",
             height="150%")
      }, deleteFile = FALSE
    )
    
    # Second column: select a gene to check where is expressed
    ## Create a UI with the available genes in selected cell population
    output$geneset_sc<-renderUI({
      selectInput(inputId = NS(id,"geneset_sc"),
                  label = "Select a gene to check expression",
                  choices = strsplit(gene_set, "\n")[[1]], #parse list tissue specific genes
                  multiple = FALSE
      )
    })
    
    
    ## create expression plot
    source("./functions/expression_sc_atlas.R")
    
    observeEvent(input$geneset_sc, {
      
      output$expr_umap <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[2]] }, #Generate plot
          error = function(e) {""}),message = "Plotting single cell atlas...")},width=500,height=600)
    
      # Third column: specific expression values
      output$expr_table <- DT::renderDataTable({
        DT::datatable(plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[1]],
                      options = list(lengthMenu = c(3,5), pageLength = 3),rownames = FALSE,)
      })
      output$expr_umap_zoom <- renderPlot({
        s = input$expr_table_rows_selected
        if (length(s)){
          plot_expression_population(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr, cellpopulation = s)[[3]]
        }
        else{
          return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
                   text(x = 0.5, y = 0.5, paste("Click over a row to check \n tissue-specific expression"), cex = 1.6, col = "black"))
        }
      },width=400,height=600)
      
    })

    
  })
}