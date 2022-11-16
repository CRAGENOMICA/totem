##====================================##
## MODULE:                            ##
## Single cell atlas server module    ##
##====================================##

## DESCRIPTION ##
# Server module for single cell atlas
# single cell tab

single_cellServer<-function(id,experiment_path,user_description,experiment_id,specie,gene_set){
  moduleServer(id, function(input, output, session){
    
    # USER DESCRIPTION
    ## Save the experiment description provided by the user. If not provided, save experiment ID and date for file name in downloads
    if(user_description == "Enter a description for your gene list (optional)"){
      # description<<-"CHANGE"
      description_exp<-paste(specie, experiment_id, "experiment \n", Sys.time(), sep = " ")
    }
    else(
      description_exp<-paste("Single cell expression of genes from user experiment: ", user_description, sep = " ")
    )
    # Selected tissue information
    output$description_sc<-renderText({
      return(paste("Single cell expression of genes from", description_exp, sep = " "))
    })
    
    # First column: atlas plot
    ## Atlas seurat plot
    output$umap_atlas <- renderImage(
      {
        # Read image
        filename <- normalizePath(paste(experiment_path,"/UMAP_CellPopulationColor.png",sep = "/"))
        list(src=filename,
             width="100%",
             height="300%")
      }, deleteFile = FALSE
    )
    
    getPage<-function() {
      return(includeHTML("./www/3D.html"))
    }
    output$threeD<-renderUI({getPage()})
    
    
    # Second column: select a gene to check where is expressed
    load(paste(experiment_path,"data.RData",sep = "/"))
    ## Create a UI with the available genes in selected cell population
    choices = gene_set[gene_set %in% unlist(tissue_atlas)]
    if(length(choices) == 0){
      output$geneset_sc<-renderText({
        return("None of the provided genes are enriched in any tissue")
      })
    }
    else{
      output$geneset_sc<-renderUI({
        selectInput(inputId = NS(id,"geneset_sc"),
                    label = "Select a tissue-specific gene",
                    choices = choices, 
                    multiple = FALSE
        )
      })
    }
    
    
    ## Create a box with not enriched and not found genes
    
    # Genes not enriched in any tissue
     source("functions/not_enriched.R")
    output$not_enriched<-renderText({
      not_enriched(user_genes = gene_set,
                   tissue_atlas = tissue_atlas)
    })
    
    # Genes not found
    source("functions/not_found.R")
    output$not_found<-renderText({
      not_found(user_genes = gene_set,
                geneuniverse = geneuniverse)
    })
    
    
    
    ## create expression plot
    source("./functions/expression_sc_atlas.R")
    
    observeEvent(input$geneset_sc, {
      
      output$expr_umap <- renderPlot({
      withProgress(
        tryCatch( # avoid error text
          { plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[2]] }, #Generate plot
          error = function(e) {""}),message = "Plotting single cell atlas...")
        },width=600,height=600)
    
      # Third column: specific expression values
      output$expr_table <- DT::renderDataTable({
        DT::datatable(plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[1]],
                      options = list(lengthMenu = c(3,5,10), pageLength = 6),rownames = FALSE)
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
      },width=500,height=600)
      
    })

    
  })
}

# Testing purposes:
# setwd("C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigenòmica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM_actual/shinyTOTEM")
x<-"./experiments/Arabidopsis/Root_SingleCell"
c("AT2G41650","AT4G39400","AT1G04560","AT1G65484","AT2G21400","AT5G59310","AT5G02020")
# c("AT2G01430","AT2G41650","AT3G13380","AT4G39400","AT2G27550","AT5G59220","AT5G62420","AT3G20810","AT5G25610","AT1G11600")
z<-c("AT2G41650", "AT4G39400","AT1G04560","AT1G65484","AT2G21400","AT5G59310","AT5G02020")
sp = "Arabidopsis"
y = "Root_SingleCell"
desc<-"Enter a description for your gene list (optional)"


single_cellApp <- function(id) {

  ui <- fluidPage(
    single_cellUI("sc")
  )

  server<-function(input,output,session) {

    single_cellServer("sc",
                      experiment_path = x,
                      user_description = desc,
                      experiment_id = y,
                      specie = sp,
                      gene_set = z)
  }

  shinyApp(ui, server)
}
# single_cellApp()

