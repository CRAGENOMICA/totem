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
    
    ## FOR MAIZE ENDOSPERM SINGLE CELL WE DON'T HAVE DATA TO PLOT. REDIRECT USER TO THE OFICIAL PAGE OF THE AUTHORS WITH A BUTON
    if(experiment_id == "Endosperm_SingleCell"){
      
      shinyalert(
        title = "Go to oficial endosperm browser",
        text = "Authors from original study developed a web tool to check expression. \n Click to go to their web page",
        type = "info",
        showCancelButton = TRUE,
        showConfirmButton = TRUE,
        confirmButtonCol = "darkgreen",
        callbackR = function(x) {
          if (x == FALSE) {
            # if cancel button is clicked
            # Update tab to enrichment result tab
            updateTabItems(session = session,
                            inputId = "tabs",
                            selected = NS(id,"results"))
          }
          else {
            browseURL("https://maize-endosperm.cn/")
          }
        }
      )
      
   
    }
    else{
      # First column: atlas plot
      ## Atlas seurat plot
      output$umap_atlas <- renderImage(
        {
          # Read image
          filename <- normalizePath(paste(experiment_path,"/UMAP_CellPopulationColor.png",sep = "/"))
          list(src=filename,
               width="100%",
               height=900, display="inline-block")
        }, deleteFile = FALSE
      )
      
      ## 3D plots
      addResourcePath('myhtmlfiles', experiment_path)
      getPage <- function() {
        return(tags$iframe(src = paste0("myhtmlfiles/", "threeDatlas.html"), height =980, width = "100%", scrolling = "yes", display="inline-block"))
      }
      
      output$threeD <- renderUI({
        getPage()
      })
      
      
      # Second column: select a gene to check where is expressed
      load(paste(experiment_path,"data.RData",sep = "/"))
      
      ## Create a UI with the available genes in selected cell population
      
      if(length(gene_set) == 0){
        output$geneset_sc<-renderText({
          return("None of the provided genes are enriched in any tissue")
        })
      }
      else{
        output$geneset_sc<-renderUI({
          selectInput(inputId = NS(id,"geneset_sc"),
                      label = "Select a gene",
                      choices = gene_set,
                      selected = gene_set[1],
                      multiple = FALSE
          )
        })
      }
      
      #Create a box with the enrichment information of the input genes 
      observeEvent(input$geneset_sc, {
        selected_gene<<-input$geneset_sc
        desc <<- NULL
        enr <- selected_gene[selected_gene %in% unlist(tissue_atlas)]
        if(length(enr)>0){
          desc_enr <<- paste0(enr, " is enriched in: ", paste(names(unlist(sapply(tissue_atlas, function(x) which(x %in% enr)))), collapse = ", "))
        }
        else{
          desc_enr <<- ""
        }
        notenr <- selected_gene[selected_gene %in% unlist(tissue_atlas) == FALSE & selected_gene %in% geneuniverse]
        if(length(notenr)>0){
          desc_notenr <<- paste0(notenr, " is not enriched in any tissue")
        }
        else{
          desc_notenr <<- ""
        }
        not <- selected_gene[selected_gene %in% geneuniverse ==FALSE]
        if(length(not)>0){
          desc_not <<- paste0(not, " is not found in the selected experiment")
        }
        else{
          desc_not <<- ""
        }
        desc <<- paste0(desc_enr, desc_notenr, desc_not)
        output$enrich_description<-renderText(
          return(desc)
        )
      })
      
      ## create expression plot
      source("./functions/expression_sc_atlas.R")
      
      observeEvent(input$geneset_sc, {
        
        output$expr_umap <- renderPlot({
          if(grepl("not found",desc_not)){
            return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
                     text(x = 0.5, y = 0.5, paste("Please select a gene available \n in the experiment universe"), cex = 1.6, col = "black"))
          }
          else{
            withProgress(
              tryCatch( # avoid error text
                { plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[2]] }, #Generate plot
                error = function(e) {""}),message = "Plotting single cell atlas...")
          }
          
        },width=600,height=800)
        
        output$expr_violin <- renderPlot({
          if(grepl("not found",desc_not)){
            return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
                     text(x = 0.5, y = 0.5, paste(""), cex = 1.6, col = "black"))
          }
          else{
            withProgress(
              tryCatch( # avoid error text
                { plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[3]] }, #Generate plot
                error = function(e) {""}),message = "Plotting violin plot of expressions...")
          }
        },width=450,height=800)
        
        #Download button
        output$download_scExpression <- downloadHandler(filename = function(){
          paste(paste("SingleCellExpression", selected_gene, experiment_id, sep = "_"), "png", sep = ".")
        },
        content = function(file){
          png(file, width = 980, height = 700, units = "px", pointsize = 12, bg = "white", res = NA)
          plot <- plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[2]]+
            plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[3]]
          print(plot)
          dev.off()
        }, contentType = "image/png")
        
        # Third column: specific expression values
        output$expr_table <- DT::renderDataTable({
          if(grepl("not found",desc_not)){
            shiny::showNotification("No data", type = "error")
            NULL
          }
          else{
            DT::datatable(plot_expression(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr)[[1]],
                          options = list(lengthMenu = c(5,10,20), pageLength = 8),rownames = FALSE)
          }
        })
        output$expr_umap_zoom <- renderPlot({
          if(grepl("not found",desc_not)){
            shiny::showNotification("No data", type = "error")
            NULL
          }
          else{
            s = input$expr_table_rows_selected
            if (length(s)){
              plot_expression_population(experiment_path = experiment_path, gene = input$geneset_sc, color = input$color_expr, cellpopulation = s)[[3]]
            }
            else{
              return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
                       text(x = 0.5, y = 0.5, paste("Click over a row of the table in the left \n to check tissue-specific expression"), cex = 1.6, col = "black"))
            }
          }
        },width=550,height=800)
        
      })
    }
    
  })
}

# # Testing purposes:
# # setwd("C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigenòmica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM_actual/shinyTOTEM")
# x<-"./experiments/Arabidopsis thaliana/Leaf_SingleCell"
# # c("AT2G41650","AT4G39400","AT1G04560","AT1G65484","AT2G21400","AT5G59310","AT5G02020")
# # c("AT2G01430","AT2G41650","AT3G13380","AT4G39400","AT2G27550","AT5G59220","AT5G62420","AT3G20810","AT5G25610","AT1G11600")
# z<-c("AT2G41650", "AT4G39400","AT1G04560","AT1G65484","AT2G21400","AT5G59310","AT5G02020")
# sp = "Arabidopsis thaliana"
# y = "Leaf_SingleCell"
# desc<-"Enter a description for your gene list (optional)"
# 
# 
# single_cellApp <- function(id) {
# 
#   ui <- fluidPage(
#     single_cellUI("sc")
#   )
# 
#   server<-function(input,output,session) {
# 
#     single_cellServer("sc",
#                       experiment_path = x,
#                       user_description = desc,
#                       experiment_id = y,
#                       specie = sp,
#                       gene_set = z)
#   }
# 
#   shinyApp(ui, server)
# }
# single_cellApp()
# 
