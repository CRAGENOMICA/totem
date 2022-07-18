##======================##
## MODULE:              ##
## Enrichment  Results  ##
##======================##

## DESCRIPTION ##
# User Interface for variable selection
# Reactive UI
# New experiment tab
# Module 2


enrichment_resultsUI <- function(id) {
    
    # Layout
    fluidPage(
        column(width = 6,offset = 6,align = "center",
               # Output Barlplot
               imageOutput(outputId = NS(id,"barplot"), inline = TRUE),
               
               fluidRow(
                   # Select color
                   selectInput(inputId = NS(id, "color_barplot"),
                               label = "Change color",
                               choices = c("salmon","steelblue","olivedrab"),
                               multiple = FALSE),
                   # Download button
                   downloadButton(outputId = NS(id,"download_barplot"))
                   )
               )
    )
}

enrichment_resultsServer <- function(id, experiment_path,user_genelist,fc_button,sc_button) {
    
    moduleServer(id, function(input,output,session) {
        
        # LOAD AN R DATA
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        # RUN ENRICHMENT
        source("functions/tissue_enrichment.R")
        enrichment_values_internal <- tissue_enrichment(user_genelist=user_genelist,
                                         tissue_atlas=tissue_atlas,
                                         geneuniverse=geneuniverse)
        # GENERATE BARPLOT
        observeEvent(input$color_barplot, {
                
                # Output file
                png("./enrichment_result_barplot.png",height = 18,width = 18,units = "cm",res=300)
                
                # Barplot
                bar<-barplot(height = enrichment_values_internal,
                             beside = TRUE,cex.names = 0.8,las=2,
                             ylab = "-log(p-value)",
                             ylim = c(0,ceiling(max(enrichment_values_internal))),
                             main = "Arabidopsis root longitudinal section enrichment results",
                             col = input$color_barplot
                )
                
                # Add significative threshold line:
                abline(h=3,lty=2,col="tomato")
                
                # Close devidce and save png image
                dev.off()
                
                output$barplot <- renderImage(
                    {
                        # Read image
                        filename <-normalizePath("./enrichment_result_barplot.png")
                        list(src=filename,
                             width="70%",
                             height="70%")
                    }, deleteFile = FALSE
                )
                
                })
            
        
        ## Download button for barplot
        output$download_barplot <- downloadHandler(filename = "enrichment_result_barplot.png",content = normalizePath("enrichment_result_barplot.png"))
        
        ## Functional characterization button 
        observeEvent(input$func_char_tiss,{
          fc_button$func_char_tiss <- input$func_char_tiss #increment fc_button
        })
        
        ## single cell atlas button
        observeEvent(input$single_cell_atlas,{
          sc_button$single_cell_atlas <- input$single_cell_atlas #increment sc_button
        })
        
        # RETURN ENRICHMENT VECTOR
        list(
            enrichment_values=reactive({enrichment_values_internal})
            )
    })
    
    
}
