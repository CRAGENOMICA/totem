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
                   selectInput(inputId = NS(id, "color"),
                               label = "Change color",
                               choices = c("salmon","steelblue","olivedrab"),
                               multiple = FALSE),
                   # Download button
                   downloadButton(outputId = NS(id,"download_barplot"))
                   )
               )
    )
}

enrichment_resultsServer <- function(id, experiment_path,user_genelist) {
    
    moduleServer(id, function(input,output,session) {
        
        # LOAD AN R DATA
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        # RUN ENRICHMENT
        source("functions/tissue_enrichment.R")
        enrichment_values_internal <- tissue_enrichment(user_genelist=user_genelist,
                                         tissue_atlas=tissue_atlas,
                                         geneuniverse=geneuniverse)
        # GENERATE BARPLOT
        observeEvent(input$color, {
                
                # Output file
                png("./enrichment_result_barplot.png",height = 18,width = 18,units = "cm",res=300)
                
                # Barplot
                bar<-barplot(height = enrichment_values_internal,
                             beside = TRUE,cex.names = 0.8,las=2,
                             ylab = "-log(p-value)",
                             ylim = c(0,ceiling(max(enrichment_values_internal))),
                             main = "Arabidopsis root longitudinal section enrichment results",
                             col = input$color
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
        
        
        # RETURN ENRICHMENT VECTOR
        list(
            enrichment_values=reactive({enrichment_values_internal})
            )
    })
    
    
}

# Testing purposes:
#x<-"/home/flozano/OneDrive/PROJECTS/4_TOTEM/shiny_totem/shinyTOTEM/experiments/Arabidopsis/Root_longitudinal_patterns_Brady2007"
#y<-c("AT2G01430","AT3G13380","AT4G39400","AT2G27550","AT5G59220","AT5G62420","AT3G20810","AT5G25610","AT1G11600")

enrichment_resultsApp <- function(id) {
    
    ui <- fluidPage(
        enrichment_resultsUI("x")
    )
    
    server<-function(input,output,session) {
        
        enrichment_resultsServer("x",
                                 experiment_path = x,
                                 user_genelist = y)
    }
    
    shinyApp(ui, server)
}
