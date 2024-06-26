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

enrichment_resultsServer <- function(id, experiment_path,user_description,experiment_id,specie,user_genelist,fc_button) { 
    
    moduleServer(id, function(input,output,session) {
      
          # USER DESCRIPTION
          ## Save the experiment description provided by the user. If not provided, save experiment ID and date for file name in downloads
          if(user_description == "Enter a description for your gene list (optional)"){
            # description<<-"CHANGE"
            description_exp<-paste(specie, experiment_id, "experiment \n", Sys.time(),sep = " ")
          }
          else(
            description_exp<-user_description
          )
  
        # add user provided description
        output$description_exp <- renderText({
          return(description_exp)
        })
        
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
                # png("./enrichment_result_barplot.png",height = 20,width = 18,units = "cm",res=400)
                # par(mar=c(4,11,4,4))
                # # Barplot
                # bar<-barplot(height = enrichment_values_internal,
                #              beside = TRUE,cex.names = 0.6,las=1,horiz=T,
                #              xlab = "-log(p-value enrichment)",
                #              xlim = c(0,ceiling(max(enrichment_values_internal))),
                #              main = paste(specie, paste(strsplit(experiment_id, "_")[[1]],collapse=" "), "enrichment results", sep = " "),
                #              col = input$color_barplot
                # )
                # 
                # # Add significative threshold line in 0.05 pval:
                # abline(v=(-log10(0.05)),lty=2,col="tomato")
                # 
                # # Close devidce and save png image
                # dev.off()
                mybarplot2(myvector = enrichment_values_internal, color = input$color_barplot, outputfile = "./enrichment_result_barplot.png")
                
                # plot the image saved of barplot
                output$barplot <- renderImage(
                    {
                        # Read image
                        filename <-normalizePath("./enrichment_result_barplot.png")
                        list(src=filename,
                             width="70%",
                             height="80%")
                    }, deleteFile = FALSE
                )
                
        })
            
        
        ## Download button for barplot
        #filename
        filename = c(gsub(" ", "_", gsub("\n", "",gsub(":",".",description_exp)), 
                          fixed = TRUE) # User description / Specie_Experiment / Date (replace : by ; -> invalid filename)
        )
        #download button
        output$download_barplot <- downloadHandler(
        filename = function(){
          paste(paste("EnrichmentBarplot", filename, sep = "_"), "png", sep = ".")
        },
        content = function(file) {
          file.copy("enrichment_result_barplot.png", file)
        }, contentType = "image/png")
        
        
        ## Functional characterization button 
        observeEvent(input$func_char_tiss,{
          fc_button$func_char_tiss <- input$func_char_tiss #increment fc_button
        })
        
        # RETURN ENRICHMENT VECTOR
        list(
            enrichment_values=reactive({enrichment_values_internal})
            )
    })
    
    
}

