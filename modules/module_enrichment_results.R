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

enrichment_resultsServer <- function(id, experiment_path,user_genelist,user_description,fc_button,sc_button) {
    
    moduleServer(id, function(input,output,session) {
      
          # USER DESCRIPTION
          ## Save the experiment description provided by the user. If not provided, save experiment ID and date for file name in downloads
          specie_experiment = strsplit(strsplit(experiment_path, "experiments")[[1]][2], "\\", fixed=T)[[1]]
          if(user_description == "Enter a description for your gene list (optional)"){
            # description<<-"CHANGE"
            description<<-as.character(paste(specie_experiment[2], specie_experiment[3], "experiment", sep = " "))
          }
          else(
            description<<-as.character(user_description)
          )
  
        # add user provided description
        output$description <- renderText({
          return(description)
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
                png("./enrichment_result_barplot.png",height = 18,width = 18,units = "cm",res=400)
                par(mar=c(11,4,4,4))
                # Barplot
                bar<-barplot(height = enrichment_values_internal,
                             beside = TRUE,cex.names = 0.6,las=2,
                             ylab = "-log(p-value)",
                             ylim = c(0,ceiling(max(enrichment_values_internal))),
                             main = paste(specie_experiment[2], specie_experiment[3], "enrichment results", sep = " "),
                             col = input$color_barplot
                )
                
                # Add significative threshold line:
                abline(h=3,lty=2,col="tomato")
                
                # Close devidce and save png image
                dev.off()
                
                # plot the image saved of barplot
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
        #filename
        filename = c(gsub(" ", "_", description, fixed = TRUE), # User description / Specie_Experiment
                     "Plot",  #Plot -> to be replaced
                     gsub(" ", "_", gsub(":",";",Sys.time()), fixed = TRUE) # Date (replace : by ; -> invalid filename)
        )
        #download button
        output$download_barplot <- downloadHandler(
        filename = function(){
          paste(paste("EnrichmentBarplot", filename[1], filename[3], sep = "_"), "png", sep = ".")
        },
        content = function(file) {
          file.copy("enrichment_result_barplot.png", file)
        }, contentType = "image/png")
        
        
        ## Functional characterization button 
        observeEvent(input$func_char_tiss,{
          fc_button$func_char_tiss <- input$func_char_tiss #increment fc_button
        })
        
        ## Single cell button -> only visible if the experiment is SingleCell 
        output$single_cell_atlas <- renderUI(expr = if(grepl("SingleCell", specie_experiment[3])){
          actionButton(inputId = NS(id,"single_cell_atlas"), label = "Single cell atlas", align = "center")
          
        }
        else{
          NULL
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
