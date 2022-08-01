##======================##
## MODULE:              ##
## Color SVG            ##
##======================##

## DESCRIPTION ##
# Color SVG images
# Module 3


colorSVG_UI <- function(id) {
    
    # Layout
    fluidRow(
        column(width = 6,align = "left",
               # Output Colored SVG
               tags$head(tags$style(### adjust image to the windows size
                   type = "text/css",
                   "#colored_svg img {max-width: 100%; width: auto; max-height: 200%; height: auto}" 
               )),
               # Output image
               imageOutput(outputId = NS(id,"colored_svg")),
               
               fluidRow(
                   
                   # Select color
                   selectInput(inputId = NS(id,"color_svg"),
                               label = "Change color",
                               choices = c("salmon","steelblue","olivedrab"),
                               multiple = FALSE),
                   # Download button
                   downloadButton(outputId = NS(id,"download_colored_svg")),
                   
               )
        )
    )
}

colorSVG_Server <- function(id, experiment_path, user_description, experiment_id, enrichment_values) {
    
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
        
        # LOAD AN R DATA
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        # COLOR SVG AND SAVE PNG
        
        ## Run drawing vector to solve overlapping between tissues in the SVG image
        svg_enrich_values<-drawing_vector(enrichment_values)
        
        ## Color svg acording to color input
        source("./functions/generate_color_scale.R")
        source("./functions/color_svg.R")
        observeEvent(input$color_svg, {
            # Generate color scale
            colors<-generate_color_scale(input = svg_enrich_values,
                                         color = input$color_svg)
            # Color_svg
            color_svg(input_svg=normalizePath(paste(experiment_path,paste(experiment_id,"svg",sep="."),sep = "/")),
                      tissue_colors=colors,
                      output_file="colored_svg.png")

            # Output colored SVG
            output$colored_svg <- renderImage(
                {
                    # Read image
                    filename <- normalizePath("colored_svg.png")
                    list(src=filename,
                         width="50%",
                         height="100%")
                }, deleteFile = FALSE
            )
            
        })
        
        ## Download button for SVG
        #filename
        filename = c(gsub(" ", "_", description, fixed = TRUE), # User description / Specie_Experiment
                     "Plot",  #Plot -> to be replaced
                     gsub(" ", "_", gsub(":",";",Sys.time()), fixed = TRUE) # Date (replace : by ; -> invalid filename)
        )
        #download button
        output$download_colored_svg <- downloadHandler(
        filename = function(){
          paste(paste("EnrichmentColoredSVGimage", filename[1], filename[3], sep = "_"), "png", sep = ".")
        },
        content = function(file) {
          file.copy("colored_svg.png", file)
        }, contentType = "image/png")
        
    })

}
