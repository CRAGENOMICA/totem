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
                   selectInput(inputId = NS(id,"color"),
                               label = "Change color",
                               choices = c("salmon","steelblue","olivedrab"),
                               multiple = FALSE),
                   # Download button
                   downloadButton(outputId = NS(id,"download_colored_svg")),
                   
               )
        )
    )
}

colorSVG_Server <- function(id, experiment_path, experiment_id, enrichment_values) {
    
    moduleServer(id, function(input,output,session) {
        
        # LOAD AN R DATA
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        # COLOR SVG AND SAVE PNG
        
        ## Run drawing vector to solve overlapping between tissues in the SVG image
        svg_enrich_values<-drawing_vector(enrichment_values)
        
        ## Color svg acording to color input
        source("./functions/generate_color_scale.R")
        source("./functions/color_svg.R")
        observeEvent(input$color, {
            # Generate color scale
            colors<-generate_color_scale(input = svg_enrich_values,
                                         color = input$color)
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
        output$download_colored_svg <- downloadHandler(filename = "draw.png",content = normalizePath("colored_svg.png"))
        
    })

}

# Testing purposes:
#x<-"/home/flozano/OneDrive/PROJECTS/4_TOTEM/shiny_totem/shinyTOTEM/experiments/Arabidopsis/Root_longitudinal_patterns_Brady2007"
#y<-"Root_longitudinal_patterns_Brady2007"
#z<-c(0.0000000,0.0000000,0.7066333,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.7572440,0.0000000)
#names(z)<-c("Columella","Section_1","Section_2","Section_3","Section_4","Section_5","Section_6","Section_7","Section_8","Section_9","Section_10","Section_11","Section_12")

colorSVG_App <- function(id) {
    
    ui <- fluidPage(
        colorSVG_UI("x")
    )
    
    server<-function(input,output,session) {
        
        colorSVG_Server("x",
                        experiment_path = x,
                        experiment_id = y,
                        enrichment_values = z)
    }
    
    shinyApp(ui, server)
}
