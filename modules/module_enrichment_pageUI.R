##======================##
## MODULE:              ##
## Enrichment  Page UI  ##
##======================##

## DESCRIPTION ##
# User Interface for the overall enrichment page UI
# Reactive UI
# New experiment tab
# Module 2

enrichment_pageUI <- function(id) {
    
    # Layout
    fluidPage(
      
        ## Add the description given by the user in the New search tab
        verbatimTextOutput(outputId =  NS(id,"description")),
      
        br(),
        
        column(width = 7,align = "center",
               
               # Output Colored SVG
               tags$head(tags$style(### adjust image to the windows size
                   type = "text/css",
                   "#ui-colored_svg img {max-width: 100%; width: auto; max-height: 200%; height: auto}" 
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
        ),
        
        column(width = 5,align = "center",
               
               # Output Barplot
               imageOutput(outputId = NS(id,"barplot"), inline = TRUE),
               
               fluidRow(
                   # Select color
                   selectInput(inputId = NS(id, "color_barplot"),
                               label = "Change color",
                               choices = c("salmon","steelblue","olivedrab"),
                               multiple = FALSE),
                   # Download button
                   downloadButton(outputId = NS(id,"download_barplot")),
               ),
               
               # TISSUE FiNDER
               fluidRow(
                   column(width = 6,
                          box(title="Genes enriched in a specific tissue",
                              solidHeader = FALSE, collapsible=TRUE, width = 12,
                              # Tissue finder selector
                              uiOutput(outputId = NS(id,"tissue_finder")),
                              # Finder text box
                              column(3,
                                     h6("Number of genes enriched"),
                                     verbatimTextOutput(outputId = NS(id,"number_genes_in_tissue"),placeholder = TRUE)
                              ),
                              column(9,
                                     verbatimTextOutput(outputId = NS(id,"genes_in_tissue"),placeholder = TRUE)
                              ), 
                              actionButton(inputId =  NS(id,"func_char_tiss"), label = "Functional characterization", align = "center"),
                              actionButton(inputId =  NS(id,"atlas"), label = "Single cell resolution", align = "center"))
                          
                          ),
                   
                   column(width = 6,
                          
                          # Not enriched in any tissue box
                          box(title="Genes not enriched in any tissue", 
                              solidHeader=FALSE, collapsible=TRUE, width = 12, 
                              column(3,
                                     h6("Number of genes not enriched"),
                                     verbatimTextOutput(outputId = NS(id,"number_not_enriched"),placeholder = TRUE)
                              ),
                              column(9,
                                     verbatimTextOutput(outputId = NS(id,"not_enriched")),
                              ),
                              actionButton(inputId = NS(id,"func_char_notenr"), label = "Functional characterization", align = "center")), 
                          
                          hr(),
                          
                          # Not found box
                          box(title="Genes not found in the experiment", 
                              solidHeader=FALSE, collapsible=TRUE, width = 12, 
                              column(3,
                                     h6("Number of genes not found"),
                                     verbatimTextOutput(outputId = NS(id,"number_not_found"),placeholder = TRUE)
                              ),
                              column(9,
                                     verbatimTextOutput(outputId = NS(id,"not_found"))
                                     ),
                              actionButton(inputId = NS(id,"func_char_notfound"), label = "Functional characterization", align = "center")
                              )
                   ),
                   hr()
                   )
        )
    )
}
