##====================================##
## MODULE:                            ##
## Single cell atlas visualization UI ##
##====================================##

## DESCRIPTION ##
# User Interface for single cell atlas
# Reactive UI
# single cell tab


single_cellUI <- function(id) {
  
  # Layout
  fluidPage(
    # In  this column, the complete atlas
    fluidRow(
      column(12,
             # Description
             # Here add the user experient ID
             verbatimTextOutput(outputId = NS(id,"description_sc"))
      ),
      
      hr(),
      
      column(4,
             # Atlas
               imageOutput(outputId = NS(id,"umap_atlas")),
             
                htmlOutput(NS(id, "threeD"))
             
      ),
      
      
      br(),
      column(8,
         column(12,
               box(
                 title = NULL, 
                 status = "success", 
                 solidHeader = F,
                 collapsible = F,
                 width = "100%",
                 column(8,
                        plotOutput(outputId = NS(id,"expr_umap"), width = "100%", height = "100%"),
                         
                ),
                 
                column(4,
                       column(12,
                              uiOutput(outputId = NS(id, "geneset_sc")),
                              
                       ),
                       column(12,
                              selectInput(
                                inputId = NS(id,"color_expr"),label = "Select a color",
                                choices = c("darkblue", "darkred", "darkgreen"),
                                selected = "darkblue",
                                multiple = FALSE,
                                width = "100%"),
                       ),
                       hr(),
                         column(12,
                                # Title
                                tags$label(class = "control-label","Non tissue-specific genes"),
                                # Not enriched in any tissue box
                                verbatimTextOutput(outputId = NS(id,"not_enriched"),placeholder = TRUE)
                         ),     
                         column(12,
                                # Title
                                tags$label(class = "control-label","Non found genes"),
                                # Not found genes
                                verbatimTextOutput(outputId = NS(id,"not_found"),placeholder = TRUE)
                         )
              )
            )   
               
              
        ),
        hr(),
        column(12, 
               box(
                 title = NULL, 
                 status = "success", 
                 solidHeader = F,
                 collapsible = F,
                 width = "100%",
             column(6,
                    fluidRow(
                      
                      box(
                          title = "Tissue-specific expression summary",solidHeader=T, collapsible=F, width = 12,
                          style = 'overflow-x: scroll;overflow-y: scroll;', ### add a scroll bar
                          DT::dataTableOutput(NS(id,'expr_table')),
                      )
                    )
             ),
             column(6,
                    plotOutput(outputId = NS(id,"expr_umap_zoom"), width = "100%")
             )
               )
             
      )
            
            
      )
      
    )
  )
}

