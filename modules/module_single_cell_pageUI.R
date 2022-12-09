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
             fluidRow(
               column(12,
                      # 3D
                      htmlOutput(outputId = NS(id,"threeD"))
               ),
               hr(),
               column(12,
                      # Atlas
                      imageOutput(outputId = NS(id,"umap_atlas"))
                      
               )
             )
             
      ),
      
      
      br(),
      column(8,
             
             column(12,
                    column(6,
                           uiOutput(outputId = NS(id, "geneset_sc")),
                           
                    ),
                    column(6,
                           selectInput(
                             inputId = NS(id,"color_expr"),label = "Select a color",
                             choices = c("darkblue", "darkred", "darkgreen", "darkmagenta"),
                             selected = "darkblue",
                             multiple = FALSE,
                             width = "100%"),
                    ),
                    hr()
             ),
             
         column(12,
               box(
                 title = NULL, 
                 status = "success", 
                 solidHeader = F,
                 collapsible = F,
                 width = "100%",
                 height = 900,
                column(10,
                        verbatimTextOutput(outputId = NS(id,"enrich_description"),placeholder = TRUE)
                ),
                column(2,align="right",
                       downloadButton(outputId = NS(id,"download_scExpression"))
                ),
                column(7, 
                       plotOutput(outputId = NS(id,"expr_umap"), width = "100%", height = "100%")
                ),
                column(5,
                        plotOutput(outputId = NS(id,"expr_violin"), width = "100%", height = "100%")
                         
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
                 height = 900,
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

