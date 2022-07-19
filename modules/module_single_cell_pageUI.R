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
    column(4,
           # Add the atlas image
           tags$head(tags$style(### adjust image to the windows size
             type = "text/css",
             "#umap_atlas img {max-width: 100%; width: auto; max-height: 200%; height: auto}"
           )),
           imageOutput(outputId = NS(id,"umap_atlas"))
    ),
    
    column(4,
           verbatimTextOutput(outputId = NS(id,"geneset_sc")),
           br(),
           column(6,
                  selectizeInput(
                    inputId = NS(id,"gene_expr"),label = "Select a gene",
                    choices = NULL,
                    multiple = FALSE,
                    width = "100%"),
                  
           ),
           column(6,
                  selectInput(
                    inputId = NS(id,"color_expr"),label = "Select a color",
                    choices = c("darkblue", "darkred", "darkgreen"),
                    selected = "darkblue",
                    multiple = FALSE,
                    width = "100%"),
           ),
           column(12,
                  # downloadButton(outputId = "download_expr", label = "Download expression plot"),
                  plotOutput(outputId = NS(id,"expr_umap"), width = "130%")
           )
    ),
    
    column(4,
           box(title = "Tissue-specific expression",solidHeader=FALSE, collapsible=FALSE, width = 12,
               style = 'overflow-x: scroll;', ### add a scroll bar
               # downloadButton(outputId = "download_cluster_info", label = "Download expression summary"),
               DT::dataTableOutput(NS(id,'expr_table')),
           ),
           br(),
           plotOutput(outputId = NS(id,"expr_umap_zoom"), width = "130%")
           
    )
    
  )
}