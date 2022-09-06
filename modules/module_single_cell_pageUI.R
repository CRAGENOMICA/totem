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
           # Description
           # Here add the user experient ID
           verbatimTextOutput(outputId = NS(id,"description_sc")),
           hr(),
           # Atlas
           imageOutput(outputId = NS(id,"umap_atlas"))
    ),
    
    
    # Main panel for displaying gene sets
    mainPanel( width = 8,
               
               tabsetPanel(type = "tabs",
                           tabPanel("Tissue-scpecific genes", 
                                    br(),
                                    column(6,
                                           
                                           column(8,
                                                  uiOutput(outputId = NS(id, "geneset_sc")),
                                                  
                                           ),
                                           column(4,
                                                  selectInput(
                                                    inputId = NS(id,"color_expr"),label = "Select a color",
                                                    choices = c("darkblue", "darkred", "darkgreen"),
                                                    selected = "darkblue",
                                                    multiple = FALSE,
                                                    width = "100%"),
                                           ),
                                         
                                           column(12,
                                                  plotOutput(outputId = NS(id,"expr_umap"), width = "100%")
                                           )
                                    ),
                                    
                                    column(6,
                                           box(title = "Tissue-specific expression summary",solidHeader=FALSE, collapsible=FALSE, width = 12,
                                               style = 'overflow-x: scroll;', ### add a scroll bar
                                               # downloadButton(outputId = "download_cluster_info", label = "Download expression summary"),
                                               DT::dataTableOutput(NS(id,'expr_table')),
                                           ),
                                           br(),
                                           plotOutput(outputId = NS(id,"expr_umap_zoom"), width = "100%")
                                           
                                    )
                           ),
                           tabPanel("Non tissue-specific genes", 
                                    br(),
                                    column(12,
                                      # Title
                                      tags$label(class = "control-label","Non tissue-specific genes"),
                                      # Not enriched in any tissue box
                                      verbatimTextOutput(outputId = NS(id,"not_enriched"),placeholder = TRUE)
                                    )
                           ),
                           tabPanel("Non found genes", 
                                    br(),
                                    column(12,
                                      # Title
                                    tags$label(class = "control-label","Non found genes"),
                                    # Not found genes
                                    verbatimTextOutput(outputId = NS(id,"not_found"),placeholder = TRUE)
                                    )
                           )
                           
               )
    )
  )
}

