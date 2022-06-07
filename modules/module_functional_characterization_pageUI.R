##================================##
## MODULE:                        ##
## Functional characterization UI ##
##================================##

## DESCRIPTION ##
# User Interface for variable selection
# Reactive UI
# Funtional characterization tab
# Module 3


functional_characterizationUI <- function(id) {
  
  # Layout
  fluidPage(
    verbatimTextOutput(outputId = NS(id,"geneset")),
    
    br(),
    
    column(12,
           # Annotation table
           box(title = "Annotation of selected genes",solidHeader=FALSE, collapsible=TRUE, width = 12, 
               style = 'overflow-x: scroll;', ### add a scroll bar
               DT::dataTableOutput(NS(id,'ann_table')),
               downloadButton(outputId = NS(id,"download_annotation"), label = "Download table")
           )
    ),
    
    hr(),
    
    column(12,
           box(title = "Gene Ontology analysis of selected genes",solidHeader=FALSE, collapsible=TRUE,width = 12,
               style = "width: 100%;height: 60em;",
               sidebarLayout(
                 # side panel for GO parameters reactive inputs
                 sidebarPanel(width = 3,
                              
                              selectInput(inputId = NS(id,"select_ontology"),
                                          label = "Select the ontology",
                                          choices = c("Biological process" = "GO:BP",
                                                      "Molecular function" = "GO:MF",
                                                      "Celular component" = "GO:CC"),
                                          selected = "GO:BP", multiple = FALSE),
                              br(),# br() element to introduce extra vertical spacing
                              selectInput(inputId = NS(id,"select_padjmethod"),
                                          label = "Select the method for p-value adjustment",
                                          choices = c("Bonferroni" = "bonferroni", "Benjamini & Hochberg (FDR)" = "fdr", 
                                                      "gProfiler algorithm (g:SCS)" = "gSCS"),
                                          selected = "fdr", multiple = FALSE),
                              br(),
                              numericInput(NS(id,"select_pvalcutoff"), "p-value cut-off", 0.05, step = 0.01),
                              br(),
                              
                              sliderInput(NS(id,"select_nCategory"), "Maximum number of GO categories to show", value = 20, min = 1, max = 50)
                              
                 ),
                 
                 # Main panel for displaying GO plot outputs
                 mainPanel( width = 9,
                            
                            tabsetPanel(type = "tabs",
                                        tabPanel("Dotplot: GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_dotGO"), label = "Download GO term plot"),
                                                 plotOutput(outputId = NS(id,"dotGO"), width = "100%")),
                                        tabPanel("Net: GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_net"), label = "Download GO term net"),
                                                 plotOutput(outputId = NS(id,"net"), width = "100%")),
                                        tabPanel("Heatmap: Genes in GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_heatGO"), label = "Download GO term heatmap"),
                                                 plotOutput(outputId = NS(id,"heatGO"), width = "100%")),
                                        tabPanel("Dotplot: KEGG pathways", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_dotKEGG"), label = "Download KEGG pathway plot"),
                                                 plotOutput(outputId = NS(id,"dotKEGG"), width = "100%")),
                                        tabPanel("Heatmap: Genes in KEGG pathways", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_heatmap"), label = "Download KEGG pathway heatmap"),
                                                 plotOutput(outputId = NS(id,"heatKEGG"), width = "100%"))
                                        
                            )
                 )
               )
           )
    )
  )
}
