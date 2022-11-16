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
    verbatimTextOutput(outputId = NS(id,"description_fc")),
    
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
               style = "width: 100%;height: 55em;",
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
                                          choices = c("Benjamini & Hochberg (FDR)" = "fdr", "Bonferroni" = "bonferroni"),
                                          selected = "fdr", multiple = FALSE),
                              br(),
                              # numericInput(NS(id,"select_pvalcutoff"), "p-value cut-off", 0.05, step = 0.01),
                              selectInput(inputId = NS(id,"select_pvalcutoff"),
                                          label = "Select p-value cut-off",
                                          choices = c("0.01" = 0.01, "0.05" = 0.05, "0.1" = 0.1, "1" = 1),
                                          selected = "0.05", multiple = FALSE)
                              
                 ),
                 
                 # Main panel for displaying GO plot outputs
                 mainPanel( width = 9,
                            
                            tabsetPanel(type = "tabs",
                                        tabPanel("Dotplot: GO terms", 
                                                 br(),
                                                 column(8,
                                                        plotOutput(outputId = NS(id,"dotGO"), width = "100%")),
                                                 column(4,
                                                        downloadButton(outputId = NS(id,"download_dotGO"), label = "Download GO term plot"))),
                                      
                                        
                                        tabPanel("Net: GO terms", 
                                                 br(),
                                                 column(8,
                                                        plotOutput(outputId = NS(id,"netGO"), width = "100%")),
                                                 column(4,
                                                        downloadButton(outputId = NS(id,"download_netGO"), label = "Download GO term net"))),
                                        
                                        tabPanel("Genes related to GO terms", 
                                                 br(),
                                                 column(8,
                                                        plotOutput(outputId = NS(id,"heatGO"), width = "100%")),
                                                 column(4,
                                                        downloadButton(outputId = NS(id,"download_heatGO"), label = "Download GO term genes"))),
                                        
                                        tabPanel("Dotplot: KEGG pathways", 
                                                 br(),
                                                 column(8,
                                                        plotOutput(outputId = NS(id,"dotKEGG"), width = "100%")),
                                                 column(4,
                                                        downloadButton(outputId = NS(id,"download_dotKEGG"), label = "Download KEGG pathway plot"))),
                                        
                                        tabPanel("Genes related to KEGG pathways", 
                                                 br(),
                                                 column(8,
                                                        plotOutput(outputId = NS(id,"heatKEGG"), width = "100%")),
                                                 column(4,
                                                        downloadButton(outputId = NS(id,"download_heatKEGG"), label = "Download KEGG pathway genes")))
                            )
                 )
               )
           )
    )
  )
}

