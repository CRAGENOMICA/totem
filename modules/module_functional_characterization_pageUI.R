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
                                                 downloadButton(outputId = NS(id,"download_dotGO"), label = "Download GO term plot"),
                                                 plotOutput(outputId = NS(id,"dotGO"), width = "100%")),
                                        tabPanel("Net: GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_netGO"), label = "Download GO term net"),
                                                 plotOutput(outputId = NS(id,"netGO"), width = "100%")),
                                        tabPanel("Genes related to GO terms", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_heatGO"), label = "Download GO term genes"),
                                                 plotOutput(outputId = NS(id,"heatGO"), width = "100%")),
                                        tabPanel("Dotplot: KEGG pathways", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_dotKEGG"), label = "Download KEGG pathway plot"),
                                                 plotOutput(outputId = NS(id,"dotKEGG"), width = "100%")),
                                        tabPanel("Genes related to KEGG pathways", 
                                                 br(),
                                                 downloadButton(outputId = NS(id,"download_heatKEGG"), label = "Download KEGG pathway genes"),
                                                 plotOutput(outputId = NS(id,"heatKEGG"), width = "100%"))
                                        
                            )
                 )
               )
           )
    )
  )
}


x<-"C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigenomica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM/experiments/Arabidopsis/Root_SingleCell"
y<-c("AT2G41650\nAT4G39400\nAT1G04560\nAT1G65484\nAT2G21400\nAT5G59310\nAT5G02020")
z = "Arabidopsis"
tissue = "CC_B"

# load(paste(x,"data.RData",sep = "/"))

functional_characterizationApp <- function(id) {
  
  ui <- fluidPage(
    functional_characterizationUI("fc")
  )
  
  server<-function(input,output,session) {
    
    source("modules/module_functional_characterization.R")
    
    functional_characterizationServer("fc",
                                      experiment_path = x,
                                      specie = z,
                                      gene_set = y,
                                      tissue = tissue)
  }
  
  shinyApp(ui, server)
}
# functional_characterizationApp()
