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
        
        column(width = 6, align = "left",
               
               # Here add the user experient ID
               verbatimTextOutput(outputId = NS(id,"description_exp")),
               hr(),
               
               # Output colored SVG
               imageOutput(outputId = NS(id,"colored_svg"),width = "200%",height = "200%"),
               # Output legend of coloured SVG
               column(12, align = "center",
                      imageOutput(outputId = NS(id,"legend"),width = "100%",height = "10%")
                      ),
               
               fluidRow(
                   
                   column(width = 8,
                          
                          # Select color
                          selectInput(inputId = NS(id,"color_svg"),
                                      label = "Change color",
                                      choices = c("salmon","steelblue","olivedrab", "viridis", "magma"),
                                      multiple = FALSE)
                   ),
                   
                   column(width = 4, align="left", style = "margin-top: 25px;",
                          
                          # Download button
                          downloadButton(outputId = NS(id,"download_colored_svg"),)
                          
                          )
               )  
        ),
        
        column(width = 6,align = "left",
               
               # Output Barplot
               imageOutput(outputId = NS(id,"barplot"), height = "150%",width = "130%"),
               
               fluidRow(
                   
                   column(width = 8,
                   
                          # Select color
                          selectInput(inputId = NS(id, "color_barplot"),
                                      label = "Change color",
                                      choices = c("salmon","steelblue","olivedrab","gold1","darkblue","darkmagenta"),
                                      multiple = FALSE)
                          ),
                   
                   column(width = 4,align="left", style = "margin-top: 25px;",
                          
                          # Download button
                          downloadButton(outputId = NS(id,"download_barplot"))
                   )
               ),
               
               hr(),
               
               # TISSUE CLASSIFIER
               fluidRow(
                   
                   # Tissue finder selector
                 tags$label(class = "control-label","Tissue-specific genes"),
                   uiOutput(outputId = NS(id,"tissue_finder")),
                   hr(),
                   verbatimTextOutput(outputId = NS(id,"genes_in_tissue"),placeholder = TRUE),
                   column(6,
                          actionButton(inputId = NS(id,"func_char_tiss"), label = "Functional characterization", align = "center")
                          ),
                   column(6,
                         )
               ),
               
               hr(),
               
               fluidRow(
                   
                   # Title
                   tags$label(class = "control-label","Non tissue-specific genes"),
                   # Not enriched in any tissue box
                   verbatimTextOutput(outputId = NS(id,"not_enriched"),placeholder = TRUE)
                   ),
               
               fluidRow(
                   
                   # Title
                   tags$label(class = "control-label","Non found genes"),
                   
                   # Not found genes
                   verbatimTextOutput(outputId = NS(id,"not_found"),placeholder = TRUE)
                   ),
               
               hr()
        )
    )
}


# ## App function
# # setwd("C:/Users/vcoleto/OneDrive - CRAG - Centre de Recerca en Agrigen?mica - CSIC IRTA UAB UB/ACano-LAB/SingleCell/TOTEM/shinyTOTEM_actual/shinyTOTEM")
# # Testing purposes:
# if (exists("testing")) {
    # x<-"/home/flozano/OneDrive/PROJECTS/4_TOTEM/shiny_totem/shinyTOTEM/experiments/Arabidopsis/Root_longitudinal_patterns_Brady2007"
    # x<-"./experiments/Arabidopsis thaliana/Root_longitudinal_patterns"
    # sp = "Arabidopsis thaliana"
    # g<-c("AT2G01430","AT2G41650","AT3G13380","AT4G39400","AT2G27550","AT5G59220","AT5G62420","AT3G20810","AT5G25610","AT1G11600")
    # y<-"Root_longitudinal_patterns"
    # desc<-"Enter a description for your gene list (optional)"
    # z<-c(0.0000000,0.0000000,0.7066333,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.7572440,0.0000000)
    # names(z)<-c("Columella","Section_1","Section_2","Section_3","Section_4","Section_5","Section_6","Section_7","Section_8","Section_9","Section_10","Section_11","Section_12")
# }
# 
# 
# enrichment_resultsApp <- function(id) {
# 
#     ui <- fluidPage(
# 
#         enrichment_pageUI(id = "x")
#     )
# 
#     server<-function(input,output,session) {
# 
#         source("modules/module_enrichment_results.R")
#         enrichment_resultsServer("x",
#                                  experiment_path = x,
#                                  user_genelist = g,
#                                  user_description = desc,
#                                  experiment_id = y,
#                                  specie = sp)
#         source("modules/module_colorSVG.R")
#         colorSVG_Server(id = "x",experiment_path = x,experiment_id = y, specie = sp,enrichment_values = z, user_description = desc)
# 
#         source("modules/module_gene_classifier.R")
#         gene_classifierServer(id = "x",experiment_path = x,user_genelist = g)
#     }
# 
#     shinyApp(ui, server)
# }
# enrichment_resultsApp()
