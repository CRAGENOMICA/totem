##======================##
## MODULE:              ##
## experiment_selector  ##
##======================##

## DESCRIPTION ##
# User Interface for variable selection
# Reactive UI
# New experiment tab
# Module 1

dataset_selectorUI <- function(id) {
  
  # Layout
  fluidRow(
    
    hr(),
    
    column(width = 12, align="center",
           h1("Please, select how to choose datasets")
    ),
    
    hr(),
    
    column(width = 6, align="center",
           h2("You want to upload your predefined dataset for enrichment analysis:"),
           ## Select type of comparison
           uiOutput(outputId = NS(id, "comparison_upload"))
    ),
    column(width = 6, align="center",
           h2("You want to select our datasets for enrichment analysis:"),
           ## Select type of comparison
           uiOutput(outputId = NS(id, "comparison_own"))
    )
  )
}

dataset_selectorServer <- function(id, input_data) {
  
  moduleServer(id, function(input,output,session) {
    
    # Display buttons to select experiment or upload your own data ####
    ## buttons
    output$comparison_upload <- renderUI({
      tags$button(
        id = NS(id,"uploaddata"),
        class = "btn action-button",
        align = "center",
        tags$img(src = "upload.png",width = "50%", align = "center")
      )
    })
    output$comparison_own <- renderUI({
      tags$button(
        id = NS(id,"owndata"),
        class = "btn action-button",
        align = "center",
        tags$img(src = "available.png",width = "50%", align = "center")
      )
    })
    
    ## Check the comparison selected
    observeEvent(input$owndata, {
      if (input$owndata > 0 ) {
        input_data$submit_own <- input$owndata #increment button
      }
    })
    observeEvent(input$uploaddata, {
      if (input$uploaddata > 0 ) {
        input_data$submit_upload <- input$uploaddata #increment button
      }
    })
    
    # VALUES TO RETURN:
    list(
      submit_own = reactive({
        input$submit_own
      }),
      submit_upload = reactive({
        input$submit_upload
      })
    )
    
    
  })
}

# dataset_selectorApp <- function() {
#   
#   ui <- fluidPage(
#     dataset_selectorUI("x")
#   )
#   
#   server<-function(input,output,session) {
#     
#     dataset_selectorServer("x", in)  
#   }
#   
#   shinyApp(ui, server)
# }
# dataset_selectorApp()
