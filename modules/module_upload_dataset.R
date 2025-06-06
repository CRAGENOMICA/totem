##======================##
## MODULE:              ##
## upload_dataset       ##
##======================##

## DESCRIPTION ##
# User Interface and server for importing custom dataset
# New experiment tab


upload_datasetUI <- function(id) {
  
  # Layout
  fluidRow(
    
    column(width = 4,
           
           ## Select specie
           fileInput(NS(id,"input_file"), 'Choose the dataset file (.xlsx, .csv, or .txt) containing tissue-especific genes',
                     accept = c(".xlsx", ".csv", ".txt")),
           
           fileInput(NS(id,"input_geneuniverse"), 'Choose the gene universe file (.xlsx, .csv, or .txt) of you organism',
                     accept = c(".xlsx", ".csv", ".txt")),
           
           actionButton(NS(id, "show_example"), "Show me an example! ✓"),
           
           hr(),
           
           ## User description <- Add a max. of 60 chars.
           textAreaInput(inputId = NS(id,"user_description"),
                         label = "User gene list name",
                         value = "Enter a description for your gene list (optional)",
                         width = "100%"
           ),
           
           ## User gene list
           textAreaInput(inputId = NS(id,"user_genelist"),
                         label = "Gene list (*)",
                         value = "",
                         rows = 20,
                         width = "100%"
           ),
           
           h4("(*) List separated by enter, tab, comma or semicolon format are admitted"),
           
           #Clear gene list button
           actionButton(inputId = NS(id,"clear"),
                        label = "Clear"),
           
           # Calculate enrichment button
           ## This one is not gonna be specific of the namespace!!
           actionButton(inputId = "submit",
                        label = "Calculate enrichment")
    ),
    
    column(8,
           
           # Experiment details
               
           box(
             title = "Tissue-specific genes dataset",solidHeader=T, collapsible=F, width = 12,
             style = 'height: 80vh;overflow-x: scroll;overflow-y: scroll;', ### add a scroll bar
             DT::dataTableOutput(NS(id,'file'))
           ),
           box(
             title = "Gene universe",solidHeader=T, collapsible=F, width = 12,
             style = 'height: 20vh; overflow-x: scroll;overflow-y: scroll;', ### add a scroll bar
             textOutput(NS(id,'universefile'))
           )
    )
  )
}

upload_datasetServer <- function(id) {
  
  moduleServer(id, function(input,output,session) {
    
    ## Show example
    example_dataset <- data.frame(
      Companion_Cells = c("AT1G07640","AT1G10380","AT1G12010","AT1G12140","AT1G22710","AT1G23530"),
      Protophloem = c("AT1G06490","AT1G08160","AT1G11915","AT1G14730","AT1G21140","AT1G29520"),
      Metaphloem = c("AT1G06490","AT1G08160","AT1G11570","AT1G11915","AT1G14730","AT1G16022"),
      Metaxylem= c("AT1G01780","AT1G01900","AT1G02640","AT1G02720","AT1G03820","AT1G07380"),
      Protoxylem = c("AT1G01240","AT1G02335","AT1G02640","AT1G02860","AT1G06090","AT1G08340"))
    example_geneuniverse <- unique(unname(unlist(example_dataset)))
    example_usergenes <- c("AT1G12010","AT1G12140","AT1G22710")
    
    output$example_dataset_tbl <- DT::renderDataTable(example_dataset)
    output$example_geneuniverse_txt <- renderText(
      paste(example_geneuniverse, collapse = "\n")
    )
    output$example_usergenes_commas<- renderText(
      paste(example_usergenes, collapse = ", ")
    )
    
    observeEvent(input$show_example, {
      showModal(
        modalDialog(
          title = "File examples",
          h4("Dataset"),
          DT::dataTableOutput(NS(id, "example_dataset_tbl")),
          br(),
          h4("Gene universe"),
          verbatimTextOutput(NS(id, "example_geneuniverse_txt")),
          br(),
          h4("User gene list"),
          verbatimTextOutput(NS(id, "example_usergenes_commas")),
          easyClose = TRUE,
          size = "l"
        )
      )
    })
    

    # CLEAR BUTTON
    observeEvent(input$clear, {
      updateTextInput(session, "user_genelist", value = "")
    })
    
    # PARSE USER GENES
    source("functions/parse_input_genes.R")
    observeEvent(input$user_genelist,
                 {
                   user_genelist <<- parse_input_genes(input = input$user_genelist,
                                                       input_specie = " ")
                 })
    
    
    # READ THE DATASET
    df_dataset <- reactive({
      req(input$input_file)
      
      ext <- tolower(tools::file_ext(input$input_file$name))
      switch(ext,
             csv  = read.csv(input$input_file$datapath),
             txt  = read.table(input$input_file$datapath, sep = "\t", header = TRUE),
             xlsx = readxl::read_xlsx(input$input_file$datapath),
             validate("Invalid file; please upload .csv, .txt or .xlsx")
      )
    })
    
    # READ THE GENE UNIVERSE
    geneuniverse <- reactive({
      req(input$input_geneuniverse)
      
      ext <- tolower(tools::file_ext(input$input_geneuniverse$name))
      vec <- switch(ext,
                    csv  = read.csv(input$input_geneuniverse$datapath)[[1]],
                    txt  = read.table(input$input_geneuniverse$datapath,
                                      sep = "\t", header = TRUE)[[1]],
                    xlsx = readxl::read_xlsx(input$input_geneuniverse$datapath)[[1]],
                    validate("Invalid file; please upload .csv, .txt or .xlsx")
      )
      as.vector(vec)
    })
    
    output$file <- DT::renderDataTable(df_dataset(), options = list(lengthMenu = c(10,20,50), pageLength = 15))
    output$universefile <- renderText(paste(geneuniverse(), collapse = ", "))
    
    
    experiment_data <- reactive({
      list(
        df_dataset   = df_dataset(),   # dataframe completo
        geneuniverse = geneuniverse()  # vector de genes
      )
    })
    
    

    # VALUES TO RETURN:
    list(
      experiment_id="Custom",
      experiment_path=experiment_data,
      submit=reactive({input$submit}),
      user_description=reactive({input$user_description}),
      user_genelist=reactive({user_genelist}),
      specie="None"
    )
    
    
  })
}
# 
# upload_datasetApp <- function() {
# 
#   ui <- fluidPage(
#     upload_datasetUI("x")
#   )
# 
#   server<-function(input,output,session) {
# 
#     upload_datasetServer("x")
#   }
# 
#   shinyApp(ui, server)
# }
# upload_datasetApp()


