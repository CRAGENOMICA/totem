##======================##
## MODULE:              ##
## experiment_selector  ##
##======================##

## DESCRIPTION ##
# User Interface for variable selection
# Reactive UI
# New experiment tab
# Module 1

experiment_selectorUI <- function(id) {
  
  # Layout
  fluidRow(
    
    column(width = 4,
           
           ## Select specie
           selectInput(
             inputId = NS(id,"specie"),
             label = "Select specie",
             choices = list.dirs(path = normalizePath("./experiments"), full.names = FALSE, recursive = FALSE),
             selected = "Arabidopsis thaliana",
             multiple = FALSE,
             width = "100%"),
           
           ## Select experiment
           # Here input a list of experiments, which should be the names of the experiments
           uiOutput(outputId = NS(id,"experiment")),
           
           ## User description <- Add a max. of 60 chars.
           textAreaInput(inputId = NS(id,"user_description"),
                         label = "User gene list name",
                         value = "Enter a description for your gene list (optional)",
                         width = "100%"
           ),
           
           ## User gene list
           textAreaInput(inputId = NS(id,"user_genelist"),
                         label = "Gene list",
                         value = "",
                         rows = 20,
                         width = "100%"
           ),
           
           #Clear gene list button
           actionButton(inputId = NS(id,"clear"),
                        label = "Clear"),
           
           # Calculate enrichment button
           ## This one is not gonna be specific of the namespace!!
           actionButton(inputId = "submit",
                        label = "Calculate enrichment")
    ),
    
    column(width = 8,offset = 0,
           
           # Experiment details
           ## Add a label on top of html box
           tags$label(class = "control-label","Selected experiment details"),
           
           ## Text box with experiment description (HTML)
           htmlOutput(outputId = NS(id,"experiment_description")),
           
           hr(),
           
           column(12, align="center",
                  ## Output image
                  imageOutput(outputId = NS(id,"blank_image"))
           )
           
    )
  )
}

experiment_selectorServer <- function(id) {
  
  moduleServer(id, function(input,output,session) {
    
    ## REACTIVE EXPERIMENT SELECTION (Dependent on the selected organism) Observe any change in specie & Modify list of available experiments
    observeEvent(input$specie, {
      
      ## Create a list of available experiment within each organism (remove "_" from the name to be show, it's clearer)
      exps <<- sort(list.dirs(normalizePath(paste("./experiments",input$specie,sep = "/")),full.names = FALSE,recursive = FALSE), decreasing = T)
      exps_list <<- as.list(exps)
      names(exps_list) <<- unlist(lapply(strsplit(exps, split = rep("_", length(exps))), paste, collapse = " "))
      
      output$experiment<-renderUI({
        selectInput(inputId = NS(id,"experiment_id"),
                    label = "Select an experiment",
                    choices = exps_list,
                    multiple = FALSE,
                    width = "100%"
        )
      })
    })
    
    ## Selected experiment path
    observeEvent(input$experiment_id, {
      
      # Get experiment path    
      experiment_path<<-normalizePath(paste("./experiments",input$specie,input$experiment_id,sep = "/"))
      
      ## Text box with description of the experiment
      # output$experiment_description <- renderText({
      #   description <- readLines(normalizePath(paste(experiment_path,"experiment_description.txt",sep = "/")))
      #   HTML(description)
      # })
      getPage<-function() {
        return(includeHTML(normalizePath(paste(experiment_path,"experiment_description.html",sep = "/"))))
      }
      output$experiment_description<-renderUI({getPage()})
      
      ## Blank Image
      
      output$blank_image <- renderImage(
        {
          # Read image
          filename <- normalizePath(paste(experiment_path,"blank_image.png",sep = "/"))
          # Arguments
          list(src=filename,
               height="180%")
        }, deleteFile = FALSE
      )
    })
    
    # CLEAR BUTTON
    observeEvent(input$clear, {
      updateTextInput(session, "user_genelist", value = "")
    })
    
    # REACTIVE GENE EXAMPLE, DEPENDING OF THE ORGANISM
    observeEvent(input$specie, {
      example_genes<-read.delim(normalizePath(paste("./experiments",input$specie,"example_genes.txt",sep = "/")),header = FALSE)
      updateTextAreaInput(session = session,
                          inputId = "user_genelist",
                          label = "Gene list",
                          value = paste(example_genes[,1],collapse = "\n")
      )
    })
    # PARSE USER GENES
    source("functions/parse_input_genes.R")
    observeEvent(input$user_genelist,
                 {
                   user_genelist <<- parse_input_genes(input = input$user_genelist,
                                                       input_specie = input$specie)
                   # CHeck: print(user_genelist)
                   # There is a problem when selecting sorghum genes -> Is in parse_input_function
                 })
    # VALUES TO RETURN:
    list(
      submit=reactive({input$submit}),
      experiment_id=reactive({input$experiment_id}),
      user_description=reactive({input$user_description}),
      experiment_path=reactive({experiment_path}),
      user_genelist=reactive({user_genelist}),
      specie=reactive({input$specie})
    )
    
  })
}

experiment_selectorApp <- function() {
  
  ui <- fluidPage(
    experiment_selectorUI("x")
  )
  
  server<-function(input,output,session) {
    
    experiment_selectorServer("x")  
  }
  
  shinyApp(ui, server)
}
# experiment_selectorApp()
