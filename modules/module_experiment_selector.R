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
            selected = "Arabidopsis",
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
                      rows = 12,
                      width = "100%"
        ),
        
        #Clear gene list button
        actionButton(inputId = NS(id,"clear"),
                     label = "Clear"),
        
        # Calculate enrichment button
        actionButton(inputId = NS(id,"submit"),
                     label = "Calculate enrichment")
    ),
    
    column(width = 8,offset = 0,
           
           # Experiment details
           ## Add a label on top of html box
           tags$label(class = "control-label","Selected experiment details"),
           
           wellPanel(
               ## Text box with experiment description (HTML)
               htmlOutput(outputId = NS(id,"experiment_description"))
               ),
        
        ## A image
        tags$head(tags$style(### adjust image to the windows size
            type = "text/css",
            "#x-blank_image img {max-width: 100%; width: auto; max-height: 200%; height: auto; text-align: center}"
        )),
        
        ## Output image
        imageOutput(outputId = NS(id,"blank_image"))
    )
    )
}

experiment_selectorServer <- function(id) {
    
    moduleServer(id, function(input,output,session) {
        
        # REACTIVE EXPERIMENT SELECTION (Dependent on the selected organism)
        
        ## Create a list of available experiment within each organism
        output$experiment<-renderUI({
            selectInput(inputId = NS(id,"experiment_id"),
                        label = "Select an experiment",
                        choices = list.dirs(normalizePath(paste("./experiments",input$specie,sep = "/")),full.names = FALSE,recursive = FALSE),
                        multiple = FALSE,
                        width = "100%"
            )
        })
        
        ## Observe any change in specie & Modify list of available experiments
        observeEvent(input$specie, {
            updateSelectInput(session = session,
                              inputId = NS(id,"experiment_id"),
                              label = "Select an experiment",
                              choices = list.dirs(normalizePath(paste("./experiments",input$specie,sep = "/")),full.names = FALSE,recursive = FALSE)
            )
            # Get the specie
            selected_specie<<-as.character(input$specie)
        })
        
        ## Selected experiment path
        observeEvent(input$experiment_id, {
            
            # Get experiment path    
            experiment_path<<-normalizePath(paste("./experiments",input$specie,input$experiment_id,sep = "/"))
            
            ## Text box with description of the experiment
            
            output$experiment_description <- renderUI({
                description <- readLines(normalizePath(paste(experiment_path,"experiment_description.txt",sep = "/")))
                HTML(description)
            })
            
            ## Blank Image
            
            output$blank_image <- renderImage(
                {
                    # Read image
                    filename <- normalizePath(paste(experiment_path,"blank_image.png",sep = "/"))
                    
                    # Read myImage's width and height. These are reactive values, so this
                    # expression will re-run whenever they change.
                    width  <- session$clientData$output_blank_image_width
                    height <- session$clientData$output_blank_image_height
                    
                    list(src=filename,
                         width=width,
                         height=height)
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
                         annotation_file<-read.delim(normalizePath(paste("./experiments",input$specie,"annotation_file.txt",sep = "/")),header = FALSE)
                         user_genelist <<- parse_input_genes(input = input$user_genelist,
                                                            input_specie = input$specie,
                                                            annotation_file = annotation_file)
                         # CHeck: print(user_genelist)
                         # There is a problem when selecting sorghum genes -> Is in parse_input_function
                     })
        
        # VALUES TO RETURN:
        list(
            submit=reactive({input$submit}),
            experiment_id=reactive({input$experiment_id}),
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
