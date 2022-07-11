##======================##
## MODULE:              ##
## newsearch_sel1_ui    ##
##======================##

## DESCRIPTION ##
# User Interface for variable selection
# New experiment tab
# Module 1

newsearch_sel1_ui <- function(id) {
    ns<-NS(id)
    
    # Assemble UI elements
    tagList(
        ## Select specie
        selectInput(
            inputId = ns("specie"),
            label = "Select specie",
            choices = list.dirs(path = normalizePath("./experiments"), full.names = FALSE, recursive = FALSE),
            selected = "Arabidopsis",
            multiple = FALSE,
            width = "100%"),
        
        
        ## Select experiment
        # Here input a list of experiments, which should be the names of the experiments
        uiOutput(outputId = ns("experiment")),
        
        ## User description
        textAreaInput(inputId = ns("user_description"),
                      label = "User gene list name",
                      value = "Enter a description for your gene list (optional)",
                      width = "100%"
        ),
        
        ## User gene list
        textAreaInput(inputId = ns("user_genelist"),
                      label = "Gene list",
                      value = "Paste you gene list here e.g:\nAT1G012032 for Arabidopsis, Sobic.001G000700 or Sb01g000230 for Sorghum and Solyc00g011670 for Tomato",
                      rows = 12,
                      width = "100%"
        ),
        
        #Clear gene list button
        actionButton(inputId = ns("clear"),
                     label = "Clear"),
        
        # Calculate enrichment button
        actionButton(inputId = ns("submit"),
                     label = "Calculate enrichment"),
    )
    
}