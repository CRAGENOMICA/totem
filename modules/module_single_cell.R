##====================================##
## MODULE:                            ##
## Single cell atlas server module    ##
##====================================##

## DESCRIPTION ##
# Server module for single cell atlas
# single cell tab


sc_buttonServer <- function(id, parent, sc_button) { #sc_button is an argument
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 
                 observeEvent(input$single_cell_atlas,{
                   sc_button$single_cell_atlas <- input$single_cell_atlas #increment sc_button
                 })
               })
}

single_cellServer<-function(id,experiment_path,specie,gene_set,tissue){
  moduleServer(id, function(input, output, session){
    
  })
}