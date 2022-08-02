##====================================##
## MODULE:                            ##
## Single cell atlas server module    ##
##====================================##

## DESCRIPTION ##
# Server module for single cell atlas
# single cell tab

single_cellServer<-function(id,experiment_path,specie,gene_set,tissue){
  moduleServer(id, function(input, output, session){
    
    output$umap_atlas <- renderImage(
      {
        # Read image
        filename <- normalizePath(paste(experiment_path,"UMAP_CellPopulationColor.png",sep = "/"))
        list(src=filename,
             width="100%",
             height="150%")
      }, deleteFile = FALSE
    )
    
  })
}