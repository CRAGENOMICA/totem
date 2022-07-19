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
        
        # Read myImage's width and height. These are reactive values, so this
        # expression will re-run whenever they change.
        # width  <- session$clientData$output_umap_atlas_width
        # height <- session$clientData$output_umap_atlas_height
        
        list(src=filename,
             width="100%",
             height="200%")
      }, deleteFile = FALSE
    )
    
  })
}