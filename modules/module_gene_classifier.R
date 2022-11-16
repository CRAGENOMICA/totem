##======================##
## MODULE:              ##
## Gene classifier      ##
##======================##

## DESCRIPTION ##
# With the user's gene input, it classifies these genes as 
# Tissue-specific, Non tissue-specific or Not found
# according to the selected experiment

gene_classifierServer <- function(id, experiment_path,user_genelist) {
    
    moduleServer(id, function(input,output,session) {
        
        # LOAD AN R DATA
        load(paste(experiment_path,"data.RData",sep = "/"))
        
        ## For loop here to extract number of genes per tissue
        intersection_tissue_labels<-c()
        
        for (i in names(tissue_atlas)) {
            n_genes<-length(intersect(user_genelist,tissue_atlas[[i]]))
            intersection_tissue_labels<-c(intersection_tissue_labels,
                                          paste(i,"  (",n_genes,")",sep = ""))
        }
        
        ## Create a UI with the available tissues names + number of genes
        output$tissue_finder<-renderUI({
            selectInput(inputId = NS(id,"tissue_finder"),
                        label = "Tissue-specific genes",
                        choices = intersection_tissue_labels,
                        multiple = FALSE,
                        width = "100%"
            )
        })
        
        ## Output the genes of the selected tissue
        source("functions/tissue_gene_finder.R")
        observeEvent(input$tissue_finder, {
            ### Here used tissue_gene_finder function
            selected_tissue<<-strsplit(input$tissue_finder,split = "  ")[[1]][1]
            genes_tissue<<-tissue_gene_finder(user_genes = user_genelist,
                                   tissue_atlas = tissue_atlas,
                                   tissue = selected_tissue)
            output$genes_in_tissue<-renderText(
                return(genes_tissue)
            )
        })
        
        # Genes not enriched in any tissue
        source("functions/not_enriched.R")
        output$not_enriched<-renderText({
            not_enriched(user_genes = user_genelist,
                         tissue_atlas = tissue_atlas)
        })
        
        # Genes not found
        source("functions/not_found.R")
        output$not_found<-renderText({
            not_found(user_genes = user_genelist,
                      geneuniverse = geneuniverse)
        })
        
        # return list of tissue specific genes and tissue for functional char and single cell tabs
        list(
          gene_set = reactive({genes_tissue}),
          selected_tissue = reactive({selected_tissue})
        )
        
    })
}
