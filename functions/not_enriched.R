## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: not_enriched.R
##
## Purpose of script: Compare user gene list and get genes that are not enriched in any particular tissue
##
## Author: Fidel Lozano-Elena
##
## Date Created: 2022-01-27
##
## Copyright (c) Fidel Lozano, 2022
## Email: fidel.lozano@cragenomica.es
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Notes:
##   
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%

not_enriched<-function(user_genes,tissue_atlas) {
    
    output<-setdiff(user_genes,unlist(tissue_atlas))
    
    if (length(output)>0) {
        return(paste(output,collapse = "\n"))
    } else {
        return("All provided genes are enriched in at least one tissue")
    }
}