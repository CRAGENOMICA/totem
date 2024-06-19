## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: tissue_gene_finder.R
##
## Purpose of script: Intersect and return user genes that are tissue-specific for a particular tissue
##
## Author: Fidel Lozano-Elena
##
## Date Created: 2022-01-14
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

tissue_gene_finder<-function(user_genes,tissue,tissue_atlas) {
    
    output<-user_genes[which(user_genes%in%unlist(tissue_atlas[match(tissue, names(tissue_atlas))]))]
    
    if (length(output)>0) {
        return(paste(output,collapse = "\n"))
    } else {
        return("none")
    }
}

