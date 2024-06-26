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
  
 # Case insensitive -> be careful with IDs for sorghum/tomato (Sobic, Sb, Solyc identifiers) -> case insensitive in tissue_atlas too
  user_genes<-unique(user_genes)
  mygenes<-toupper(user_genes)
  tissue_atlas_genes<-toupper(unlist(tissue_atlas))

  output<-setdiff(mygenes,tissue_atlas_genes)
    
  if (length(output)>0) {
      return(paste(user_genes[mygenes %in% output],collapse = "\n"))
  } else {
      return("All provided genes are enriched in at least one tissue")
   }
}