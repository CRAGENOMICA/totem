## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: tissue_enrichment.R
##
## Purpose of script: Calculate enrichment with a given tissue gene set
##
## Author: Fidel Lozano-Elena
##
## Date Created: 2022-01-17
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


tissue_enrichment<-function(user_genelist, tissue_atlas, geneuniverse) {
  
  # Avoid duplications in user-input genes
  mygenes<-unique(user_genelist)
  
  # Case insensitive -> be careful with IDs for sorghum/tomato (Sobic, Sb, Solyc identifiers) -> case insensitive in geneuniverse and tissue_atlas too
  mygenes<-toupper(mygenes)
  geneuniverse<-toupper(geneuniverse)
  # Create a vector that will hold the pvalues of Fisher test per tissue
  myvector<-c()
  
  # Loop over the tissue list with expression-specific genes (experiment id)
  for (i in 1:length(tissue_atlas)) {
    # Genes detected in the microarray/RNAseq that are not in user-input
    nomygenes<-setdiff(geneuniverse,mygenes)
    # Generate contingency table
    tissue_atlas_genes = toupper(tissue_atlas[[i]])
    mytable<-rbind(c(length(which(mygenes%in%tissue_atlas_genes)),#  genes in mylist & enriched in tissue iN
                     length(which(mygenes%in%tissue_atlas_genes==FALSE))), #  genes in my list & NOT enriched in tissue iN
                   c(length(which(nomygenes%in%tissue_atlas_genes)), #  genes NOT in my list & enriched in tissue iN
                     length(which(nomygenes%in%tissue_atlas_genes==FALSE)))) #  genes NOT in my list & NOT enriched in tissue iN
    # Add names to the contingency table (traceback)
    rownames(mytable)<-c("Mygenes","!Mygenes"); colnames(mytable)<-c("Enriched","!Enriched")
    # Append to myvector the Fishers exact test result (pvalue)
    myvector<-c(myvector,fisher.test(mytable,alternative = "greater")$p.value) # Alternative = greater because the scope is to detect overepresentation of tissue-specific genes in the user gene list
  }
  
  # Add tissue names to myvector
  names(myvector)<-names(tissue_atlas)
  
  # Return the -log transformed pvalue vector
  return(-log(myvector))
}
