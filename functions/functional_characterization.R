## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: functional_characterization.R
##
## Purpose of script: Functional characterization of a gene list
##
## Author:Veredas Coleto-Alcudia
##
## Date Created: 2022-03-29
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


functional_characterization<-function(input_genes, annotation_file, specie) {
  
  ### Return a table with the genes annotation
  if(length(input_genes) > 0){
    if(specie == "Sorghum"){
      table = subset(annotation_file, annotation_file$locusName_Version3 %in% input_genes)
    }
    else{
      table = subset(annotation_file, annotation_file$locusName %in% input_genes)
    }
    return(table)
  }
  
}

