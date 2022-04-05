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


functional_characterization<-function(input_genes, annotation_file) {
  
  ### Return a table with the genes annotation ####
  if(length(input_genes) > 0){
    table = subset(annotation_file, annotation_file$locusName %in% input_genes)
    table[is.na(table)] = ""
    return(table)
  }
  else{
    df = data.frame(ncol = ncol(annotation_file))
    colnames(df) = colnames(annotation_file)
    df[1,1] = "This set of genes is empty"
    return(df)
  }
  # stop("Try other set of genes or parameters")

}

