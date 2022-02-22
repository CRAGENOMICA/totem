## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: parse_input_genes.R
##
## Purpose of script: Read input in text box and return a character vector
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


## Parse input

parse_input_genes<-function(input,external=FALSE) {
  
  # Open an external file instead of an object?
  if (external) {
    user_genes<-readChar(input, nchars = 25000)
  } else {
      user_genes<-input
    }
  
  # Check any possible sep character and keep the one providing the longest vector
  sep_vector<-c("\t","\r","\n","\r\n",",",";")
  sep_count<-c()
  for ( i in sep_vector) {
    
    list_length<-length(strsplit(x = user_genes,split = i)[[1]])
    #print(paste("The character",i,"yields",list_length,"genes",sep=" "))
    sep_count<-c(sep_count,list_length)
    
  }
  
  best_separator<-sep_vector[which.max(sep_count)]
  
  # Split the input
  genes_vector<-strsplit(x = user_genes,split = best_separator)[[1]]
  
  # Return a character vector
  return(as.character(genes_vector))
  
}

