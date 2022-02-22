## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: generate_color_scale.R
##
## Purpose of script: Input has to be enrichment values and the output a named (tissues) vector with colors 
##
## Author: Fidel Lozano Elena
##
## Date Created: 2022-01-23
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


generate_color_scale<-function(input,color="tomato",normalized=TRUE,only_significative=FALSE) {
  
  # Normalize enrichment values between 1 and 0
  if (normalized) {
    enrich_values_norm<-input/max(input)
  } else {
    enrich_values_norm<-input
  }
  
  # Generate lineal color scale from white to color and assign 
  colfunc<-colorRampPalette(colors = c("white",color))
  
  color_ramp<-colfunc(1001)
  
  # Take the enrich_values_norm and asign to values of color vectors
  colorvector<-c()
  for (i in enrich_values_norm) {
    colorvector<-c(colorvector,color_ramp[round(x = i,digits = 3)*1000+1])
  }
  
  names(colorvector)<-names(input)
  
  # Return vector with colors
  return(colorvector)
  
}

