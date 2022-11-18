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


generate_color_scale<-function(input,color="tomato",normalized=FALSE,only_significative=FALSE) {
  
  # Normalize enrichment values between 1 and 0
  if (normalized) {
    enrich_values_norm<-input/max(input)
    # For normalization, the number of colors to generate is 1001
    maxval = 1001
  } else {
    enrich_values_norm<-input
    # For no normalization, the number of colors to generate is 1000*(max enrich value+1 -> to avoid round when <0.5)+1
    maxval = 1000*round(max(enrich_values_norm)+1, digits = 0)+1
  }
  if(color=="viridis"){
    # Generate lineal color scale with viridis palette colors
    color_ramp<-c("white", viridis(maxval-1)[(maxval-1):1])
  }
  else if(color == "magma"){
    # Generate lineal color scale with magma palette colors
    color_ramp<-c("white", magma(maxval-1)[(maxval-1):1])
  }
  else{
    # Generate lineal color scale from white to color and assign 
    colfunc<-colorRampPalette(colors = c("white",color))
    color_ramp<-colfunc(maxval)
  }
  ## Generate legend 
  forlegend = t(data.frame(vals = seq(min(enrich_values_norm), max(enrich_values_norm), by = (max(enrich_values_norm)/(maxval-1)))))
  png("./legend_svg.png", height = 8,width = 18,units = "cm",res=400)
  image(y= maxval, x = forlegend[1,], t(forlegend), col = color_ramp, axes = F, ylab = "", xlab = "")
  title("-log(p-value enrichment) color scale")
  axis(1)
  dev.off()
  
  # Take the enrich_values_norm and asign to values of color vectors
  colorvector<-c()
  for (i in enrich_values_norm) {
    colorvector<-c(colorvector,color_ramp[round(x = i,digits = 3)*1000+1])
  }
  
  names(colorvector)<-names(enrich_values_norm)
  
  # Return vector with colors
  return(colorvector)
  
}

