## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: color_svg.R
##
## Purpose of script: Read blank SVG image as XML and change the attributes (colors) 
##                    of the nodes in the groups (tissues) according a provided
##                    color vector (From enrichment values) and export a png file
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


color_svg<-function(input_svg,tissue_colors,output_file) {
  
  # Requires the following packages
  library(xml2)
  library(rsvg)  # This Requires library librsvg2
  
  # Read blank svg image (a path)
  svg<-read_xml(input_svg)
  
  ## 2 Analyze xml and reach the child <g>
  tissues<-xml_contents(xml_child(svg,search = 3))
  
  ## Provide the tissues present in the vector names
  mytissues<-names(tissue_colors)
  
  ## 3 Match child names with the ones in the selected experiments
  # need to provide my tissues (e.g. names from drawing vector)
  defined_tissues<-xml_attr(tissues,"id")[match(names(tissue_colors),xml_attr(tissues,"id"))]
  
  ## 4 Loop? to change attributes according a provided color scale
  for (i in defined_tissues) {
    
    # The color for the tissue i
    current_color<-tissue_colors[i]
    
    # Find attributes within the group o
    xml_attr(xml_contents(tissues[which(xml_attr(tissues,"id")==i)]),attr = "style")
    this_tissue_attributes<-xml_attr(xml_contents(tissues[which(xml_attr(tissues,"id")==i)]),attr = "style")
    
    # Grep all occurrences of "fill:none" and replace by "fill:current_color"
    this_tissue_attributes<-gsub("fill:none",this_tissue_attributes,replacement = paste("fill",current_color,sep = ":"))
    
    # Replace the xml file
    ## I use try(silent=TRUE) because I got an error but that do not stop the function to do its job
    try(xml_attr(xml_contents(tissues[which(xml_attr(tissues,"id")==i)]),attr = "style")<-this_tissue_attributes,
        silent = TRUE)
    
  }
  
  ## Save the new svg as png
  rsvg_png(svg = charToRaw(as.character(svg)),file = output_file)
}
