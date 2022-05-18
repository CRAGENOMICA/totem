## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: expression_scatlas.R
##
## Purpose of script: Plot the expression of a gene in the single cell atlas
##
## Author: Veredas Coleto Alcudia
##
## Date Created: 2022-05-12
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

library(readr)
library(ggplot2)

plot_expression<- function(experiment_id, gene, color, cellpopulation=NULL){
  
  # Load the gene index file and get the selected gene index
  index = read.delim(paste0("./single_cell_visualization/", experiment_id, "/geneindex.txt"), header = F)
  geneindex = as.numeric(rownames(index)[index$V1 == gene])
  
  # Load the umap info file
  umap = read.table(paste0("./single_cell_visualization/", experiment_id, "/UMAP.txt"))
  
  # Read lines function from readr package to load a single line of the loaded file
  if(experiment_id == "Root_SingleCell"){
    my_data1 <- read_lines(paste0("./single_cell_visualization/", experiment_id, "/data1.txt"),skip=geneindex,n_max = 1)
    my_data2 <- read_lines(paste0("./single_cell_visualization/", experiment_id, "/data2.txt"),skip=geneindex,n_max = 1)
    my_data3 <- read_lines(paste0("./single_cell_visualization/", experiment_id, "/data3.txt"),skip=geneindex,n_max = 1)
    my_data <- paste(my_data1,my_data2,my_data3, sep = " ")
  }
  else{
    my_data <- read_lines(paste0("./single_cell_visualization/", experiment_id, "/data.txt"),skip=geneindex,n_max = 1)
  }
  
  #get the expressions of all cells
  expressions = strsplit(my_data, " ")[[1]]
  expressions = as.double(expressions[grepl("AT", expressions)==FALSE])
  
  
  #KEEP CELLS WITH SOME EXPRESSION
  expr = expressions[expressions>0]
  cells = rownames(umap)[expressions>0]
  df = umap[cells, ]
  df$Expression = expr

  for(i in 1:length(unique(df$CellPopulation))){
    pop = unique(df$CellPopulation)[i]
    if(i == 1){
      df_final = cbind(data.frame(CellPopulation = unique(pop), Cells = nrow(subset(df, df$CellPopulation == pop))), 
                       t(data.frame(unclass(summary(subset(df, df$CellPopulation == pop)$Expression, digits=4)), check.names = FALSE, stringsAsFactors = FALSE)))
    }
    else{
      df_final = rbind(df_final, 
                       cbind(data.frame(CellPopulation = unique(pop), Cells = nrow(subset(df, df$CellPopulation == pop))), 
                             t(data.frame(unclass(summary(subset(df, df$CellPopulation == pop)$Expression, digits=4)), check.names = FALSE, stringsAsFactors = FALSE))))
    }
  }
  rownames(df_final) = 1:nrow(df_final)
  df_final = df_final[order(-df_final$Mean),]
  
  if(is.null(cellpopulation)){
    ## plot
    plot = ggplot(umap, aes(x=UMAP_1, y=UMAP_2, colour = CellPopulation))+
      geom_point(shape = 19, colour = "lightgrey")+
      theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      #add the cells with the expression color
      geom_point(data=df, aes(x=UMAP_1,y=UMAP_2, color = Expression))+
      scale_color_gradient(low = "lightgrey", high = color)+
      labs(color=paste0(gene, " expression"))
    result = list(df_final, plot)
  }
  else{
    population = df_final[cellpopulation,"CellPopulation"]
    if(length(population) == 1){
      label = population
    }
    else{
      label = "selected tissues"
    }
    sub_umap = subset(umap, umap$CellPopulation == population)
    expr = as.numeric(gsub("Cell_", "", rownames(sub_umap)))
    sub_umap$Expression = expressions[expr]
    
    sub_umap_expr = subset(df, df$CellPopulation == population)
    
    plot_zoom = ggplot(umap, aes(x=UMAP_1, y=UMAP_2, colour = CellPopulation))+
      geom_point(shape = 19, colour = "white")+
      theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      ##add the tissue cells as grey dots
      geom_point(data=sub_umap, aes(x=UMAP_1,y=UMAP_2, color = Expression))+
      scale_color_gradient(low = "lightgrey", high = "lightgrey")+
      # scale_color_gradient(low = "lightgrey", high = "darkred")+
      #add the cells with the expression color
      geom_point(data=sub_umap_expr, aes(x=UMAP_1,y=UMAP_2, color = Expression))+
      scale_color_gradient(low = "lightgrey", high = color)+
      labs(color=paste0(gene, " expression"))+
      ggtitle(paste0(gene, " expression in ", label))
    
    result = list(df_final, plot, plot_zoom)
  }
  
  
  return(result)
}

