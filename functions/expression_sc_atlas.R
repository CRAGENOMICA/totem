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

plot_expression<- function(experiment_path, gene, color){
  
  # Load the umap info file
  umap = read.table(paste0(experiment_path, "/UMAP_coordinates.txt"))
  
  # Read gene .gz file. It contains the expression values of all cells with > 0.1 expression 
  my_data <- read.table(paste0(experiment_path, "/genes/", gene, ".gz"), row.names = 1)
  my_data[,1] = as.double(my_data[,1])
  
  #get the UMAP coordinates of cells expressing the selected gene
  df = umap[rownames(my_data),]
  df$Expression = my_data[,1]
  
  #KEEP CELLS WITH SOME EXPRESSION
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
  
  plot = ggplot(umap, aes(x=UMAP_1, y=UMAP_2, colour = CellPopulation))+
      geom_point(shape = 19, colour = "lightgrey")+
      theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      #add the cells with the expression color
      geom_point(data=df, aes(x=UMAP_1,y=UMAP_2, color = Expression))+
      scale_color_gradient(low = "lightgrey", high = color)+
      labs(color=paste0(gene, " expression"))
  
  return(list(df_final, plot))
}


plot_expression_population<- function(experiment_path, gene, color, cellpopulation){
  
  # Load the umap info file
  umap = read.table(paste0(experiment_path, "/UMAP_coordinates.txt"))
  
  # Read gene .gz file. It contains the expression values of all cells with > 0.1 expression 
  my_data <- read.table(paste0(experiment_path, "/genes/", gene, ".gz"), row.names = 1)
  my_data[,1] = as.double(my_data[,1])
  
  #get the UMAP coordinates of cells expressing the selected gene
  df = umap[rownames(my_data),]
  df$Expression = my_data[,1]
  
  #KEEP CELLS WITH SOME EXPRESSION
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
  
  
  population = df_final[cellpopulation,"CellPopulation"]
  if(length(population) == 1){
    label = population
  }
  else{
    label = "selected tissues"
  }
  sub_umap = subset(umap, umap$CellPopulation %in% population)
  expr = rownames(sub_umap)
  sub_umap$Expression = my_data[expr,1]
  is.na(sub_umap$Expression) = 0
  
  sub_umap_expr = subset(df, df$CellPopulation %in% population)
  
  plot_zoom = ggplot(umap, aes(x=UMAP_1, y=UMAP_2, colour = CellPopulation))+
    geom_point(shape = 19, colour = "white")+
    theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    ##add the tissue cells as grey dots
    geom_point(data=sub_umap, aes(x=UMAP_1,y=UMAP_2), color = "lightgrey")+
    #add the cells with the expression color
    geom_point(data=sub_umap_expr, aes(x=UMAP_1,y=UMAP_2, color = Expression))+
    scale_color_gradient(low = "lightgrey", high = color)+
    labs(color=paste0(gene, " expression"))+
    ggtitle(paste0(gene, " expression in ", label))
  
  return(list(df_final, plot, plot_zoom))

}