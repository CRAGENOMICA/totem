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

library(clusterProfiler)
library(patchwork)
library(ggplot2)
library(enrichplot)
library(org.At.tair.db)

GOEA <- function(input_genes, specie, annotation_file,ontology, padjmethod, pvalcutoff, qvalcutoff) {
  if(specie != "Arabidopsis"){ # keep genes annotated with an Arabidopsis ortholog in phytozome annotation file
    input_genes = toupper(input_genes)
    genes = annotation_file$Best.hit.arabi.name[input_genes %in% annotation_file$locusName] 
  }
  else{
    genes = input_genes
  }
  
  #### GO plots ####
  
  if(length(genes)>2){ #only GO terms 
    
    go<-enrichGO(gene = genes,
                 universe = keys(org.At.tair.db), # Include all genes
                 OrgDb = org.At.tair.db,
                 keyType = "TAIR",
                 ont = ontology, # biology process
                 pAdjustMethod= padjmethod,
                 pvalueCutoff= pvalcutoff,
                 qvalueCutoff= qvalcutoff)
    go2 <- clusterProfiler::simplify(go, cutoff=0.7, by="p.adjust", select_fun=min) #remove redundant GO terms -> cutoff as in revigo
    return(go2)
  }
}

KEGGEA<-function(input_genes, specie, annotation_file, padjmethod, pvalcutoff, qvalcutoff) {
  
  if(specie != "Arabidopsis"){ # keep genes annotated with an Arabidopsis ortholog in phytozome annotation file
    input_genes = toupper(input_genes)
    genes = annotation_file$Best.hit.arabi.name[input_genes %in% annotation_file$locusName] 
  }
  else{
    genes = input_genes
  }
  
  ## KEGG plots
  genes.df = bitr(genes, fromType = "TAIR",toType = c("ENTREZID", "SYMBOL"),OrgDb = org.At.tair.db)
  if(length(genes)>2){ 
    kegg <- enrichKEGG(genes,
                       organism = "ath",
                       pAdjustMethod = padjmethod,
                       pvalueCutoff  = pvalcutoff,
                       qvalueCutoff  = qvalcutoff)
    
    return(kegg)
    
  }
}


dotplotGO<-function(input_genes, specie, annotation_file, ontology, padjmethod, pvalcutoff, qvalcutoff, color) {
  if(length(genes)>2){ #only GO terms 
    go2 = GOEA(input_genes, specie, annotation_file, ontology, padjmethod, pvalcutoff, qvalcutoff)
    if(nrow(as.data.frame(go2)) != 0){
      print("GO dotplot")
      # Create plots
      dotgo = dotplot(go2, x = "Count") + ggtitle(paste0("Gene Ontology Enrichment Analysis: GO terms")) + scale_color_gradient(low = color, high = "beige")
      return(dotgo)
    }
    else{
      par(mar = c(0,0,0,0))
      return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
               text(x = 0.5, y = 0.5, paste("No GOEA for the selected gene set"), cex = 1.6, col = "black"))
    }

  }
  else{
    par(mar = c(0,0,0,0))
    return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
             text(x = 0.5, y = 0.5, paste("Please, select a gene set with more than 2 genes \n for gene ontology enrichment analysis"), cex = 1.6, col = "black"))
  }
}
netGO<-function(input_genes, specie, annotation_file, ontology, padjmethod, pvalcutoff, qvalcutoff, color) {
  if(length(genes)>2){ #only GO terms 
    go2 = GOEA(input_genes, specie, annotation_file, ontology, padjmethod, pvalcutoff, qvalcutoff)

    if(nrow(as.data.frame(go2)) != 0){
      print("GO net")
      # Create plots
      emap = emapplot(pairwise_termsim(go2))+ ggtitle(paste0("Gene Ontology Enrichment Analysis: GO terms relation"))+ scale_color_gradient(low=color, high="beige")
      return(emap)
    }
    else{
      par(mar = c(0,0,0,0))
      return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
               text(x = 0.5, y = 0.5, paste("No GOEA for the selected gene set"), cex = 1.6, col = "black"))
    }
    
  }
  else{
    par(mar = c(0,0,0,0))
    return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
             text(x = 0.5, y = 0.5, paste("Please, select a gene set with more than 2 genes \n for gene ontology enrichment analysis"), cex = 1.6, col = "black"))
  }
}
netgenesGO<-function(input_genes, specie,annotation_file, ontology, padjmethod, pvalcutoff, qvalcutoff, color) {
  if(length(genes)>2){ #only GO terms 
    go2 = GOEA(input_genes, specie,annotation_file, ontology, padjmethod, pvalcutoff, qvalcutoff)
    gnames = setReadable(go2, org.At.tair.db, keyType = "TAIR")
    
    if(nrow(as.data.frame(go2)) != 0){
      print("GO net genes")
      # Create plots
      cnet = cnetplot(gnames, circular = F, colorEdge = T)+ ggtitle(paste0("Gene Ontology Enrichment Analysis: genes related to GO terms"))
      return(cnet)
    }
    else{
      par(mar = c(0,0,0,0))
      return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
               text(x = 0.5, y = 0.5, paste("No GOEA for the selected gene set"), cex = 1.6, col = "black"))
    }
    
  }
  else{
    par(mar = c(0,0,0,0))
    return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
             text(x = 0.5, y = 0.5, paste("Please, select a gene set with more than 2 genes \n for gene ontology enrichment analysis"), cex = 1.6, col = "black"))
  }
}

dotplotKEGG <- function(input_genes, specie, annotation_file, padjmethod, pvalcutoff, qvalcutoff, color) {
  kegg = KEGGEA(input_genes, specie, annotation_file, padjmethod, pvalcutoff, qvalcutoff)
  if(length(genes)>2){ 
    if(nrow(as.data.frame(kegg)) != 0){
      print("KEGG plots")
      dotkegg = dotplot(kegg,x = "Count") + ggtitle(paste0("KEGG Enrichment Analysis: KEGG terms"))+scale_color_gradient(low = color, high = "beige")
      
      return(dotkegg)
    }
    else{
      par(mar = c(0,0,0,0))
      return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
               text(x = 0.5, y = 0.5, paste("No KEGG enrichment for the selected gene set"), cex = 1.6, col = "black"))
    }
  }
  else{
    par(mar = c(0,0,0,0))
    return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
             text(x = 0.5, y = 0.5, paste("Please, select a gene set with more than 2 genes \n for KEGG enrichment analysis"), cex = 1.6, col = "black"))
  }
  # stop("Try other set of genes or parameters for KEGG enrichment analysis")
}
heatmapKEGG <- function(input_genes, specie, annotation_file, padjmethod, pvalcutoff, qvalcutoff, color) {
  kegg = KEGGEA(input_genes, specie, annotation_file, padjmethod, pvalcutoff, qvalcutoff)
  if(length(genes)>2){ 
    if(nrow(as.data.frame(kegg)) != 0){
      print("KEGG plots")
      heatkegg = heatplot(kegg)+ ggtitle(paste0("KEGG Enrichment Analysis: genes related to KEGG terms"))
     
      return(heatkegg)
    }
    else{
      par(mar = c(0,0,0,0))
      return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
               text(x = 0.5, y = 0.5, paste("No KEGG enrichment for the selected gene set"), cex = 1.6, col = "black"))
    }
  }
  else{
    par(mar = c(0,0,0,0))
    return(plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')+
             text(x = 0.5, y = 0.5, paste("Please, select a gene set with more than 2 genes \n for KEGG enrichment analysis"), cex = 1.6, col = "black"))
  }
  # stop("Try other set of genes or parameters for KEGG enrichment analysis")
}
