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

input_genes = c("AT2G27550",
                "AT2G33480",
                "AT2G40080",
                "AT2G37750",
                "AT3G17790",
                "AT2G34610",
                "AT3G57380",
                "AT2G37870")
# annotation_file = read.csv("Arabidopsis_AnnotationFunctional.csv")
# specie = "Arabidopsis"


GO_plots<-function(input_genes, annotation_file, specie, ontology, padjmethod, pvalcutoff, qvalcutoff, outputfile) {
  
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
    go2 <- clusterProfiler::simplify(go, cutoff=0.6, by="p.adjust", select_fun=min) #remove redundant GO terms
    gnames = setReadable(go2, org.At.tair.db, keyType = "TAIR")
    kegg <- enrichKEGG(genes,
                       universe = keys(org.At.tair.db), # Include all genes
                       organism = "ath",
                       keyType = "kegg",
                       pAdjustMethod = padjmethod,
                       pvalueCutoff  = pvalcutoff,
                       qvalueCutoff  = qvalcutoff)
    if(nrow(go2@result) != 0){
      # Create plots
      dotgo = dotplot(go2, showCategory=20, x = "Count") + ggtitle(paste0("A) GOEA in ", specie)) + scale_color_gradient(low = "dark red", high = "beige")
      dotkegg = dotplot(kegg, showCategory=20,x = "Count") + ggtitle(paste0("B) KEGG in ", specie))+scale_color_gradient(low = "dark red", high = "beige")
      cnet = cnetplot(gnames, showCategory = 20, circular = F, colorEdge = T)+ ggtitle(paste0("C) GOEA: network with genes in ", specie))
      emap = emapplot(pairwise_termsim(go2), showCategory = 20)+ ggtitle(paste0("D) GOEA: network in ", specie))+ scale_color_gradient(low='dark red', high='beige')
      
      # Output file
      png(filename = outputfile, width = 60,height = 30,units = "cm",res=800)
      # return(wrap_plots(dotgo, dotkegg, cnet, emap))
      print(wrap_plots(dotgo, dotkegg, cnet, emap))
      # Close device and save png image
      dev.off()
    }
    else{
      # Output file
      png(filename = outputfile, width = 60,height = 30,units = "cm",res=800)
       par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No GO enrichment for this set of genes.\n"), 
           cex = 1.6, col = "black")
      dev.off()
    }
  }
  else{
    # Output file
    png(filename = outputfile, width = 60,height = 30,units = "cm",res=800)
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Please try a set > 2 genes for a GO enrichment analysis.\n"), 
         cex = 1.6, col = "black")
    dev.off()
  }
  # stop("Try other set of genes or parameters")
}
