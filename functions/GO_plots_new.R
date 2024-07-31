## %%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Script name: functional_characterization.R
##
## Purpose of script: GO enrichment of a gene list
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

gene_list_org <- function(updateProgress = NULL, input_genes, specie){
  if(specie == "Solanum lycopersicum"){ 
    genes = paste0(input_genes, ".1")  # gprofiler tomato input needs transcript ID instead of gene -> add .1 to the gene 
    org = "slycopersicum"
  }
  else if(specie == "Sorghum bicolor"){
    genes = gsub("Sobic.", "SORBI_3", input_genes)# gprofiler sorghum input uses ensembl plant id --> change sobic. prefix for sorbi_3
    org = "sbicolor"
  }
  else if(specie == "Arabidopsis thaliana"){
    genes = input_genes # for arbidopsis, gprofiler input is AT*G*****
    org = "athaliana"
  }
  else if(specie == "Zea mays"){
    ann = read.delim("experiments/Zea mays/annotation_file.txt", header= T)
    genes = unique(ann[ann$locusName %in% user_genelist, "locusName_B73"]) #gprofiler uses the B73 v5 gene ID
    genes = genes[is.na(genes)==F]
    org = "zmays"
  }
  
  return(list(genes, org))
  
}

dotplotGO<-function(updateProgress = NULL, genes, org, ontology, padjmethod, pvalcutoff) {
    
  if(length(genes)>2){ #only GO terms 

    # use gost gprofiler function to calculate enrichment
    gostres <- gost(query = genes, 
                    organism = org, exclude_iea = TRUE, evcodes = TRUE,
                    user_threshold = pvalcutoff, correction_method = padjmethod, 
                    domain_scope = "annotated", sources = ontology)
    
    if(is.null(gostres) == FALSE){
      # for plotting, we have to create a enrichResult object by creating previously a gp_mod dataframe
      gp_mod = gostres$result[,c("query", "source", "term_id", "term_name", "p_value", "query_size",
                                 "intersection_size", "term_size", "effective_domain_size", "intersection")]
      gp_mod$GeneRatio = paste0(gp_mod$intersection_size, "/", gp_mod$term_size) #Gene ratio is the percentage of input genes in a given GO term 
      gp_mod$BgRatio = paste0(gp_mod$term_size, "/", gp_mod$effective_domain_size)
      names(gp_mod) = c("Cluster", "source", "ID", "Description", "p.adjust",
                        "query_size", "Count", "term_size", "effective_domain_size", "geneID", "GeneRatio", "BgRatio")
      gp_mod$geneID = gsub(",", "/", gp_mod$geneID)
      row.names(gp_mod) = gp_mod$ID
      # define as enrichResult object
      gp_mod_enrich = new("enrichResult", result = gp_mod, ontology = gsub("GO:", "", ontology))
      
      #remove redundant GO terms -> cutoff as in revigo, 0,7
      go2 <- clusterProfiler::simplify(gp_mod_enrich, cutoff=0.7, by="p.adjust", select_fun=min) 
      go2@gene = genes
    
      # Create plots
      dotgo = dotplot(go2, x = "Count", showCategory = 20) +
        ggtitle("Gene Ontology Enrichment Analysis: GO terms")
      
      
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

netgenesGO<-function(updateProgress = NULL, genes, org, ontology, padjmethod, pvalcutoff) {

  if(length(genes)>2){ #only GO terms
    # use gost gprofiler function to calculate enrichment
    gostres <- gost(query = genes,
                    organism = org, exclude_iea = TRUE, evcodes = TRUE,
                    user_threshold = pvalcutoff, correction_method = padjmethod,
                    domain_scope = "annotated", sources = ontology)

    if(is.null(gostres) == FALSE){
      # for plotting, we have to create a enrichResult object by creating previously a gp_mod dataframe
      gp_mod = gostres$result[,c("query", "source", "term_id", "term_name", "p_value", "query_size",
                                 "intersection_size", "term_size", "effective_domain_size", "intersection")]
      gp_mod$GeneRatio = paste0(gp_mod$intersection_size, "/", gp_mod$query_size)
      gp_mod$BgRatio = paste0(gp_mod$term_size, "/", gp_mod$effective_domain_size)
      names(gp_mod) = c("Cluster", "source", "ID", "Description", "p.adjust",
                        "query_size", "Count", "term_size", "effective_domain_size", "geneID", "GeneRatio", "BgRatio")
      gp_mod$geneID = gsub(",", "/", gp_mod$geneID)
      row.names(gp_mod) = gp_mod$ID
      # define as enrichResult object
      gp_mod_enrich = new("enrichResult", result = gp_mod, ontology = gsub("GO:", "", ontology))

      #remove redundant GO terms -> cutoff as in revigo, 0,7
      go2 <- clusterProfiler::simplify(gp_mod_enrich, cutoff=0.7, by="p.adjust", select_fun=min)
      # go2@gene = genes

      # Create plots
      emap = emapplot(pairwise_termsim(go2), showCategory = 20, cex_label_category = 0.8)+
        ggtitle("Gene Ontology Enrichment Analysis: GO terms relation")

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
heatmapGO<-function(updateProgress = NULL, genes, org, ontology, padjmethod, pvalcutoff) {

  if(length(genes)>2){ #only GO terms

    # use gost gprofiler function to calculate enrichment
    gostres <- gost(query = genes,
                    organism = org, exclude_iea = TRUE, evcodes = TRUE,
                    user_threshold = pvalcutoff, correction_method = padjmethod,
                    domain_scope = "annotated", sources = ontology)

    if(is.null(gostres) == FALSE){
      # for plotting, we have to create a enrichResult object by creating previously a gp_mod dataframe
      gp_mod = gostres$result[,c("query", "source", "term_id", "term_name", "p_value", "query_size",
                                 "intersection_size", "term_size", "effective_domain_size", "intersection")]
      gp_mod$GeneRatio = paste0(gp_mod$intersection_size, "/", gp_mod$query_size)
      gp_mod$BgRatio = paste0(gp_mod$term_size, "/", gp_mod$effective_domain_size)
      names(gp_mod) = c("Cluster", "source", "ID", "Description", "p.adjust",
                        "query_size", "Count", "term_size", "effective_domain_size", "geneID", "GeneRatio", "BgRatio")
      gp_mod$geneID = gsub(",", "/", gp_mod$geneID)
      row.names(gp_mod) = gp_mod$ID
      # define as enrichResult object
      gp_mod_enrich = new("enrichResult", result = gp_mod, ontology = gsub("GO:", "", ontology))

      #remove redundant GO terms -> cutoff as in revigo, 0,7
      go2 <- clusterProfiler::simplify(gp_mod_enrich, cutoff=0.7, by="p.adjust", select_fun=min)
      go2@gene = genes

      # Create plots
      heatGO = heatplot(go2, showCategory = 20)+
        ggtitle("Gene Ontology Enrichment Analysis: genes related to GO terms")
      return(heatGO)
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

dotplotKEGG <- function(updateProgress = NULL, genes, org, padjmethod, pvalcutoff) {

  ## KEGG plots
  if(length(genes)>2){
    # use gost gprofiler function to calculate enrichment
    gostres <- gost(query = genes, 
                    organism = org, exclude_iea = TRUE, evcodes = TRUE,
                    user_threshold = pvalcutoff, correction_method = padjmethod, 
                    domain_scope = "annotated", sources = "KEGG")
    
    if(is.null(gostres) == FALSE){
      # for plotting, we have to create a enrichResult object by creating previously a gp_mod dataframe
      gp_mod = gostres$result[,c("query", "source", "term_id", "term_name", "p_value", "query_size",
                                 "intersection_size", "term_size", "effective_domain_size", "intersection")]
      gp_mod$GeneRatio = paste0(gp_mod$intersection_size, "/", gp_mod$query_size)
      gp_mod$BgRatio = paste0(gp_mod$term_size, "/", gp_mod$effective_domain_size)
      names(gp_mod) = c("Cluster", "source", "ID", "Description", "p.adjust",
                        "query_size", "Count", "term_size", "effective_domain_size", "geneID", "GeneRatio", "BgRatio")
      gp_mod$geneID = gsub(",", "/", gp_mod$geneID)
      row.names(gp_mod) = gp_mod$ID
      # define as enrichResult object
      gp_mod_enrich = new("enrichResult", result = gp_mod, ontology = "KEGG")
      
      kegg <- gp_mod_enrich 
      kegg@gene = genes
    
      dotkegg = dotplot(kegg,x = "Count", showCategory = 20) + 
        ggtitle("KEGG pathway Enrichment Analysis: KEGG pathways")
      
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
}
heatmapKEGG <- function(updateProgress = NULL, genes, org, padjmethod, pvalcutoff) {

  ## KEGG plots
  if(length(genes)>2){
    # use gost gprofiler function to calculate enrichment
    gostres <- gost(query = genes,
                    organism = org, exclude_iea = TRUE, evcodes = TRUE,
                    user_threshold = pvalcutoff, correction_method = padjmethod,
                    domain_scope = "annotated", sources = "KEGG")

    if(is.null(gostres) == FALSE){
      # for plotting, we have to create a enrichResult object by creating previously a gp_mod dataframe
      gp_mod = gostres$result[,c("query", "source", "term_id", "term_name", "p_value", "query_size",
                                 "intersection_size", "term_size", "effective_domain_size", "intersection")]
      gp_mod$GeneRatio = paste0(gp_mod$intersection_size, "/", gp_mod$query_size)
      gp_mod$BgRatio = paste0(gp_mod$term_size, "/", gp_mod$effective_domain_size)
      names(gp_mod) = c("Cluster", "source", "ID", "Description", "p.adjust",
                        "query_size", "Count", "term_size", "effective_domain_size", "geneID", "GeneRatio", "BgRatio")
      gp_mod$geneID = gsub(",", "/", gp_mod$geneID)
      row.names(gp_mod) = gp_mod$ID
      # define as enrichResult object
      gp_mod_enrich = new("enrichResult", result = gp_mod, ontology = "KEGG")

      kegg <- gp_mod_enrich
      kegg@gene = genes

      heatkegg = heatplot(kegg, showCategory = 20)+
      ggtitle("KEGG pathway Enrichment Analysis: genes related to KEGG patways")

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
}
