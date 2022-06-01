#=======================#
## RESULTS PAGE MODULE ##
#=======================#
    
ResultsTabServer <- function (id) {
    
    moduleServer(
        id,
        
        ## MODULE FUNCTIONS
        function(input,output,session) {
            
            # Add description
            output$description <- renderText({
                return(input$user_description)
            })
            
            
            # 1. Load RData of the selected experiment
            load(paste(experiment_path,"data_with_annotation.RData",sep = "/"))
            
            
            # Parse gene list
            source("./functions/parse_input_genes.R")
            parsed_user_genelist<-parse_input_genes(input = input$user_genelist, input_specie = selected_specie, annotation_file = annotation_file)
            
            # Observe first if genes input are OK. If not do not create results tab and show error message
            {
                # TO BE FILLED
            }
            
            # Update the tabs menu and redirect to results page
            output$dynamic_tabs <- renderMenu({
                sidebarMenu(
                    # Separator and identifier -> IF not description is provided, change for date-time?
                    h5(print(input$user_description)),
                    menuItem(text = "Enrichment results",tabName = "results",icon = icon("seedling", lib = "font-awesome"))
                )
            })
            
            # Redirect to results page
            updateTabItems(session = session,
                           inputId = "tabs",
                           selected = "results")
            
            # Create a list of available tissues for function finder
            output$tissue_finder<-renderUI({
                selectInput(inputId = "tissue_finder",
                            label = paste0("Select a tissue to deploy genes"),
                            choices = names(tissue_atlas),
                            multiple = FALSE
                )
            })
            
            # Run enrichment
            source("./functions/tissue_enrichment.R")
            enrich_values<-tissue_enrichment(user_genelist=parsed_user_genelist,
                                             tissue_atlas=tissue_atlas,
                                             geneuniverse=geneuniverse)
            
            # Run drawing vector to solve overlapping between tissues in the SVG image
            svg_enrich_values<-drawing_vector(enrich_values)
            
            # Color SVG
            source("./functions/generate_color_scale.R")
            source("./functions/color_svg.R")
            colored_svg<-reactive({
                ## Generate color scale
                colors<-generate_color_scale(input = svg_enrich_values,color = input$color) #use the new enrich values for overlapping avoidance
                # color_svg
                color_svg(input_svg=normalizePath(paste(experiment_path,paste(input$experiment_id,"svg",sep="."),sep = "/")),
                          tissue_colors=colors,
                          output_file="first_TOTEM_test.png")
            })
            
            ## Generate a barplot
            colored_barplot<-reactive({
                mybarplot2(myvector = enrich_values,
                           color = input$color,
                           outputfile = "enrichment_result_barplot.png")
            })
            
            
            ## Output colored SVG 
            output$colored_svg <- renderImage(
                {
                    # Generate image
                    colored_svg()
                    # Read image
                    filename <- normalizePath("first_TOTEM_test.png")
                    list(src=filename,
                         width="50%",
                         height="100%")
                }, deleteFile = FALSE
            )
            
            ## Download button for SVG
            output$download_colored_svg <- downloadHandler(filename = "draw.png",content = normalizePath("first_TOTEM_test.png"))
            
            ## Output barplot
            output$barplot <- renderImage(
                {
                    # Generate image
                    colored_barplot()
                    # Read image
                    filename <-normalizePath("enrichment_result_barplot.png")
                    list(src=filename,
                         width="70%",
                         height="70%")
                }, deleteFile = FALSE
            )
            
            ## Download button for barplot
            output$download_barplot <- downloadHandler(filename = "barplot.png",content = normalizePath("enrichment_result_barplot.png"))
            
            # Tissue-specific genes box
            source("./functions/tissue_gene_finder.R")
            output$genes_in_tissue <- renderText({
                ## Finder function
                expr = tissue_gene_finder(user_genes = parsed_user_genelist,
                                          tissue = input$tissue_finder,
                                          tissue_atlas = tissue_atlas)
            })
            
            # add number of genes enriched
            output$number_genes_in_tissue <- renderText({
                genes = strsplit(x = tissue_gene_finder(user_genes = parsed_user_genelist,
                                                        tissue = input$tissue_finder,
                                                        tissue_atlas = tissue_atlas),
                                 split = "\n")[[1]]
                if("none" %in% genes){
                    return(0)
                }
                else{
                    return(length(genes))
                }
            })
            
            
            # Not enriched in any tissue box
            source("./functions/not_enriched.R")
            output$not_enriched <- renderText({
                
                ## Not enriched function
                expr = not_enriched(user_genes = parsed_user_genelist,
                                    tissue_atlas = tissue_atlas)
            })
            # add number of genes not enriched
            output$number_not_enriched <- renderText({
                genes = strsplit(x = not_enriched(user_genes = parsed_user_genelist,
                                                  tissue_atlas = tissue_atlas),
                                 split = "\n")[[1]]
                if("none" %in% genes){
                    return(0)
                }
                else{
                    return(length(genes))
                }
            })
            
            # Not found box
            source("./functions/not_found.R")
            output$not_found <- renderText({
                
                expr = not_found(user_genes = parsed_user_genelist,
                                 geneuniverse=geneuniverse)
                
            })
            # add number of genes not enriched
            output$number_not_found <- renderText({
                genes = strsplit(x = not_found(user_genes = parsed_user_genelist,
                                               geneuniverse=geneuniverse),
                                 split = "\n")[[1]]
                if("none" %in% genes){
                    return(0)
                }
                else{
                    return(length(genes))
                }
            })
            
            # Modal: Show warning message when trying to return to new search page
            # New search warning pop up
            
        }
    )
}
    
    