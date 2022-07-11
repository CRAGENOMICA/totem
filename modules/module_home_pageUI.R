##======================##
## MODULE:              ##
## Home page UI         ##
##======================##


## DESCRIPTION ##
# User Interface for variable selection
# Reactive UI
# New experiment tab
# Module 1

homeUI <- function(id) {
    
    fluidPage(
        
        # TOTEM banner
        tags$img(alt="TOTEM Logo", src="totem_banner_trans.png", width="125%", align="left"),
        
        # Text
        h2("Welcome to TOTEM web tool"),
        
        h4("TOTEM is a web tool designed to check if 
       tissue-specific genes are overrespresented in a user-provided gene list"),
        
        h4("In addition, the calculated p-values are 
       represented over a drawing of the specified plant specie"),
        
        hr(),
        
        h2("How to use TOTEM"),
        
        h4("Press New search tab to get started") ,
        
        h6("Here probably any cartoon or something")
    )
    
}