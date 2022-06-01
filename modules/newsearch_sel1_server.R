##==========================##
## MODULE:                  ##
## newsearch_sel1_server    ##
##==========================##

## DESCRIPTION ##
# Server side of variable selection 1
# New experiment tab
# Module 1

newsearch_sel1_server <- function(input, output, session) {
    
    return(
        list(
            specie = reactive({ input$specie }),
            user_description = reactive({ input$user_description }),
            user_genelist = reactive({ input$user_genelist }),
            clear = reactive({ input$clear }),
            submit = reactive({ input$submit })
        )
    )
}