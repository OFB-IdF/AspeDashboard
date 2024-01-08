#' selecteur_espece UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selecteur_espece_ui <- function(id){
    ns <- NS(id)
    uiOutput(ns("espece"))
}

#' selecteur_variable Server Functions
#'
#' @noRd 
mod_selecteur_espece_server <- function(id, variable){
    moduleServer( id, function(input, output, session){
        ns <- session$ns
        
        observe({
                req(variable)
                
                if (variable() == "distribution") {
                    output$espece <- renderUI({
                        selectInput(
                            ns("espece"),
                            "Espèce",
                            sort(names(AspeDashboard::codes_especes))
                        )
                    })
                } else {
                    output$espece <- renderUI({})
                }   
            })
        
        return(reactive({input$espece}))
    })
    
    
}

## To be copied in the UI
# mod_selecteur_variable_ui("selecteur_variable_1")

## To be copied in the server
# mod_selecteur_variable_server("selecteur_variable_1")
