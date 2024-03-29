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
mod_selecteur_espece_server <- function(id, variable, bassin, departements){
    moduleServer( id, function(input, output, session){
        ns <- session$ns
        
        observe({
                req(variable, bassin, departements)
            
            liste_especes <- carte_operations %>% 
                dplyr::filter(dh_libelle %in% bassin()) %>% 
                dplyr::filter(dept_id %in% departements()) %>% 
                dplyr::distinct(esp_code_alternatif) %>% 
                tidyr::drop_na() %>% 
                dplyr::arrange(esp_code_alternatif) %>% 
                dplyr::pull(esp_code_alternatif)
                
                if (variable() == "distribution") {
                    output$espece <- renderUI({
                        selectInput(
                            ns("espece"),
                            "Espèce",
                            c("Choisir une espèce: " = "", liste_especes)
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
