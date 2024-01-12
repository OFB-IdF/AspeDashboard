#' panneau_droit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_panneau_droit_ui <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(ns("panneau"))
  )
}
    
#' panneau_droit Server Functions
#'
#' @noRd 
mod_panneau_droit_server <- function(id, variable, departement, bassin, point, espece){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$panneau <- renderUI({

        mod_generer_chiffres_cles_server(
            id = "chiffres_cles",
            variable = variable,
            departement = departement,
            bassin = bassin
        )
        
        mod_graphes_metriques_server(
            id = "graphe_metrique",
            variable = variable,
            point = point,
            departement = departement,
            bassin = bassin,
            espece = espece
        )
        
        mod_graphe_ipr_server(
            id = "graphe_ipr",
            departement = departement,
            bassin = bassin
        )
        
        if (variable() == "especes") {
            mod_generer_chiffres_cles_ui(id = ns("chiffres_cles"))
        } else {
            if (variable() == "ipr") {
                if (is.null(point())) {
                    tagList(
                        mod_generer_chiffres_cles_ui(id = ns("chiffres_cles")),
                        mod_graphe_ipr_ui(id = ns("graphe_ipr"))
                    )
                    
                } else {
                    tagList(
                        mod_graphes_metriques_ui(id = ns("graphe_metrique"))
                    )
                    
                }
            } else {
                if (variable() == "distribution") {
                    mod_graphes_metriques_ui(id = ns("graphe_metrique"))
                }
            }
        }
        
    })
    
  })
}
    
## To be copied in the UI
# mod_panneau_droit_ui("panneau_droit_1")
    
## To be copied in the server
# mod_panneau_droit_server("panneau_droit_1")
