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
        # cat("\n----\nvariable: ")
        # print(variable())
        # cat("point: ")
        # print(point())
        # cat("bassin: ")
        # print(bassin())
        # cat("departement: ")
        # print(departement())
        # cat("espece: ")
        # print(espece())

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
        
        if (variable() != "distribution") {
            if (is.null(point())) {
                # print("chiffre clé")
                mod_generer_chiffres_cles_ui(id = ns("chiffres_cles"))
            } else {
                # print("graphique métriques")
                mod_graphes_metriques_ui(id = ns("graphe_metrique"))
            }
            
        } else {
            # cat("graphiques distribution")
            # print(paste(variable(), espece()))
            mod_graphes_metriques_ui(id = ns("graphe_metrique"))
        }
    })
    
 
  })
}
    
## To be copied in the UI
# mod_panneau_droit_ui("panneau_droit_1")
    
## To be copied in the server
# mod_panneau_droit_server("panneau_droit_1")
