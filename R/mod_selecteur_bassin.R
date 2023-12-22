#' selecteur_bassin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selecteur_bassin_ui <- function(id){
  ns <- NS(id)
  tagList(
      selectInput(
          inputId = ns("bassin"),
          label = "Bassin",
          choices = c(
              "Choisir un bassin" = "",
              "Artois-Picardie",
              "Seine-Normandie",
              "Rhin-Meuse",
              "Loire-Bretagne",
              "Adour-Garonne",
              "Rhône-Méditerranée",
              "Corse"
          ),
          multiple = TRUE
      )
  )
}
    
#' selecteur_bassin Server Functions
#'
#' @noRd 
mod_selecteur_bassin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive({

        if (length(input$bassin) == 0) {
            selection <- c(
                "Artois-Picardie",
                "Seine-Normandie",
                "Rhin-Meuse",
                "Loire-Bretagne",
                "Adour-Garonne",
                "Rhône-Méditerranée",
                "Corse"
            )
            
        } else {
            selection <- input$bassin
        }
        selection
    })
  })
}
    
## To be copied in the UI
# mod_selecteur_bassin_ui("selecteur_bassin_1")
    
## To be copied in the server
# mod_selecteur_bassin_server("selecteur_bassin_1")
