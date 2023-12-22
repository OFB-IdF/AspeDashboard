#' selecteur_admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selecteur_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("region"),
      label = "Région",
      choices = c(
        "Choisir une région" = "",
        "Auvergne-Rhône-Alpes" = "84",
        "Bourgogne-Franche-Comté" = "27",
        "Bretagne" = "53",
        "Centre-Val-de-Loire" = "24",
        "Grand-Est" = "44",
        "Hauts-de-France" = "32",
        "Ile-de-France" = "11",
        "Normandie" = "28",
        "Nouvelle-Aquitaine" = "75",
        "Occitanie" = "76",
        "Pays-de-la-Loire" = "52",
        "Provence-Alpes-Côte-d'Azur" = "93",
        "Corse" = "94"
      ),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("departement"),
      label = "Département",
      choices = selectionner_departement("", ""),
      multiple = TRUE
    )
  )
}
    
#' selecteur_admin Server Functions
#'
#' @noRd 
mod_selecteur_admin_server <- function(id, bassin){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe(
      {

        updateSelectInput(
          session = session,
          inputId = "departement",
          choices = selectionner_departement(
              region = input$region,
              bassin = bassin()
              )
        )
          
        }
      )
    
    reactive({

      if (length(input$departement) == 0) {
        selection <- selectionner_departement(
            region = input$region,
            bassin = bassin()
            )[-1] %>%
          unname()
      } else {
        selection <- input$departement
      }
      selection
    })
    
  })
}
    
## To be copied in the UI
# mod_selecteur_admin_ui("selecteur_admin_ui_1")
    
## To be copied in the server
# mod_selecteur_admin_server("selecteur_admin_ui_1")
