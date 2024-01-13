#' selecteur_periode UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selecteur_periode_ui <- function(id){
  ns <- NS(id)
  tagList(
      sliderInput(
          inputId = ns("periode"),
          label = "Choisir la période",
          min = min(captures$annee),
          max = max(captures$annee),
          value = range(captures$annee),
          round = TRUE,
          sep = "",
          ticks = FALSE,
          # animate = TRUE
      )
  )
}
    
#' selecteur_periode Server Functions
#'
#' @noRd 
mod_selecteur_periode_server <- function(id, bassin, departement){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
        req(bassin, departement)
        
        DataPeriode <- captures %>% 
            dplyr::filter(
                dh_libelle %in% bassin(),
                dept_id %in% departement()
            )
        
        updateSliderInput(
            session = session,
            inputId = "periode",
            label = "Choisir la période",
            value = range(DataPeriode$annee),
            min = min(DataPeriode$annee),
            max = max(DataPeriode$annee)
        )
        
        })
    
    reactive({input$periode})
 
  })
}
    
## To be copied in the UI
# mod_selecteur_periode_ui("selecteur_periode_1")
    
## To be copied in the server
# mod_selecteur_periode_server("selecteur_periode_1")
