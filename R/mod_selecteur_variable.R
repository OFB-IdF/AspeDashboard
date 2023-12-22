#' selecteur_variable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selecteur_variable_ui <- function(id){
  ns <- NS(id)
  tagList(
      selectInput(
          inputId = ns("variable"),
          label = "Variable d'intérêt",
          choices = c(
              "Composition taxonomique" = "especes",
              "Indice Poisson Rivière" = "ipr"
          ),
          multiple = FALSE
      )
  )
}
    
#' selecteur_variable Server Functions
#'
#' @noRd 
mod_selecteur_variable_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    reactive({
        input$variable
        })
  })
}
    
## To be copied in the UI
# mod_selecteur_variable_ui("selecteur_variable_1")
    
## To be copied in the server
# mod_selecteur_variable_server("selecteur_variable_1")
