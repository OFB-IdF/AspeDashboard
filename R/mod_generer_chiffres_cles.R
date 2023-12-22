#' generer_chiffres_cles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny.semantic cards card
#' @importFrom shinydashboardPlus accordion accordionItem
mod_generer_chiffres_cles_ui <- function(id){
    ns <- NS(id)
    
    CardStyle <- "
  min-height: 45px;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: left;
  text-align: left;
  padding-top: 5px;
  padding-bottom:5px;
  padding-left: 10px;
  margin-top: 20px;
  margin-bottom: -10px;
  background-color: #8DB6CD;
  border-radius: 5px;
  "
    
    fluidPage(
        tags$head(
            tags$style(HTML("
            .panel {
                margin-bottom: 10px;
                border: 0;
                background-color: transparent;
            }
            .box-title {
                font-size: 14px;
            }
            .col-sm-12 {
                padding-left: 0px;
                padding-right: 0px;
            }
            "
        ))),
        div(),
        shiny.semantic::cards(
            class = "one",
            shiny.semantic::card(htmlOutput(ns("un")), style = CardStyle),
            shiny.semantic::card(htmlOutput(ns("deux")), style = CardStyle)
            ),
        div(),
        shiny.semantic::card(
            shinydashboardPlus::accordion(
                id = "accordion1",
                shinydashboardPlus::accordionItem(
                    title = htmlOutput(ns("trois")),
                    collapsed = TRUE,
                    htmlOutput(ns("troisbis"))
                ),
                shinydashboardPlus::accordionItem(
                    title = htmlOutput(ns("quatre")),
                    collapsed = TRUE,
                    htmlOutput(ns("quatrebis"))
                )
            ),
            style = CardStyle
        )
    )
    
}
    
#' generer_chiffres_cles Server Functions
#'
#' @noRd 
#' @importFrom dplyr filter
mod_generer_chiffres_cles_server <- function(id, variable, departement, bassin){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
        req(variable(), departement)
        
        if (variable() == "especes")
            donnees <- captures %>%
                dplyr::filter(
                    dept_id %in% departement(),
                    dh_libelle %in% bassin()
                    )
        
        if (variable() == "ipr")
            donnees <- ipr %>%
                dplyr::filter(
                    dept_id %in% departement(),
                    dh_libelle %in% bassin()
                    )
        
        indicateurs <- calculer_chiffres_cles(donnees, variable())
        
        output$un <- renderText(indicateurs$un)
        output$deux <- renderText(indicateurs$deux)
        output$trois <- renderText(indicateurs$trois$texte)
        output$troisbis <- renderText(indicateurs$trois$tooltip)
        output$quatre <- renderText(indicateurs$quatre$texte)
        output$quatrebis <- renderText(indicateurs$quatre$tooltip)

    })
  })
}
    
## To be copied in the UI
# mod_generer_chiffres_cles_ui("generer_chiffres_cles_1")
    
## To be copied in the server
# mod_generer_chiffres_cles_server("generer_chiffres_cles_1")
