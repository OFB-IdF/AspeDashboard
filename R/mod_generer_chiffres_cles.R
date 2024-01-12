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
        uiOutput(ns("chiffres_cle"))
    )
    
}
    
#' generer_chiffres_cles Server Functions
#'
#' @noRd 
#' @importFrom dplyr filter
mod_generer_chiffres_cles_server <- function(id, variable, departement, bassin, periode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
    
    output$chiffres_cle <- renderUI({
        req(variable(), departement)
        
        if (variable() %in%  c("especes", "distribution"))
            donnees <- captures %>%
                dplyr::filter(
                    dept_id %in% departement(),
                    dh_libelle %in% bassin(),
                    annee >= min(periode()) & annee <= max(periode())
                    )
        
        if (variable() == "ipr")
            donnees <- ipr %>%
                dplyr::filter(
                    dept_id %in% departement(),
                    dh_libelle %in% bassin(),
                    annee >= min(periode()) & annee <= max(periode())
                    )
        
        indicateurs <- calculer_chiffres_cles(donnees, variable())

        tagList(
            div(),
            shiny.semantic::cards(
                class = "one",
                shiny.semantic::card(
                    HTML(indicateurs$un),
                    style = CardStyle
                    ),
                shiny.semantic::card(
                    HTML(indicateurs$deux),
                    style = CardStyle
                )
            ),
            div(),
            if(variable() == "especes") {
                shiny.semantic::card(
                    shinydashboardPlus::accordion(
                        id = "accordion1",
                        shinydashboardPlus::accordionItem(
                            title = HTML(indicateurs$trois$texte),
                            collapsed = TRUE,
                            HTML(indicateurs$trois$tooltip)
                        ),
                        shinydashboardPlus::accordionItem(
                            title = HTML(indicateurs$quatre$texte),
                            collapsed = TRUE,
                            HTML(indicateurs$quatre$tooltip)
                        )
                    ),
                    style = CardStyle
                )
            }

        )
        
    })
  })
}
    
## To be copied in the UI
# mod_generer_chiffres_cles_ui("generer_chiffres_cles_1")
    
## To be copied in the server
# mod_generer_chiffres_cles_server("generer_chiffres_cles_1")
