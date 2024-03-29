#' graphe_ipr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_graphe_ipr_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("graphe"))
  )
}
    
#' graphe_ipr Server Functions
#'
#' @noRd 
#' @importFrom dplyr filter
#' @importFrom ggplot2 theme element_line element_text scale_x_continuous scale_y_continuous
#' @importFrom templatesOFB theme_ofb int_breaks int_limits
mod_graphe_ipr_server <- function(id, departement, bassin, periode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    DonneesGraphe <- ipr %>% 
        dplyr::filter(
            dh_libelle %in% bassin(),
            dept_id %in% departement(),
            annee >= min(periode()) & annee <= max(periode())
        ) 
    
    output$graphe <- renderPlot(
        graphe_ipr(donnees = DonneesGraphe) +
            templatesOFB::theme_ofb() +
            ggplot2::theme(
                panel.grid.major.y = ggplot2::element_line(colour = "grey"),
                strip.text = ggplot2::element_text(size = 12, face = "bold"),
                axis.text = ggplot2::element_text(size = 10)
            ) +
            ggplot2::scale_x_continuous(
                breaks = templatesOFB::int_breaks,
                limits = templatesOFB::int_limits
            ) +
            ggplot2::scale_y_continuous(
                breaks = templatesOFB::int_breaks,
                limits = templatesOFB::int_limits
            )
    )
  })
}
    
## To be copied in the UI
# mod_graphe_ipr_ui("graphe_ipr_1")
    
## To be copied in the server
# mod_graphe_ipr_server("graphe_ipr_1")
