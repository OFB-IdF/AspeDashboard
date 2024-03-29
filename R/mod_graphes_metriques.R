#' graphes_metriques UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_graphes_metriques_ui <- function(id){
  ns <- NS(id)
  tagList(
      tags$style(type = "text/css", paste0("#", ns("graphe"), " {height: calc(100vh - 210px) !important;}")),
      
      plotOutput(ns("graphe"))
  )
}
    
#' graphes_metriques Server Functions
#'
#' @noRd 
#' @importFrom dplyr filter distinct mutate pull
#' @importFrom ggplot2 scale_x_continuous labs theme_minimal theme element_blank element_text scale_y_continuous
#' @importFrom templatesOFB int_breaks int_limits
mod_graphes_metriques_server <- function(id, variable, point, departement, bassin,  periode, espece){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$graphe <- renderPlot({
        req(variable, point, bassin, departement, espece)
        if (variable() != "distribution") {
            if (!is.null(point())) {
                SelectionMetriques <- metriques %>% 
                    dplyr::filter(
                        variable == variable(),
                        pop_id == point(),
                        annee >= min(periode()) & annee <= max(periode())
                    )
                
                if (nrow(SelectionMetriques) == 0) {
                    NULL
                } else {
                    SelectionMetriques %>% 
                        gg_temp_metriq_grille(
                            df_metriques = .,
                            var_id_sta = pop_libelle,
                            var_nom_metrique = metrique,
                            var_valeur_metrique = valeur,
                            nb_colonnes = 2,
                            orientation = "v"
                        ) +
                        ggplot2::scale_x_continuous(
                            breaks = templatesOFB::int_breaks,
                            limits = templatesOFB::int_limits
                        ) +
                        ggplot2::labs(
                            title = SelectionMetriques %>% 
                                dplyr::distinct(pop_libelle, ope_id) %>% 
                                dplyr::mutate(
                                    titre = paste0(
                                        pop_libelle, " (", ope_id, ")"
                                    )
                                ) %>% 
                                dplyr::pull(titre)
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            panel.grid.major.x = ggplot2::element_blank(),
                            panel.grid.minor.x = ggplot2::element_blank(),
                            panel.grid.minor.y = ggplot2::element_blank(),
                            plot.title = ggplot2::element_text(face = "bold")
                        )
                }
            }
        } else {
            graphe <- captures %>% 
                dplyr::filter(
                    dh_libelle %in% bassin(),
                    dept_id %in% departement(),
                    annee >= min(periode()) & annee <= max(periode())
                ) %>% 
                graphe_synthese_espece(
                espece = espece(),
                station = point()
            ) +
                ggplot2::scale_x_continuous(
                    breaks = templatesOFB::int_breaks,
                    limits = templatesOFB::int_limits
                )  +
                ggplot2::scale_y_continuous(
                    breaks = templatesOFB::int_breaks,
                    limits = templatesOFB::int_limits
                ) +
                ggplot2::theme(
                    strip.text = ggplot2::element_text(hjust = 0, size = 12, face = "bold"),
                    axis.text = ggplot2::element_text(size = 10),
                    legend.text = ggplot2::element_text(size = 10)
                )
            
            graphe
        }

        
    })
    
  })
}
    
## To be copied in the UI
# mod_graphes_metriques_ui("graphes_metriques_1")
    
## To be copied in the server
# mod_graphes_metriques_server("graphes_metriques_1")
