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
#' @importFrom dplyr filter
#' @importFrom ggplot2 scale_x_continuous theme_minimal theme element_blank
mod_graphes_metriques_server <- function(id, variable, point, departement, bassin,  periode, espece){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    int_breaks <- function(x, n = 5){
        if (length(unique(x)) > 1) {
            pretty(x, n)[round(pretty(x, n), 1) %% 1 == 0]
        } else {
            round(unique(x)) + c(-1, 0, 1)
        }
    }
    
    int_limits <- function(x) {
        if (length(unique(x)) > 1) {
            range(x)
        } else {
            range(int_breaks(x))
        }
    }
    
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
                            breaks = int_breaks,
                            limits = int_limits
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
                    breaks = int_breaks,
                    limits = int_limits
                )  +
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
