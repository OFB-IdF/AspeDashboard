#' Title
#'
#' @param captures 
#' @param bassins 
#' @param departements 
#' @param espece 
#'
#' @return
#' @export
#'
#' @examples
graphe_synthese_espece <- function(captures, espece, station) {
    data_graphe <- captures %>% 
        dplyr::group_by(pop_id, annee, ope_id) %>% 
        dplyr::summarise(
            effectif = sum(effectif[esp_code_alternatif == espece]),
            .groups = "drop"
        ) %>% 
        dplyr::mutate(presence = effectif > 0)
    
    labels_metriques <- c(
        nb_pop = "Nombre de stations",
        eff_tot = "Effectif total sur l'ensemble des stations",
        eff_moy = "Effectif moyen lorsque l'espèce est présente"
    )
    
    data_n_stations <- data_graphe %>% 
        dplyr::group_by(annee, presence) %>% 
        dplyr::summarise(
            nb_pop = dplyr::n_distinct(pop_id),
            eff_tot = sum(effectif),
            eff_moy = mean(effectif),
            .groups = "drop"
        ) %>% 
        tidyr::pivot_longer(
            cols = c(nb_pop, eff_tot, eff_moy),
            names_to = "metrique",
            values_to = "valeurs"
        ) %>% 
        dplyr::filter(
            !(metrique != "nb_pop" & !presence)
        ) %>% 
        dplyr::mutate(
            metrique = labels_metriques[metrique] %>% 
                factor(levels = labels_metriques)
        )
    
    data_station <- data_graphe %>% 
        dplyr::filter(pop_id %in% station) %>% 
        dplyr::group_by(annee, presence) %>% 
        dplyr::summarise(
            eff_moy = mean(effectif),
            .groups = "drop"
        ) %>% 
        tidyr::pivot_longer(
            cols = eff_moy,
            names_to = "metrique",
            values_to = "valeurs"
        ) %>% 
        dplyr::mutate(
            metrique = labels_metriques[metrique] %>% 
                factor(levels = labels_metriques),
            pop_id = as.character(station)
        ) %>% 
        (function(df) {
            if (nrow(df) > 0) {
                df %>% 
                    dplyr::left_join(
                        captures %>% 
                            dplyr::filter(pop_id %in% station) %>% 
                            dplyr::distinct(pop_id, pop_libelle) %>% 
                            dplyr::mutate(
                                dplyr::across(
                                    dplyr::everything(), 
                                    as.character
                                    )
                                ),
                        by = "pop_id"
                        )
                } else {
                df %>% 
                        dplyr::mutate(
                            pop_id = "",
                            pop_libelle = ""
                        )
            }
        }) %>% 
        dplyr::mutate(label = paste0(pop_libelle, "\ (", pop_id, ")"))
        
    
    ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = annee, y = valeurs, fill = presence
            )
        ) +
        ggplot2::geom_col(
            data = data_n_stations,
            colour = "darkgrey"
        ) +
        ggplot2::geom_point(
            data = data_station,
            mapping = ggplot2::aes(colour = label),
            shape = 21, size = 3
            ) +
        ggplot2::facet_wrap(facets = ggplot2::vars(metrique), ncol = 1, scales = "free_y") +
        ggplot2::scale_fill_manual(
            values = c("white", "grey"),
            labels = c("absence", "présence")
        ) +
        ggplot2::scale_colour_manual(
            values = "black"
        ) +
        ggplot2::labs(
            title = paste0("Evolution temporelle des prises de l'espèce ", espece),
            x = "", y = "", fill = "", colour = ""
        ) +
        ggplot2::guides(
            fill = ggplot2::guide_legend(order = 1),
            color = ggplot2::guide_legend(order = 2)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "bottom",
            legend.justification = "left",
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(hjust = 0)
        )
    
}
