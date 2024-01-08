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
graphe_synthese_espece <- function(captures, bassins, departements, espece) {
    data_graphe <- captures %>% 
        dplyr::filter(
            dh_libelle %in% bassins,
            dept_id %in% departements
            ) %>% 
        dplyr::group_by(pop_id, annee, ope_id) %>% 
        dplyr::summarise(
            effectif = sum(effectif[esp_code_alternatif == espece]),
            .groups = "drop"
        ) %>% 
        dplyr::mutate(presence = effectif > 0)
    
    labels_metriques <- c(
        nb_pop = "Nombre de stations",
        eff_tot = "Effectif total sur l'ensemble des stations",
        eff_moy = "Effectif moyen par station où l'espèce est présente"
    )
    
    data_graphe %>% 
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
        ) %>% 
        ggplot2::ggplot() +
        ggplot2::geom_col(
            mapping = ggplot2::aes(
                x = annee, y = valeurs, fill = presence
            ),
            colour = "darkgrey"
        ) +
        ggplot2::facet_wrap(facets = ggplot2::vars(metrique), ncol = 1, scales = "free_y") +
        ggplot2::scale_fill_manual(
            values = c("white", "grey"),
            labels = c("absence", "présence")
        ) +
        ggplot2::labs(
            title = paste0("Evolution temporelle des prises de l'espèce ", espece),
            x = "", y = "", fill = ""
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "right",
            legend.justification = "top",
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(hjust = 0)
        )
    
}
