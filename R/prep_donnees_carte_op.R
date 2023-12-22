#' Générer la carte des opérations de pêche
#' 
#' Génère une carte {leaflet} positionnant les points de pêche issus de la base Aspe. La taille des points indique le nombre d'années de suivi, la couleur des points la richesse moyenne en espèces piscicoles. Au clic, le détail des captures pour le point s'affiche dans un popup.
#'
#' @param captures 
#' @param id_points 
#' @param localisations 
#' @param interactif 
#' @param largeur 
#' @param hauteur 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom shiny HTML
# @importFrom aspe mef_colo_ext_pops gg_colo_ext_pops
#' @importFrom dplyr mutate left_join group_by summarise n_distinct select %>%
#' @importFrom leafpop popupGraph
#' @importFrom purrr set_names
#' @importFrom sf st_as_sf st_transform
prep_donnees_carte_op <- function(captures, id_points = NULL, localisations, interactif, largeur, hauteur, ...) {

    DonneesGraphiques <- aspe::mef_colo_ext_pops(
        captures,
        id_point = id_points
    )
    
    Graphiques <- aspe::gg_colo_ext_pops(
        DonneesGraphiques,
        interactif = interactif,
        largeur = largeur,
        hauteur = hauteur,
        ...
        )
    
    DonneesPointsPeche <- localisations %>%
        dplyr::mutate(pop_id = as.character(pop_id)) %>%
        dplyr::left_join(
            captures %>%
                dplyr::mutate(pop_id = as.character(pop_id)) %>%
                dplyr::group_by(pop_id, annee) %>%
                dplyr::summarise(
                    nb_esp = dplyr::n_distinct(esp_code_alternatif),
                    .groups = "drop"
                ) %>%
                dplyr::group_by(pop_id) %>%
                dplyr::summarise(
                    nb_annees = dplyr::n_distinct(annee),
                    nb_esp_moy = mean(nb_esp),
                    .groups= "drop"
                ),
            by = "pop_id"
        ) %>%
        dplyr::mutate(
            hover = paste0(
                "<b>", pop_libelle, " (", pop_id, ")</b><br>",
                "<em>", dept_lib, " (", reg_lib, ")</em><br>",
                nb_annees, " année",
                ifelse(nb_annees > 1 , "s", ""),
                " de suivi<br>",
                round(nb_esp_moy, 1), " espèce",
                ifelse(round(nb_esp_moy, 1) > 1, "s", ""),
                ifelse(nb_annees > 1, " en moyenne", "")
            )
        ) 

    DonneesCarte <- dplyr::left_join(
        data.frame(pop_id = names(Graphiques)),
        DonneesPointsPeche,
        by = "pop_id"
    ) %>%
        sf::st_as_sf() %>%
        sf::st_transform(4326) %>% 
        dplyr::select(pop_id, dept_num, nb_annees, nb_esp_moy, hover)
    
    # if (interactif) {
    #     Popups <- leafpop::popupGraph(
    #         Graphiques, 
    #         width = largeur*72+40,
    #         height = hauteur*72+40,
    #         type = "html"
    #     ) %>% 
    #         purrr::set_names(names(Graphiques))
    # } else {
    #     Popups <- leafpop::popupGraph(
    #         Graphiques, 
    #         width = largeur*72+5,
    #         height = hauteur*72+5,
    #         type = "png"
    #     ) %>% 
    #         purrr::set_names(names(Graphiques))
    # }
    
    list(
        data = DonneesCarte,
        graphiques = Graphiques#,
        # popups = Popups
    )
}
