calculer_chiffres_cles <- function(donnees, variable) {
    stations <- donnees %>% 
        dplyr::group_by(pop_id) %>% 
        dplyr::summarise(
            nb_annees = dplyr::n_distinct(annee),
            .groups = "drop"
        )
    
    
    ChiffresCles <- list(
        un = paste0(
            "<b>Points de prélèvements: </b>",
            nrow(stations), 
            " (", 
            stations %>% 
                dplyr::filter(nb_annees >= 10) %>% 
                nrow(),
            " avec au moins dix années de suivi)"
        )
    )
    
    if (variable == "especes") {
        NombreEspeces <- donnees %>% 
            dplyr::ungroup() %>% 
            dplyr::distinct(esp_code_alternatif) %>% 
            nrow()
        
        ChiffresCles$deux <- paste0(
            "<b>Nombre de taxons contactés: </b>",
            NombreEspeces,
            " (sur ", 
            captures %>% 
                dplyr::ungroup() %>% 
                dplyr::distinct(esp_code_alternatif) %>% 
                nrow(),
            " nationaux)"
            )
        
        SyntheseTaxons <- donnees %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
                recent = (max(annee) - annee) <= 10
            ) %>% 
            dplyr::distinct(esp_code_alternatif, recent) %>% 
            dplyr::group_by(esp_code_alternatif) %>% 
            dplyr::summarise(
                presence = dplyr::case_when(
                    length(recent) == 2 ~ "maintien",
                    length(recent) == 1 & recent ~ "apparition",
                    length(recent) == 1 & !recent ~ "disparition"
                ),
                .groups = "drop"
            ) %>% 
            dplyr::distinct()
        
        ToolTips <- SyntheseTaxons %>% 
            dplyr::count(presence) %>% 
            dplyr::mutate(
                texte = dplyr::case_when(
                    presence == "disparition" ~ 
                        paste0(
                            "<span style='color:#333;' ><b>",
                            n, " taxons non recapturés depuis ", 
                            max(donnees$annee) - 10, "</b></span>"
                            ),
                    presence == "apparition" ~ 
                        paste0(
                            "<span style='color:#333;' ><b>",
                            n, " taxons capturés uniquement depuis ", 
                            max(donnees$annee) - 10, "</b></span>"
                        )
                ),
                tooltip = dplyr::case_when(
                    presence == "disparition" ~ 
                        SyntheseTaxons %>% 
                        dplyr::filter(presence == "disparition") %>% 
                        dplyr::pull(esp_code_alternatif) %>%
                        ajouter_lien_inpn() %>% 
                        paste(collapse = ", "),
                    presence == "apparition" ~ 
                        SyntheseTaxons %>% 
                        dplyr::filter(presence == "apparition") %>% 
                        dplyr::pull(esp_code_alternatif) %>% 
                        ajouter_lien_inpn() %>% 
                        paste(collapse = ", ")
                ),
                presence = factor(
                    presence, 
                    levels = c("maintien", "disparition", "apparition")
                    )
                ) %>% 
            dplyr::arrange(presence) %>% 
            dplyr::filter(presence != "maintien")
        
        ChiffresCles$trois <- ToolTips %>% 
            dplyr::filter(presence == "disparition")
        
        ChiffresCles$quatre <- ToolTips %>% 
            dplyr::filter(presence == "apparition")
    }
    
    if (variable == "ipr") {
        ChiffresCles$deux <- NULL
        ChiffresCles$trois <- NULL
        ChiffresCles$quatre <- NULL
    }
    
    ChiffresCles
}
