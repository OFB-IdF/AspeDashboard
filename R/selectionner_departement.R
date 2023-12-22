#' @importFrom dplyr distinct pull filter mutate
#' @importFrom purrr set_names
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr drop_na
selectionner_departement <- function(region, bassin) {
    if (length(region) == 0 & length(bassin) == 0) {
        c(
            "Choisir un département" = "",
            purrr::set_names(
                administratif %>% 
                    dplyr::arrange(INSEE_DEP) %>% 
                    dplyr::pull(INSEE_DEP) %>% 
                    as.character(),
                administratif %>% 
                    dplyr::arrange(INSEE_DEP) %>% 
                    dplyr::pull(departement) %>% 
                    as.character()
            )
        )
    } else {
        if (length(region) == 0)
            region <- unique(administratif$INSEE_REG)
        if (length(bassin) == 0)
            bassin <- pop_geo %>% 
                sf::st_drop_geometry() %>% 
                dplyr::distinct(dh_libelle) %>% 
                tidyr::drop_na() %>% 
                dplyr::pull(dh_libelle)
                
        SelectionPop <-  pop_geo %>% 
            sf::st_drop_geometry() %>% 
            dplyr::distinct(dept_id, dept_libelle, reg_id, dh_libelle) %>% 
            dplyr::arrange(dept_id) %>% 
            dplyr::filter(
                reg_id %in% region,
                dh_libelle %in% bassin
            ) %>%
            dplyr::mutate(
                departement = paste0(dept_libelle, " (", dept_id, ")")
            )
        
        c(
            "Choisir un département" = "",
            purrr::set_names(
                SelectionPop %>% 
                    dplyr::pull(dept_id) %>% 
                    as.character(),
                SelectionPop %>% 
                    dplyr::pull(departement)
            )
        )
    }
}

