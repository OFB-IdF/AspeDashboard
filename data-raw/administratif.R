# library(AspeDashboard)
devtools::load_all()
load("../data/processed/DONNEES PRETRAITEES/poly_geo_simp.RData")

administratif <- depts_geo %>% 
    sf::st_drop_geometry() %>% 
    dplyr::distinct(
        INSEE_REG, INSEE_DEP, NOM_DEP
    ) %>% 
    dplyr::mutate(
        INSEE_REG = factor(
            INSEE_REG,
            levels = c("84", "27", "53", "24", "44", "32", "11", "28", "75", "76", "52", "93", "94")
        ),
        departement = paste0(NOM_DEP, " (", INSEE_DEP, ")")
    ) %>% 
    dplyr::filter(!is.na(INSEE_REG)) %>% 
    dplyr::arrange(INSEE_REG, NOM_DEP) %>% 
    dplyr::mutate(INSEE_REG = as.character(INSEE_REG))

usethis::use_data(administratif, overwrite = TRUE)
