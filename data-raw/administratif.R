# Préparation données du package
if (!require("pak")) install.packages("pak")

pak::pkg_install(c("MaelTheuliere/COGiter", "dplyr", "sf", "usethis"))

dep_geo <- COGiter::departements_metro_geo %>% 
    dplyr::left_join(
        COGiter::departements,
        by = "DEP"
    ) %>% 
    dplyr::select(
        INSEE_REG = REG, 
        INSEE_DEP = DEP,
        NOM_DEP = NCCENR
    )

administratif <- dep_geo %>% 
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

usethis::use_data(administratif, internal = TRUE, overwrite = TRUE)
