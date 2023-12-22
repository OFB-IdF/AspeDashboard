## CHARGEMENT DES DONNEES ----
load("dev/data/tables_sauf_mei_2022_01_13_14_22_07.RData")

# COMMENTAIRE: de manière générale, éviter les fonctions sans argument qui
# appelle des objets/données dans le corps de la fonction, surtout si ce ne sont
# pas des objets intégrés au package
passerelle <- aspe::mef_creer_passerelle() %>% 
    dplyr::as_tibble()

# DESCRIPTION: ne conserve que les points de prélèvement associés à une station
# Sandre. Utilise les coordonnées de la station Sandre, pas celles du point de
# prélèvement.
StationsSandre <- passerelle %>% 
    dplyr::filter(!is.na(sta_id)) %>% 
    dplyr::distinct(sta_id) %>% 
    dplyr::left_join(station, by = "sta_id") %>% 
    dplyr::select(
        sta_id, sta_code_sandre, sta_libelle_sandre, 
        sta_com_code_insee,
        sta_coordonnees_x, sta_coordonnees_y,
        sta_typ_id
    ) %>% 
    dplyr::left_join(
        ref_type_projection %>% 
            dplyr::select(
                typ_id, typ_code_epsg
            ),
        by = c("sta_typ_id" = "typ_id")
    ) %>% 
    dplyr::group_by(typ_code_epsg) %>% 
    dplyr::group_split() %>% 
    purrr::map_df(
        .f = function(df) {
            df %>% 
                sf::st_as_sf(
                    coords = c("sta_coordonnees_x", "sta_coordonnees_y"),
                    crs = unique(df$typ_code_epsg),
                    remove = FALSE
                ) %>% 
                sf::st_transform(
                    crs = 4326
                )
        }
    )

leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addCircleMarkers(
        data = StationsSandre,
        label = ~ sta_libelle_sandre
    )

StationsWama <- passerelle %>% 
    dplyr::filter(is.na(sta_id)) %>% 
    dplyr::distinct(pop_id) %>% 
    dplyr::left_join(point_prelevement, by = "pop_id") %>% 
    dplyr::select(
        pop_id, pop_code_wama, pop_libelle_wama,
        pop_com_code_insee_wama,
        pop_coordonnees_x, pop_coordonnees_y, pop_typ_id
    ) %>% 
    dplyr::left_join(
        ref_type_projection %>% 
            dplyr::select(
                typ_id, typ_code_epsg
            ),
        by = c("pop_typ_id" = "typ_id")
    ) %>% 
    dplyr::group_by(typ_code_epsg) %>% 
    dplyr::group_split() %>% 
    purrr::map_df(
        .f = function(df) {
            df %>% 
                sf::st_as_sf(
                    coords = c("pop_coordonnees_x", "pop_coordonnees_y"),
                    crs = unique(df$typ_code_epsg),
                    remove = FALSE
                ) %>% 
                sf::st_transform(
                    crs = 4326
                )
        }
    )

leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addCircleMarkers(
        data = StationsWama,
        label = ~pop_libelle_wama
    )

# DESCRIPTION: 
Captures <- passerelle %>% 
    dplyr::filter(!is.na(sta_id)) %>% 
    aspe::mef_ajouter_ope_date() %>% 
    aspe::mef_ajouter_lots() %>% 
    dplyr::group_by(
        sta_id, pop_id, 
        ope_id, ope_date, annee,
        esp_code_alternatif
        ) %>% 
    dplyr::summarise(
        effectif = sum(lop_effectif, na.rm = TRUE),
        .groups = "drop"
    ) %>% 
    dplyr::mutate(mois = lubridate::month(ope_date))

# DESCRIPTION: Dans le cas où plusieurs campagnes ont été réalisées par station

# Distribution mensuelle des opérations de pêche une seule fois dans l'année
Captures %>% 
    dplyr::distinct(sta_id, annee, mois, ope_date) %>% 
    dplyr::group_by(sta_id, annee) %>% 
    dplyr::mutate(n = dplyr::n_distinct(ope_date)) %>%
    dplyr::filter(n == 1) %>%
    dplyr::count(mois) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
        mapping = ggplot2::aes(
            x = as.factor(mois),
            y = n
            )
        )

# Distribution mensuelle des opérations de pêche une seule fois dans l'année
Captures %>% 
    dplyr::distinct(sta_id, annee, mois, ope_date) %>% 
    dplyr::group_by(sta_id, annee) %>% 
    dplyr::mutate(n = dplyr::n_distinct(ope_date)) %>%
    dplyr::filter(n > 1) %>%
    dplyr::count(mois) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
        mapping = ggplot2::aes(
            x = as.factor(mois),
            y = n
        )
    )

MoisMedian <- Captures %>% 
    dplyr::distinct(sta_id, annee, mois, ope_date) %>% 
    dplyr::group_by(sta_id, annee) %>% 
    dplyr::mutate(n = dplyr::n_distinct(ope_date)) %>%
    dplyr::filter(n == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(mois_median = median(mois)) %>% 
    dplyr::pull(mois_median)

Captures <- Captures %>% 
    dplyr::mutate(mois_delta = abs(mois - MoisMedian)) %>% 
    dplyr::group_by(sta_id, annee) %>% 
    dplyr::filter(ope_date == ope_date[mois_delta == min(mois_delta)][1]) %>% 
    dplyr::ungroup()

# Si plusieurs pêches par an pour une station, ne conserve que celle dont la
# date est la plus proche de la date majoritaire des stations pêchées une seule
# fois.

usethis::use_data(
    StationsSandre, 
    Captures,
    overwrite = TRUE
)




