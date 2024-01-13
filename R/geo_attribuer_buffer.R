#' Title
#'
#' @param points_geo 
#' @param poly_sf 
#' @param crs_sortie 
#' @param buffer 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom aspe geo_attribuer
#' @importFrom dplyr bind_rows filter if_any if_all select
#' @importFrom sf st_drop_geometry st_buffer
geo_attribuer_buffer <- function(points_geo, poly_sf, crs_sortie, buffer) {
    new_points <- aspe::geo_attribuer(points_geo, poly_sf)
    
    dplyr::bind_rows(
        new_points %>% 
            dplyr::filter(dplyr::if_any(
                poly_sf %>% 
                    sf::st_drop_geometry() %>% 
                    colnames(),
                function(x) {!is.na(x)}
            )),
        new_points %>% 
            dplyr::filter(dplyr::if_all(
                poly_sf %>% 
                    sf::st_drop_geometry() %>% 
                    colnames(),
                ~is.na(.)
            )) %>% 
            dplyr::select(-(poly_sf %>% 
                              sf::st_drop_geometry() %>% 
                              colnames())) %>% 
            aspe::geo_attribuer(
                poly_sf %>% 
                    sf::st_buffer(dist = buffer)
            )
    )
    
}
