
#' @importFrom purrr map_chr
ajouter_lien_inpn <- function(codes_alternatif){

    purrr::map_chr(
        codes_alternatif,
        function(code_alternatif) {
            paste0(
                "<a href='https://inpn.mnhn.fr/espece/cd_nom/",
                codes_especes[[code_alternatif]],
                "' target='_blank'>", 
                code_alternatif, "</a>"
            )
        }
    )
}
