#' @param widgets 
#'
#' @param widget_dir 
#' @param lib_dir 
#' @param self_contained 
#' @importFrom htmlwidgets saveWidget
#' @importFrom progress progress_bar
#' @importFrom purrr map
prep_sauver_widgets <- function(widgets, widget_dir, lib_dir = NULL, self_contained = FALSE) {
    # based on https://github.com/r-spatial/leafpop/blob/master/R/addPopupWidgets.R
    if (dir.exists(widget_dir))
        unlink(widget_dir, recursive = TRUE)
    dir.create(widget_dir)
    
    if (!is.null(lib_dir) && !dir.exists(lib_dir))
        dir.create(lib_dir)
    
        pb <- progress::progress_bar$new(
        total = length(widgets),
        format = " [:bar] :percent (:eta)"
    )
    
    purrr::map(
        names(widgets), 
        function(i) {
            pb$tick()
            
            if (inherits(widgets[[i]], "htmlwidget")) {
                flnm = paste0("file_", i, ".html")
                fl = paste(widget_dir, flnm, sep = "/")
                
                htmlwidgets::saveWidget(
                    widget = widgets[[i]],
                    selfcontained = self_contained,
                    file = fl,
                    libdir = lib_dir
                )
            }
            
            return(fl)
            
        })

}

#' @param chemin_widgets 
#'
#' @param dir_widgets 
#' @param largeur 
#' @param hauteur 
#' @importFrom purrr map set_names
#' @importFrom stringr str_extract str_remove str_replace_all str_remove_all
prep_popups <- function(chemin_widgets, dir_widgets, largeur, hauteur) {
    popups <- purrr::map(
        chemin_widgets,
        leafpop:::popupIframe, 
        width = largeur*72+5, 
        height = hauteur*72+5
    )
    
    popups_style <- stringr::str_extract(
        popups[[1]], 
        pattern = "<head> <style>.*</style> </head>"
    ) %>% 
        stringr::str_remove("<head> <style>") %>% 
        stringr::str_remove("</style> </head>") %>% 
        stringr::str_replace_all(pattern = ";", replacement = ";\n") %>% 
        stringr::str_replace_all(pattern = "\\}", replacement = "\\}\n\n") %>% 
        stringr::str_replace_all(pattern = "\\{", replacement = "\\{\n")  
    
    popups <- popups %>% 
        stringr::str_remove_all(
            pattern = "<head> <style>.*</style> </head>"
        ) %>% 
        stringr::str_remove_all(
            pattern = "inst/app/"
        ) %>% 
        purrr::set_names(
            chemin_widgets %>% 
                stringr::str_remove_all(dir_widgets) %>% 
                stringr::str_remove_all("/file_") %>% 
                stringr::str_remove_all(".html")
        )
    
    list(
        popups = popups,
        css = popups_style
    )
}

#' Title
#'
#' @param plots 
#' @param dir_popup 
#' @param largeur_popup 
#' @param hauteur_popup 
#' @param verbose 
#'
#' @return
#' @export
#'
prep_sauver_popups <- function(plots, dir_popup, largeur_popup, hauteur_popup, reduire_marges = TRUE, lien_inpn = FALSE, verbose = TRUE) {


    if (verbose) cat("Enregistrement des graphiques HTML\n")
    
    widgets <- prep_sauver_widgets(
        widgets = plots,
        widget_dir = dir_popup,
        lib_dir = "js",
        self_contained = FALSE
    )
    
    if (verbose) cat("Préparation des popups\n")
    popups <- prep_popups(
        chemin_widgets = widgets,
        dir_widgets = dir_popup,
        largeur = largeur_popup,
        hauteur = hauteur_popup
    )
    
    if (reduire_marges | lien_inpn) {
        cat("Ajustements des fichiers HTML\n")
        
        fichiers <- list.files(dir_popup,
                               pattern = ".html",
                               full.names = TRUE)
        
        pb <- progress::progress_bar$new(
            total = length(fichiers),
            format = " [:bar] :percent (:eta) -- :elapsed"
        )
        
        purrr::walk(
            fichiers,
            function(chemin) {
                nouveau_texte <- readLines(chemin) %>% 
                    (function(texte) {
                        text_out <- texte
                        
                        if (reduire_marges) {
                            text_out <- text_out %>% 
                                stringr::str_replace_all(
                                    pattern = "\\\"padding\\\":40,\\\"fill\\\":false",
                                    replacement = "\\\"padding\\\":0,\\\"fill\\\":false"
                                    ) %>% 
                                stringr::str_replace_all(
                                    pattern = "\\\"padding\\\":15,\\\"fill\\\":true",
                                    replacement = "\\\"padding\\\":0,\\\"fill\\\":true"
                                )
                        }
                        
                        if (lien_inpn) {
                            codes_especes <- aspe::data_passerelle_taxo$esp_code_taxref %>% 
                                purrr::set_names(aspe::data_passerelle_taxo$esp_code_alternatif)
                            
                            
                            for (code_alternatif in names(codes_especes)) {
                                if (
                                    any(
                                        stringr::str_detect(
                                            string = text_out,
                                            pattern = code_alternatif
                                            )
                                    )
                                ) {
                                    text_out <- stringr::str_replace_all(
                                        string = text_out,
                                        pattern = paste0(
                                            ">", code_alternatif, "<\\\\/text>"
                                            ),
                                        replacement = paste0(
                                            "><a href='https://inpn.mnhn.fr/espece/cd_nom/",
                                            codes_especes[[code_alternatif]],
                                            "' target='_blank'>", 
                                            code_alternatif, "</a></text>"
                                            )
                                    )
                                }
                            }
                        }
                        
                        text_out
                    })
                
                cat(nouveau_texte, file = chemin, sep = "\n")
                
                pb$tick()

            }
        )

    }
    
    popups
}


#' Title
#'
#' @param dir_popup 
#' @param archive_name 
#' @param verbose 
#' @param delete_dir 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom utils tar
archiver_popups <- function(dir_popup, archive_name, verbose = TRUE, delete_dir = FALSE) {
    if (verbose) cat("Compression des fichiers")
    utils::tar(
        tarfile = archive_name,
        files = list.files(dir_popup, full.names = TRUE, recursive = TRUE),
        compression = "gzip"
    )
    
    if (delete_dir)  unlink(dir_popup, recursive = TRUE)
}
