# library(AspeDashboard)
devtools::load_all()
load("../data/processed/DONNEES PRETRAITEES/captures.RData")
load("../data/processed/DONNEES PRETRAITEES/pop_geo.RData")

DirPopups <- "inst/app/www/widgets"
largeur_popup <- 4
hauteur_popup <- 5

PreparationCarteOp <- prep_donnees_carte_op(
    captures = captures,
    localisations = pop_geo,
    # id_points = c("3157", "38542"),
    # id_points = sample(unique(captures$pop_id), 5),
    interactif = TRUE,
    largeur = largeur_popup,
    hauteur = hauteur_popup,
    rescale = TRUE,
    width = .96
)

save(PreparationCarteOp, file = "dev/PreparationCarteOp.rda")

carte_operations <-  PreparationCarteOp$data
graphiques <- PreparationCarteOp$graphiques 

chemin_widgets <- prep_sauver_widgets(
    widgets = graphiques, 
    widget_dir = DirPopups,
    lib_dir = "js",
    self_contained = FALSE
)

prep_remplacer_texte_fichiers(
    dir = DirPopups,
    ext = "html",
    pattern = "\\\"padding\\\":40,\\\"fill\\\":false",
    replacement = "\\\"padding\\\":0,\\\"fill\\\":false"
)

prep_remplacer_texte_fichiers(
    dir = DirPopups,
    ext = "html",
    pattern = "\\\"padding\\\":15,\\\"fill\\\":true",
    replacement = "\\\"padding\\\":0,\\\"fill\\\":true"
)

popups_list <- prep_popups(
    chemin_widgets = chemin_widgets,
    dir_widgets = DirPopups,
    largeur = largeur_popup,
    hauteur = hauteur_popup
)

popups <- popups_list$popups

usethis::use_data(carte_operations, overwrite = TRUE)
usethis::use_data(popups, overwrite = TRUE)

file.copy(
    from = "inst/app/www/style.css.bkp",
    to = "inst/app/www/style.css",
    overwrite = TRUE
)

cat(
    popups_list$css,
    file = "inst/app/www/style.css",
    append = TRUE
    )
