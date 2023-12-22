
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AspeDashboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

Ce package constitue le backoffice de l’appli de l’exploitation des
données de pêche électrique
(<https://ofbidf.shinyapps.io/AspeDashboard/>). La préparation des
données nécessaire (décrite dans le fichier
[`data-raw/PreparationDonnees.qmd`](data-raw/PreparationDonnees.md))
repose largement sur le package
[{aspe}](https://github.com/PascalIrz/aspe).

# Installation

``` r
if (!require(remotes))
    install.packages("remotes")

remotes::install_gitlab(
    repo = "cedric.mondy1/aspedashboard",
    host = "https://gitlab.ofb.fr", 
    dependencies = TRUE
    )
```

# Déploiement

Pour déployer l’application sur shinyapps, il convient de :

1.  préparer les données en utilisant le script
    `data-raw/PreparationDonnees.qmd`

2.  ne conserver dans le dossier `inst/app/www/widgets` que les archives
    `ipr.tar` et `especes.tar` et supprimer éventuellement les dossiers
    `ipr` et `especes`

3.  exclure du déploiement les dossiers `data-raw` et `dev` qui sont
    volumineux et ne sont nécessaires que pour la préparation des
    données, pas pour le fonctionnement de l’appli.
