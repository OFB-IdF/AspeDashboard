#' Title
#'
#' @param df 
#' @param var_espece 
#' @param var_abondance 
#' @param var_annee 
#' @param var_site 
#' @param var_proto 
#' @param sel_espece 
#' @param sel_site 
#'
#' @return
#' @export
#'
#' @importFrom dplyr select filter mutate distinct
#' @importFrom ggiraph geom_col_interactive geom_point_interactive girafe
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap vars scale_x_continuous theme_minimal theme element_blank element_text unit geom_line scale_shape_manual scale_y_continuous expansion labs element_rect
#' @importFrom patchwork plot_layout
#' @importFrom rlang enquo
#' @importFrom templatesOFB int_breaks int_limits
#' @importFrom tidyr complete nesting replace_na
gg_temp_ab_esp <- function(df, var_espece, var_abondance, var_annee, var_site, var_proto, sel_espece, sel_site, interactif = FALSE, largeur = 4, hauteur = 5) {
    int_limits2 <- function(x) {
        if (length(unique(x)) > 1) {
            range(x) + c(-.25, .25)
        }
        else {
            range(templatesOFB::int_breaks(x)) + c(-.25, .25)
        }
    }
    
    var_espece <- rlang::enquo(var_espece)
    var_abondance <- rlang::enquo(var_abondance)
    var_annee <- rlang::enquo(var_annee)
    var_site <- rlang::enquo(var_site)
    var_proto <- rlang::enquo(var_proto)
    
    data_esp <- df %>% 
        dplyr::select(
            site = !!var_site,
            annee = !!var_annee,
            protocole = !!var_proto,
            espece = !!var_espece,
            abondance = !!var_abondance
        ) %>% 
        dplyr::filter(site == sel_site) %>% 
        tidyr::complete(
            tidyr::nesting(site, annee, protocole),
            espece
        ) %>% 
        dplyr::filter(espece == sel_espece) %>% 
        dplyr::mutate(abondance = abondance %>% 
                          tidyr::replace_na(replace = 0),
                      lbl = paste0(sel_site, ": ", sel_espece),
                      hover = paste0(site, "<br>", annee, "<br>", espece, ": ", abondance))
    
    gg_ab <- data_esp %>% 
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = annee,
                y = abondance
            )
        ) +
        ggiraph::geom_col_interactive(
            width = .5, 
            mapping = ggplot2::aes(tooltip = hover)
            ) +
        ggplot2::geom_point(data = data_esp %>% 
                                dplyr::filter(abondance == 0),
                            shape = 4, size = 2, stroke = 2) + 
        ggplot2::facet_wrap(ggplot2::vars(lbl)) +
        ggplot2::scale_x_continuous(
            limits = templatesOFB::int_limits,
            breaks = templatesOFB::int_breaks
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(hjust = 1), 
            axis.text.x = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            plot.margin = ggplot2::unit(
                c(0, 0, -0.5, 0), "pt"
            )
        )
    
    data_proto <- data_esp %>%
        dplyr::select(annee, site, protocole) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(
            hover2 = paste0("<b>", annee, "</b><br>", protocole)
        )
    
    gg_proto <- ggplot2::ggplot(
        data = data_proto, 
        ggplot2::aes(
            x = annee, 
            fill = protocole
            )
        ) +
        ggplot2::geom_line(
            y = 0.5, group = 0,
            alpha = 0.5, lty = 1, size = 0.2
            ) + 
        ggiraph::geom_point_interactive(
            ggplot2::aes(
                x = annee, group = annee,
                fill = protocole, shape = protocole,
                tooltip = hover2
                ),
            y = 0.5, alpha = 0.7, size = 2
            ) + 
        ggplot2::scale_shape_manual(
            values = c(22, 23, 24, 25)
            ) +
        ggplot2::scale_x_continuous(
            position = "top", 
            breaks = templatesOFB::int_breaks,
            limits = int_limits2
            ) +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0))
            ) +
        ggplot2::labs(
            title = "",
            x = "",
            shape = "Protocole", fill = "Protocole"
            ) +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "grey95"), 
            strip.text = ggplot2::element_text(
                size = 11, 
                color = "white", 
                face = "bold"
                ),
            strip.background = ggplot2::element_rect(
                color = "black", 
                fill = "grey30"
                    ),
            legend.position = "bottom",
            legend.direction = "vertical",
            legend.justification = c("left"),
            legend.text = ggplot2::element_text(size = 9), 
            plot.title = ggplot2::element_text(size = 9),
            legend.title = ggplot2::element_text(size = 9), 
            axis.text.x = ggplot2::element_text(
                angle = 0, hjust = 0.5, 
                size = 9
                ), 
            axis.ticks = ggplot2::element_blank()
            )
    

    plot_comb <- (gg_ab / gg_proto) + 
        patchwork::plot_layout(heights = c(5, 0.2))
    
    if (interactif) {
        ggiraph::girafe(ggobj = plot_comb, width_svg = largeur, 
                        height_svg = hauteur, options = list())
    }
    else {
        plot_comb
    }
}

