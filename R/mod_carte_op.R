#' carte_op UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom leaflet leafletOutput
mod_carte_op_ui <- function(id){
  ns <- NS(id)
  
  css <- HTML(
      paste0(
          paste0("#", ns("carte_op"), " {height: calc(100vh - 200px) !important;}"),
          ".search-station {
            position: absolute;
            top: 0px;
            right: 20px;
          }
          
          .reset-station {
            position: absolute;
            bottom: 10px;
            right: 0px;
          }
          
           .leaflet {
                margin-top:0px;
           }

           .leaflet-control-zoom, .leaflet-top, .leaflet-bottom {
           z-index: unset !important;
           }

           .leaflet-touch .leaflet-control-layers .leaflet-control-zoom .leaflet-touch .leaflet-bar {
           z-index: 10000000000 !important;
           }
          "
      )
  )
  
  tagList(
      tags$head(
          tags$style(css)
      ),
    column(
        width = 12,
        tags$div(
            class = "search-station",
            selectizeInput(
                inputId = ns("station"),
                label = "",
                choices = c(
                    "Localiser un point de prélèvement" = ""
                ),
                multiple = FALSE
            )
        ),
        tags$div(
            class = "reset-station",
            actionButton(
                inputId = ns("reset"),
                label = "Désélectionner station"
            )
        ),
        leaflet::leafletOutput(
            ns("carte_op"),
            width = '100%'
        ),
        plotOutput(
            ns("legende"),
            height = "100px"
        )  
    )
  )
}
    
#' carte_op Server Functions
#' @noRd 
#' @importFrom dplyr filter
#' @importFrom leaflet renderLeaflet leaflet addTiles leafletProxy clearMarkers addCircleMarkers fitBounds
#' @importFrom sf st_bbox
#' @importFrom shiny HTML
#' @importFrom tidyr drop_na
mod_carte_op_server <- function(id, departement, bassin, variable, espece){
  moduleServer(
    id, 
    function(input, output, session){
    ns <- session$ns
    
    SelectionPoint <- reactiveValues(clickedMarker=NULL)
    
     radius_pal <- function(x) {
        approx(
            x = sqrt(range(carte_operations$nb_annees, na.rm = TRUE)),
            y = c(3, 10),
            xout = sqrt(x),
            yleft = 3,
            yright = 10
        )$y
     }
     
   BboxMap <- sf::st_bbox(carte_operations)
    
    output$carte_op <- leaflet::renderLeaflet(
        leaflet::leaflet() %>% 
            leaflet::addTiles(map = .) %>% 
        leaflet::fitBounds(
            map = .,
            lng1 = BboxMap[["xmin"]],
            lat1 = BboxMap[["ymin"]],
            lng2 = BboxMap[["xmax"]],
            lat2 = BboxMap[["ymax"]]
        )
    )

    output$legende <- renderPlot({
        req(variable)
        
        switch(
            variable(),
            especes = LegendeEspeces,
            ipr = LegendeIpr,
            distribution = LegendeDistribution
        )
    }
    )
    
    observe({
        req(departement, bassin, variable, espece)
        
        ChoixEspece <- ifelse(
            variable() != "distribution" | is.null(espece()), "", espece()
        )
        
        DataMap <- carte_operations %>% 
            dplyr::mutate(
                esp_code_alternatif = stringr::str_replace_na(
                    esp_code_alternatif, ""
                )
            ) %>%
            dplyr::filter(
                dept_id %in% departement(),
                dh_libelle %in% bassin(),
                variable == variable(),
                esp_code_alternatif == ChoixEspece
                ) %>% 
            tidyr::drop_na(nb_annees, variable, valeur, couleur, opacite)
        
        updateSelectizeInput(
            session = session,
            inputId = "station",
            choices = c(
                "Localiser un point de prélèvement" = "",
                pop_geo %>% 
                    sf::st_drop_geometry() %>% 
                    dplyr::filter(
                        dept_id %in% departement(),
                        dh_libelle %in% bassin()
                        ) %>% 
                    dplyr::distinct(pop_libelle) %>% 
                    dplyr::arrange(pop_libelle) %>% 
                    dplyr::pull(pop_libelle)
            ),
            server = TRUE
        )
        
        BboxMap <- sf::st_bbox(DataMap)
        
        leaflet::leafletProxy("carte_op") %>%
            leaflet::fitBounds(
                map = .,
                lng1 = BboxMap[["xmin"]],
                lat1 = BboxMap[["ymin"]],
                lng2 = BboxMap[["xmax"]],
                lat2 = BboxMap[["ymax"]]
            )
        
        popups <- switch(
            variable(),
            especes = unname(popups_especes$popups[DataMap$pop_id]),
            ipr = unname(popups_ipr$popups[DataMap$pop_id]),
            distribution = NULL
        )
        
        if (nrow(DataMap) == 0) {
            SelectionPoint$clickedMarker <- NULL
            
            leaflet::leafletProxy("carte_op") %>%
                leaflet::clearMarkers(map = .)
        } else {
            
            leaflet::leafletProxy("carte_op") %>%
            leaflet::clearMarkers(map = .) %>%
            leaflet::addCircleMarkers(
                map = .,
                data = DataMap,
                layerId = ~pop_id,
                radius = ~radius_pal(nb_annees),
                fillColor = ~identity(couleur),
                stroke = TRUE,
                color = "black",
                weight = 2,
                opacity = ~identity(opacite),
                fillOpacity = .75,
                label = ~lapply(hover, shiny::HTML),
                popup = popups
            ) 
        }
        
        observe({

            if (input$station != "") {

                CoordsStation <- pop_geo %>% 
                    dplyr::filter(pop_libelle == input$station) %>% 
                    dplyr::summarise() %>% 
                    sf::st_centroid() %>% 
                    sf::st_transform(crs = 4326) %>% 
                    sf::st_coordinates()
            
            leaflet::leafletProxy("carte_op") %>% 
                leaflet::setView(
                    lng = CoordsStation[,"X"],
                    lat = CoordsStation[,"Y"],
                    zoom = 15
                )
            } else {

                leaflet::leafletProxy("carte_op") %>%
                    leaflet::fitBounds(
                        map = .,
                        lng1 = BboxMap[["xmin"]],
                        lat1 = BboxMap[["ymin"]],
                        lng2 = BboxMap[["xmax"]],
                        lat2 = BboxMap[["ymax"]]
                    )
            }
            
        })
    })
    
    # observe the marker click info and print to console when it is changed.
    observeEvent(input$carte_op_marker_click,{
        SelectionPoint$clickedMarker <- input$carte_op_marker_click$id
    })
    
    observeEvent(input$reset, {
        SelectionPoint$clickedMarker <- NULL
    })
    
    reactive({
        SelectionPoint$clickedMarker
        })
  })
}
    
## To be copied in the UI
# mod_carte_op_ui("carte_op_ui_1")
    
## To be copied in the server
# mod_carte_op_server("carte_op_ui_1")
