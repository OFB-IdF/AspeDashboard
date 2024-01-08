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
                # choices = c(
                #     "Localiser une station" = "",
                #     sort(unique(pop_geo$pop_libelle))
                # ),
                choices = c(
                    "Localiser un point de prélèvement" = ""
                ),
                multiple = FALSE
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
        
        if(variable() == "ipr") {
            LegendeIpr
        } else {
            LegendeEspeces
        }
    }
    )
    
    observe({
        req(departement, bassin)
        
        DataMap <- carte_operations %>%
            dplyr::filter(
                dept_id %in% departement(),
                dh_libelle %in% bassin()
                )
        
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
        
        observe({
            req(variable)
            
            DonneesVariables <- DataMap %>% 
                dplyr::filter(variable == variable()) %>% 
                tidyr::drop_na(nb_annees, variable, valeur, couleur, opacite)
            
            if (variable() != "distribution")
                popups <- get(paste0("popups_", variable()))
            
            if (nrow(DonneesVariables) == 0) {
                leaflet::leafletProxy("carte_op") %>%
                    leaflet::clearMarkers(map = .)
            } else {
                leaflet::leafletProxy("carte_op") %>%
                    leaflet::clearMarkers(map = .) %>%
                    leaflet::addCircleMarkers(
                        map = .,
                        data = DonneesVariables,
                        layerId = ~pop_id,
                        radius = ~radius_pal(nb_annees),
                        fillColor = ~identity(couleur),
                        stroke = TRUE,
                        color = "black",
                        weight = 2,
                        opacity = ~identity(opacite),
                        fillOpacity = .75,
                        label = ~lapply(hover, shiny::HTML),
                        popup = unname(popups$popups[DonneesVariables$pop_id])
                    ) 
            }
        })
        
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
    

    
    SelectionPoint <- reactiveValues(clickedMarker=NULL)
    
    # observe the marker click info and print to console when it is changed.
    observeEvent(input$carte_op_marker_click,{
        SelectionPoint$clickedMarker <- input$carte_op_marker_click$id
    })
    
    observeEvent(input$carte_op_click,{
        SelectionPoint$clickedMarker <- NULL
        })
    
    reactive(SelectionPoint$clickedMarker)
  })
}
    
## To be copied in the UI
# mod_carte_op_ui("carte_op_ui_1")
    
## To be copied in the server
# mod_carte_op_server("carte_op_ui_1")
