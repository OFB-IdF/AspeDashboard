#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      div(
        h1(
          class = "TitreAppli",
          "Exploitation des données de pêche électrique"
          ),
        img(
          src = knitr::image_uri(
          app_sys("app/www/logo.png")
          ), 
          alt = 'logo', 
          style = 'position:absolute; bottom:0; right:0; padding:10px; width:200px;'
          ),
        img(
          src = knitr::image_uri(
            app_sys("app/www/filigrane.png")
          ),
          alt = "filigrane",
          style = 'position:absolute; bottom:0; right:0; padding:0px; width:800px;color:#99D7F7'
          )
        ),
      div(
          textOutput("date_export"),
          style = "position:absolute; top:30; left:10;"
      ),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 2,
          mod_selecteur_bassin_ui("bassin"),
          mod_selecteur_admin_ui("admin"),
          mod_selecteur_variable_ui("var"),
          mod_selecteur_espece_ui("espece")
        ),
        mainPanel = mainPanel(
          width = 10,
          tabsetPanel(
            tabPanel(
              title = "Synthèse des opérations",
              fluidRow(
                  column(
                      width = 7,
                      mod_carte_op_ui(
                          id = "carte_op"
                      )
                  ),
                  column(
                      width = 5,
                      mod_panneau_droit_ui(id = "panneau_droit")
                  )
              )
            ),
            # tabPanel(
            #   title = "Bilan détaillé"
            #   ),
            tabPanel(
              title = p(
                class = "TabMethode",
                "Données & Traitements"
                ),
              tags$iframe(
                  src = "www/MethodesDonnees.html",
                  style = 'width:100%; height:calc(100vh - 200px); overflow:hidden; border:none'
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(
      ico = "favicon",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'AspeDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

