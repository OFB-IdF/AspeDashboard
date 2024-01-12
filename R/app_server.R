#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
    
    if (!dir.exists("inst/app/www/widgets/especes"))
        utils::untar("inst/app/www/widgets/especes.tar")
    
    if (!dir.exists("inst/app/www/widgets/ipr"))
        utils::untar("inst/app/www/widgets/ipr.tar")
  
    
    SelectionBassin <- mod_selecteur_bassin_server(
        id = "bassin"
    )
    SelectionDepartement <- mod_selecteur_admin_server(
        id = "admin",
        bassin = SelectionBassin
    )
    SelectionVariable <- mod_selecteur_variable_server(
        id = "var"
        )
    SelectionEspece <- mod_selecteur_espece_server(
        id = "espece",
        variable = SelectionVariable, 
        bassin = SelectionBassin, departements = SelectionDepartement
    )

    SelectionPoint <- mod_carte_op_server(
      id = "carte_op",
      departement = SelectionDepartement,
      bassin = SelectionBassin,
      variable = SelectionVariable,
      espece = SelectionEspece
  )

    mod_panneau_droit_server(
        id = "panneau_droit",
        variable = SelectionVariable,
        departement = SelectionDepartement,
        bassin = SelectionBassin,
        point = SelectionPoint,
        espece = SelectionEspece
    )
    
    output$date_export <- renderText(paste0("Export: ", as.character(date_export)))

    # onSessionEnded(fun = function() {
    #     unlink("inst/app/www/widgets/especes", recursive = TRUE)
    #     unlink("inst/app/www/widgets/ipr", recursive = TRUE)
    # })
    
}
