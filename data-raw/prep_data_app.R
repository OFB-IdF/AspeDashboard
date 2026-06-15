install.packages("pak")

pak::pkg_install(c(
    "OFB-IdF/templatesOFB",
    "OFB-IdF/AspeDashboard",
    "OFB-IdF/AspeDashboardData",
    "MaelTheuliere/COGiter",
    "PascalIrz/aspe",
    "karthik/rdrop2",
    "OFB-IdF/templatesOFB"
))

download_sandre <- FALSE
download_hubeau <- TRUE
maj <- TRUE

if (download_sandre) {
    AspeDashboardData::get_data_sandre("data_sandre.rda")
}

if (download_hubeau) {
    dept_test <- NULL # choisir un petit département à des fins de test (e.g. 93)
    if (file.exists("inst/app/data/metadata.rda")) {
        load("inst/app/data/metadata.rda")
    } else {
        date_export = NULL
    }
    
    codes_stations <- AspeDashboardData::get_data_hubeau(code_departement = dept_test, data_file = "data-raw/data_hubeau.rda", last_export = date_export)
    
    if (length(codes_stations) == 0) maj <- FALSE
}

if (maj) {
    AspeDashboardData::prep_data_dashboard("data-raw/data_sandre.rda", "data-raw/data_hubeau.rda", data_dashboard = "inst/app/data", draw_legend = FALSE)
    
    # rsconnect::setAccountInfo(
    #     name = Sys.getenv("RSCONNECT_USER"),
    #     token = Sys.getenv("RSCONNECT_TOKEN"),
    #     secret = Sys.getenv("RSCONNECT_SECRET")
    # )
    # rsconnect::deployApp(
    #     appName = "AspeDashboard",
    #     appTitle = "AspeDashboard",
    #     server = "shinyapps.io",
    #     forceUpdate = TRUE
    # )
}


