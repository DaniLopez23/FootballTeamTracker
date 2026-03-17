#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = tags$img(src = "www/AppLogo.png", height = "64px"),
      id = "main_nav",
      bslib::nav_panel(
        title = "Overview",
        value = "overview",
        mod_overview_ui("overview")
      ),
      bslib::nav_panel(
        title = "Comparador",
        value = "comparador",
        tags$p("Comparador content here.")
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        tags$div(
          class = "d-flex align-items-center gap-4",
          tags$button(
            id = "btn_help",
            class = "btn btn-link nav-link p-1",
            style = "font-size: 1.4rem;",
            title = "Ayuda",
            icon("circle-question")
          ),
          tags$button(
            id = "btn_lang",
            class = "btn btn-link nav-link p-1 d-flex align-items-center gap-1",
            style = "font-size: 1.4rem;",
            title = "Cambiar idioma",
            icon("language"),
            tags$span("EN", id = "lang_label", class = "small")
          ),
          tags$button(
            id = "btn_expand",
            class = "btn btn-link nav-link p-1",
            style = "font-size: 1.4rem;",
            title = "Pantalla completa",
            icon("expand")
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FootballTeamTracker"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
