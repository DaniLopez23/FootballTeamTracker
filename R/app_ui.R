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
        title = "Equipo",
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
          class = "d-flex align-items-center header-controls",
          actionButton(
            inputId = "btn_help",
            label = NULL,
            icon = icon("circle-question"),
            class = "btn btn-link nav-link p-1 action-button",
            style = "font-size: 1.4rem;",
            title = "Ayuda"
          ),
          tags$div(
            class = "d-flex align-items-center gap-2 lang-switch",
            tags$span("ES", class = "small"),
            bslib::input_switch(
              id = "lang_switch",
              label = NULL,
              value = FALSE,
              width = "44px"
            ),
            tags$span("EN", class = "small")
          ),
          actionButton(
            inputId = "btn_expand",
            label = NULL,
            icon = icon("expand"),
            class = "btn btn-link nav-link p-1 action-button",
            style = "font-size: 1.4rem;",
            title = "Pantalla completa"
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
    ),
    tags$style(HTML("\
      .header-controls {\
        gap: 1.25rem;\
        padding-right: 0.75rem;\
      }\
      .header-controls .action-button {\
        margin: 0;\
      }\
      .lang-switch {\
        line-height: 1;\
      }\
      .lang-switch .shiny-input-container,\
      .lang-switch .form-group,\
      .lang-switch .bslib-input-switch {\
        margin: 0;\
      }\
      .lang-switch .form-check {\
        margin-bottom: 0;\
        display: flex;\
        align-items: center;\
      }\
    ")),
    tags$script(HTML("\
      Shiny.addCustomMessageHandler('toggleFullscreen', function(_) {\
        var root = document.documentElement;\
        if (!document.fullscreenElement) {\
          if (root.requestFullscreen) root.requestFullscreen();\
        } else {\
          if (document.exitFullscreen) document.exitFullscreen();\
        }\
      });\
    "))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
