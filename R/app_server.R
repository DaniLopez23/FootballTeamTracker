#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  observeEvent(input$btn_expand, {
    session$sendCustomMessage("toggleFullscreen", list())
  })

  observeEvent(input$btn_help, {
    showModal(modalDialog(
      title = "Secciones de la aplicación",
      easyClose = TRUE,
      footer = modalButton("Cerrar"),
      tags$div(
        tags$h5("Equipo"),
        tags$p("Panel principal con información del club, KPIs, evolución de la clasificación, resultados, goles por jornada y reparto total de goles."),
        tags$h5("Comparador"),
        tags$p("Sección preparada para comparar equipos y métricas de forma paralela.")
      )
    ))
  })

  mod_overview_server("overview")
}
