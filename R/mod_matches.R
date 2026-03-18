#' matches UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_matches_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # CSS cuadros de jornada (reutilizado de overview)
    tags$style("
      .jornada-sq {
        width: 30px; height: 30px;
        display: inline-flex; align-items: center; justify-content: center;
        border: 1px solid #dee2e6; border-radius: 4px;
        cursor: pointer; font-size: 0.72rem; font-weight: 600;
        color: #495057; user-select: none;
        transition: background-color 0.15s, color 0.15s, border-color 0.15s;
      }
      .jornada-sq:hover:not(.jornada-sq-active) {
        background-color: #e7f0ff; border-color: #0d6efd;
      }
      .jornada-sq-active {
        background-color: #0d6efd; color: white; border-color: #0d6efd;
      }
      .timeline-container {
        position: relative;
        padding: 1.5rem 0;
      }
      .timeline-line {
        position: absolute;
        left: 50%;
        top: 0;
        bottom: 0;
        width: 2px;
        background: #dee2e6;
        transform: translateX(-50%);
      }
      .timeline-event {
        position: relative;
        display: flex;
        align-items: center;
        margin-bottom: 1.5rem;
      }
      .timeline-event.left {
        justify-content: center;
        padding-right: 0;
      }
      .timeline-event.right {
        justify-content: center;
        padding-left: 0;
      }
      .timeline-marker {
        position: absolute;
        left: 50%;
        transform: translateX(-50%);
        width: 12px;
        height: 12px;
        border-radius: 50%;
        background: white;
        border: 2px solid;
        z-index: 2;
      }
      .timeline-content {
        background: #f8f9fa;
        padding: 0.5rem 0.75rem;
        border-radius: 6px;
        border-left: 3px solid;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .timeline-minute {
        font-weight: 700;
        font-size: 0.75rem;
        color: #495057;
        margin-right: 0.5rem;
      }
      .timeline-text {
        font-size: 0.85rem;
        color: #212529;
      }
      .match-header-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }
      .match-info-item {
        padding: 0.75rem;
        background: rgba(255,255,255,0.1);
        border-radius: 6px;
        backdrop-filter: blur(10px);
      }
    "),

    # Fila 1: Filtros de jornada y selector de partido
    bslib::card(
      class = "mb-3",
      bslib::card_body(
        class = "p-3",
        bslib::layout_columns(
          col_widths = c(6, 6),
          tags$div(
            tags$p("Jornadas", style = "font-weight:600; margin-bottom:0.5rem; font-size:0.9rem;"),
            uiOutput(ns("jornada_squares"))
          ),
          tags$div(
            tags$p("Seleccionar partido", style = "font-weight:600; margin-bottom:0.5rem; font-size:0.9rem;"),
            shiny::selectInput(
              ns("partido_selector"),
              NULL,
              choices = NULL,
              width = "100%"
            )
          )
        )
      )
    ),

    # Fila 2: Información del partido
    bslib::card(
      class = "match-header-card mb-3",
      bslib::card_body(
        class = "p-4",
        uiOutput(ns("match_header"))
      )
    ),

    # Fila 3: Timelines
    bslib::layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,

      bslib::card(
        bslib::card_header("Timeline de Eventos (Goles, Tarjetas y Sustituciones)"),
        bslib::card_body(
          class = "p-3",
          style = "min-height: 400px; max-height: 600px; overflow-y: auto;",
          uiOutput(ns("timeline_eventos"))
        )
      ),

      bslib::card(
        bslib::card_header("Timeline de Cambios"),
        bslib::card_body(
          class = "p-3",
          style = "min-height: 400px; max-height: 600px; overflow-y: auto;",
          uiOutput(ns("timeline_cambios"))
        )
      )
    )
  )
}

#' matches Server Functions
#'
#' @noRd
mod_matches_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- Carga de datos ---
    df_partidos <- readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Partidos")
    df_eventos <- readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Eventos")

    # Filtrar solo partidos del equipo (ID 1)
    df_partidos <- df_partidos[df_partidos$ID_EquipoLocal == 1 | df_partidos$ID_EquipoVisitante == 1, ]
    df_partidos <- df_partidos[order(df_partidos$Jornada), ]

    max_jornada <- max(df_partidos$Jornada)

    # --- Estado reactivo ---
    rv <- shiny::reactiveValues(
      selected_jornada = 1L,
      initialized = FALSE
    )

    # --- Actualizar selector de partidos cuando hay datos (solo una vez) ---
    observe({
      if (!rv$initialized && nrow(df_partidos) > 0) {
        choices <- setNames(
          df_partidos$Jornada,
          sprintf("J%d: %s %d - %d %s",
                  df_partidos$Jornada,
                  ifelse(df_partidos$ID_EquipoLocal == 1, "Local", "Visitante"),
                  ifelse(df_partidos$ID_EquipoLocal == 1, df_partidos$GolesLocal, df_partidos$GolesVisitante),
                  ifelse(df_partidos$ID_EquipoLocal == 1, df_partidos$GolesVisitante, df_partidos$GolesLocal),
                  ifelse(df_partidos$ID_EquipoLocal == 1, "Visitante", "Local"))
        )
        updateSelectInput(session, "partido_selector", choices = choices, selected = rv$selected_jornada)
        rv$initialized <- TRUE
      }
    })

    # --- Cuadros de jornada ---
    output$jornada_squares <- renderUI({
      sel <- rv$selected_jornada
      squares <- lapply(seq_len(max_jornada), function(j) {
        active <- if (j == sel) "jornada-sq jornada-sq-active" else "jornada-sq"
        tags$div(
          class   = active,
          `data-j` = j,
          onclick  = paste0("Shiny.setInputValue('", ns("jornada_click"), "', ", j, ", {priority:'event'})"),
          j
        )
      })
      tags$div(style = "display:flex; flex-wrap:wrap; gap:4px;", squares)
    })

    # --- Click en cuadrado de jornada → actualiza selector ---
    observeEvent(input$jornada_click, {
      j <- as.integer(input$jornada_click)
      if (!identical(rv$selected_jornada, j)) {
        rv$selected_jornada <- j
        updateSelectInput(session, "partido_selector", selected = j)
      }
    })

    # --- Cambio en selector → actualiza jornada seleccionada ---
    observeEvent(input$partido_selector, {
      if (!is.null(input$partido_selector) && nchar(input$partido_selector) > 0) {
        j <- as.integer(input$partido_selector)
        if (!identical(rv$selected_jornada, j)) {
          rv$selected_jornada <- j
        }
      }
    }, ignoreInit = TRUE)

    # --- Partido seleccionado ---
    partido_actual <- reactive({
      req(rv$selected_jornada)
      df_partidos[df_partidos$Jornada == rv$selected_jornada, ]
    })

    # --- Eventos del partido actual ---
    eventos_actual <- reactive({
      req(rv$selected_jornada)
      df_eventos[df_eventos$Jornada == rv$selected_jornada, ]
    })

    # --- Header del partido ---
    output$match_header <- renderUI({
      p <- partido_actual()
      if (nrow(p) == 0) return(tags$p("No hay datos disponibles", class = "text-center"))

      # Determinar si es local o visitante
      es_local <- p$ID_EquipoLocal == 1
      equipo_nuestro <- "C.D Cazalegas - Ebora Formación"
      equipo_rival <- "Rival"
      goles_nuestros <- if (es_local) p$GolesLocal else p$GolesVisitante
      goles_rival <- if (es_local) p$GolesVisitante else p$GolesLocal

      # Resultado
      resultado <- if (goles_nuestros > goles_rival) {
        "Victoria"
      } else if (goles_nuestros < goles_rival) {
        "Derrota"
      } else {
        "Empate"
      }

      resultado_color <- if (goles_nuestros > goles_rival) {
        "success"
      } else if (goles_nuestros < goles_rival) {
        "danger"
      } else {
        "warning"
      }

      # Manejar fecha y hora que pueden no existir
      fecha_texto <- if ("Fecha" %in% names(p) && !is.na(p$Fecha)) {
        format(p$Fecha, "%d/%m/%Y")
      } else {
        "Fecha no disponible"
      }

      hora_texto <- if ("Hora" %in% names(p) && !is.na(p$Hora)) {
        p$Hora
      } else {
        NULL
      }

      tags$div(
        # Fecha y jornada
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-3",
          tags$div(
            tags$h5(paste0("Jornada ", p$Jornada), class = "mb-0"),
            tags$div(
              class = "mt-1",
              icon("calendar-days"),
              tags$span(fecha_texto, class = "ms-2"),
              if (!is.null(hora_texto)) {
                tags$span(
                  class = "ms-3",
                  icon("clock"),
                  tags$span(hora_texto, class = "ms-2")
                )
              }
            )
          ),
          tags$span(
            class = paste0("badge bg-", resultado_color, " fs-6 px-3 py-2"),
            resultado
          )
        ),

        # Resultado del partido
        tags$div(
          class = "d-flex justify-content-center align-items-center gap-4 py-3",
          tags$div(
            class = "text-center",
            tags$h4(equipo_nuestro, class = "mb-0 fw-bold"),
            tags$small(if (es_local) "Local" else "Visitante", class = "opacity-75")
          ),
          tags$div(
            class = "text-center px-4",
            tags$h1(
              paste0(goles_nuestros, " - ", goles_rival),
              class = "mb-0 fw-bold",
              style = "font-size: 3rem;"
            )
          ),
          tags$div(
            class = "text-center",
            tags$h4(equipo_rival, class = "mb-0 fw-bold"),
            tags$small(if (es_local) "Visitante" else "Local", class = "opacity-75")
          )
        )
      )
    })

    # --- Timeline de eventos (goles, tarjetas y sustituciones) ---
    output$timeline_eventos <- renderUI({
      ev <- eventos_actual()
      p <- partido_actual()

      if (nrow(ev) == 0 || nrow(p) == 0) {
        return(tags$div(
          class = "text-center text-muted py-5",
          icon("circle-info", class = "fa-2x mb-2"),
          tags$p("No hay eventos registrados para este partido")
        ))
      }

      # Crear lista de eventos combinando goles, tarjetas y sustituciones
      eventos_lista <- list()

      # 1. Añadir goles y tarjetas
      ev_goles_tarjetas <- ev[ev$Tipo %in% c("Gol", "Tarjeta Amarilla", "Tarjeta Roja"), ]
      if (nrow(ev_goles_tarjetas) > 0) {
        for (i in seq_len(nrow(ev_goles_tarjetas))) {
          evento <- ev_goles_tarjetas[i, ]
          eventos_lista[[length(eventos_lista) + 1]] <- list(
            minuto = evento$Minuto,
            tipo = evento$Tipo,
            jugador = evento$Jugador,
            es_cambio = FALSE
          )
        }
      }

      # 2. Añadir sustituciones
      cambios <- ev[!is.na(ev$Sustituido) & ev$Sustituido == 1 & ev$Jugador != "Rival", ]

      # Verificar si existe columna de minuto de cambio
      columna_minuto <- if ("MinutoCambio" %in% names(cambios)) {
        "MinutoCambio"
      } else if ("Minuto" %in% names(cambios)) {
        "Minuto"
      } else {
        NULL
      }

      if (!is.null(columna_minuto) && nrow(cambios) > 0) {
        cambios <- cambios[!is.na(cambios[[columna_minuto]]), ]

        if (nrow(cambios) > 0) {
          for (i in seq_len(nrow(cambios))) {
            cambio <- cambios[i, ]
            eventos_lista[[length(eventos_lista) + 1]] <- list(
              minuto = cambio[[columna_minuto]],
              tipo = if (cambio$Titular == 1) "Salida" else "Entrada",
              jugador = cambio$Jugador,
              es_cambio = TRUE
            )
          }
        }
      }

      if (length(eventos_lista) == 0) {
        return(tags$div(
          class = "text-center text-muted py-5",
          icon("circle-info", class = "fa-2x mb-2"),
          tags$p("No hay eventos registrados para este partido")
        ))
      }

      # Ordenar todos los eventos por minuto
      eventos_lista <- eventos_lista[order(sapply(eventos_lista, function(x) x$minuto))]

      # Generar HTML para todos los eventos
      eventos_html <- lapply(eventos_lista, function(evento) {
        es_nuestro <- evento$jugador != "Rival"

        # Color y icono según tipo de evento
        if (evento$tipo == "Gol") {
          color <- "#198754"
          icono <- icon("futbol")
          label <- "Gol"
        } else if (evento$tipo == "Tarjeta Amarilla") {
          color <- "#ffc107"
          icono <- icon("square", style = "color: #ffc107;")
          label <- "Tarjeta Amarilla"
        } else if (evento$tipo == "Tarjeta Roja") {
          color <- "#dc3545"
          icono <- icon("square", style = "color: #dc3545;")
          label <- "Tarjeta Roja"
        } else if (evento$tipo == "Salida") {
          color <- "#dc3545"
          icono <- icon("arrow-right")
          label <- "Sale"
        } else {  # Entrada
          color <- "#198754"
          icono <- icon("arrow-left")
          label <- "Entra"
        }

        tags$div(
          class = "timeline-event left",
          tags$div(
            class = "timeline-marker",
            style = paste0("border-color: ", color, ";")
          ),
          tags$div(
            class = "timeline-content",
            style = paste0("border-left-color: ", color, ";"),
            tags$span(class = "timeline-minute", paste0(evento$minuto, "'")),
            icono,
            tags$span(
              class = "timeline-text ms-2",
              if (es_nuestro) evento$jugador else "Rival",
              " - ", label
            )
          )
        )
      })

      tags$div(
        class = "timeline-container",
        tags$div(class = "timeline-line"),
        eventos_html
      )
    })

    # --- Timeline de cambios ---
    output$timeline_cambios <- renderUI({
      ev <- eventos_actual()
      p <- partido_actual()

      if (nrow(ev) == 0 || nrow(p) == 0) {
        return(tags$div(
          class = "text-center text-muted py-5",
          icon("circle-info", class = "fa-2x mb-2"),
          tags$p("No hay cambios registrados para este partido")
        ))
      }

      # Identificar cambios: jugadores con Sustituido == 1
      # Los que son Titular==1 y Sustituido==1 → salieron
      # Los que son Titular==0 y Sustituido==1 → entraron
      # Filtrar primero por las columnas que existen
      cambios <- ev[!is.na(ev$Sustituido) & ev$Sustituido == 1 & ev$Jugador != "Rival", ]

      # Si existe MinutoCambio, usarla; si no, filtrar solo los que tienen Minuto
      if ("MinutoCambio" %in% names(cambios)) {
        cambios <- cambios[!is.na(cambios$MinutoCambio), ]
      } else {
        # Si no existe MinutoCambio, asumir que no hay info de cambios
        cambios <- cambios[FALSE, ]  # data frame vacío
      }

      if (nrow(cambios) == 0) {
        return(tags$div(
          class = "text-center text-muted py-5",
          icon("circle-info", class = "fa-2x mb-2"),
          tags$p("No hay cambios registrados para este partido")
        ))
      }

      # Ordenar por minuto de cambio
      cambios <- cambios[order(cambios$MinutoCambio), ]

      cambios_html <- lapply(seq_len(nrow(cambios)), function(i) {
        cambio <- cambios[i, ]
        es_titular <- cambio$Titular == 1
        lado <- if (i %% 2 == 1) "left" else "right"

        color <- if (es_titular) "#dc3545" else "#198754"  # Rojo=sale, Verde=entra
        icono <- if (es_titular) icon("arrow-right") else icon("arrow-left")
        texto <- if (es_titular) "Sale" else "Entra"

        tags$div(
          class = paste("timeline-event", lado),
          tags$div(
            class = "timeline-marker",
            style = paste0("border-color: ", color, ";")
          ),
          tags$div(
            class = "timeline-content",
            style = paste0("border-left-color: ", color, ";"),
            tags$span(class = "timeline-minute", paste0(cambio$MinutoCambio, "'")),
            icono,
            tags$span(class = "timeline-text ms-2", cambio$Jugador, " - ", texto)
          )
        )
      })

      tags$div(
        class = "timeline-container",
        tags$div(class = "timeline-line"),
        cambios_html
      )
    })
  })
}

## To be copied in the UI
# mod_matches_ui("matches_1")

## To be copied in the server
# mod_matches_server("matches_1")
