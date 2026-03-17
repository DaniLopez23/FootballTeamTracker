#' players UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_players_ui <- function(id) {
  ns <- NS(id)

  kpi_card <- function(ico, titulo, value_output) {
    bslib::card(
      bslib::card_body(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; padding: 1rem;",
        tags$div(
          style = "display: flex; align-items: center; gap: 0.75rem;",
          tags$span(style = "font-size: 1.8rem; line-height: 1; flex-shrink: 0; color: #0d6efd;", ico),
          tags$div(
            tags$div(titulo,       style = "font-size: 0.75rem; font-weight: 600; text-transform: uppercase; color: #6c757d; line-height: 1;"),
            tags$div(value_output, style = "font-size: 0.95rem; font-weight: 700; margin-top: 0.15rem; color: #0d6efd; line-height: 1.2;")
          )
        )
      )
    )
  }

  tagList(
    # CSS cuadros de jornada (mismo que en overview)
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
    "),

    bslib::layout_sidebar(
      fill     = FALSE,
      fillable = FALSE,

      # ── Sidebar ──────────────────────────────────────────────────────────
      sidebar = bslib::sidebar(
        width = 230,
        open  = TRUE,

        tags$p("Localización", style = "font-weight:600; margin-bottom:0.25rem;"),
        shiny::radioButtons(
          ns("location"), NULL,
          choices  = c("Todo" = "all", "Local" = "local", "Visitante" = "visitante"),
          selected = "all"
        ),

        tags$hr(style = "margin: 0.5rem 0;"),

        tags$p("Jornadas", style = "font-weight:600; margin-bottom:0.5rem;"),
        uiOutput(ns("jornada_squares")),

        tags$hr(style = "margin: 0.5rem 0;"),

        tags$p("Vuelta", style = "font-weight:600; margin-bottom:0.25rem;"),
        shiny::radioButtons(
          ns("vuelta"), NULL,
          choices  = c("Todo" = "all", "1\u00aa Vuelta" = "first", "2\u00aa Vuelta" = "second"),
          selected = "all"
        )
      ),

      # ── Contenido principal ───────────────────────────────────────────────
      # Fila 0: KPIs de Jugadores
      bslib::layout_column_wrap(
        width = 1 / 5,
        heights_equal = "row",
        fill  = FALSE,
        class = "mb-3",
        kpi_card(icon("soccer-ball"), "Máximo Goleador", textOutput(ns("top_scorer"), inline = TRUE)),
        kpi_card(icon("exclamation"), "Más Tarjetas", textOutput(ns("top_cards"), inline = TRUE)),
        kpi_card(icon("hourglass-end"), "Más Minutos", textOutput(ns("top_minutes"), inline = TRUE)),
        kpi_card(icon("arrow-right-long"), "Más Revulsivo", textOutput(ns("top_revulsivo"), inline = TRUE)),
        kpi_card(icon("arrow-left-long"), "Más Cambiado", textOutput(ns("top_changed"), inline = TRUE))
      ),
      # Fila 1: Top 10 Goleadores + Top 10 Tarjetas
      bslib::layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        class = "mb-3",

        bslib::card(
          bslib::card_header("Top 10 Goleadores"),
          bslib::card_body(plotly::plotlyOutput(ns("plot_top_scorers"), height = "350px"))
        ),

        bslib::card(
          bslib::card_header("Top 10 Tarjetas"),
          bslib::card_body(plotly::plotlyOutput(ns("plot_top_cards"), height = "350px"))
        )
      ),

      # Fila 2: Distribución de minutos por jugador
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center justify-content-between flex-wrap gap-2",
          tags$span("Distribución de minutos por jugador"),
          shiny::selectInput(
            ns("minutos_mode"),
            NULL,
            choices = c(
              "Media de minutos por partido" = "media",
              "Total de minutos"             = "total",
              "Partidos completos"           = "completos",
              "Titular y sale"               = "titular_sale",
              "Suplente y entra"             = "suplente_entra"
            ),
            selected = "total",
            width = "280px"
          )
        ),
        bslib::card_body(
          class = "p-2",
          plotly::plotlyOutput(ns("plot_minutos"), height = "380px")
        )
      )
    )
  )
}

#' players Server Functions
#'
#' @noRd
mod_players_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Carga de datos de Eventos ---
    df_eventos <- readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Eventos")

    max_jornada <- if (nrow(df_eventos) > 0) max(df_eventos$Jornada) else 1

    # --- Estado de selección de jornadas ---
    rv <- shiny::reactiveValues(
      j_start          = 1L,
      j_end            = max_jornada,
      selected_jornadas = seq_len(max_jornada)
    )

    # --- Cuadros de jornada ---
    output$jornada_squares <- renderUI({
      sel <- rv$selected_jornadas
      squares <- lapply(seq_len(max_jornada), function(j) {
        active <- if (j %in% sel) "jornada-sq jornada-sq-active" else "jornada-sq"
        tags$div(
          class   = active,
          `data-j` = j,
          onclick  = paste0("Shiny.setInputValue('", ns("jornada_click"), "', ", j, ", {priority:'event'})"),
          j
        )
      })
      tags$div(style = "display:flex; flex-wrap:wrap; gap:4px;", squares)
    })

    # --- Lógica de selección de rango al hacer click ---
    observeEvent(input$jornada_click, {
      j <- as.integer(input$jornada_click)
      if (is.null(rv$j_start) || (!is.null(rv$j_start) && !is.null(rv$j_end))) {
        rv$j_start <- j
        rv$j_end   <- NULL
        rv$selected_jornadas <- j
      } else {
        start <- min(rv$j_start, j)
        end   <- max(rv$j_start, j)
        rv$j_start <- start
        rv$j_end   <- end
        rv$selected_jornadas <- seq(start, end)
      }
    })

    # --- Vuelta preset → actualiza cuadros ---
    observeEvent(input$vuelta, {
      sel <- switch(input$vuelta,
        "all"    = seq_len(max_jornada),
        "first"  = seq_len(min(12L, max_jornada)),
        "second" = if (max_jornada > 12L) seq(13L, max_jornada) else integer(0)
      )
      rv$j_start <- if (length(sel) > 0) min(sel) else NULL
      rv$j_end   <- if (length(sel) > 0) max(sel) else NULL
      rv$selected_jornadas <- sel
    })

    # --- Datos filtrados ---
    df_filtered <- reactive({
      d <- df_eventos

      # Filtro por equipo: solo nuestro equipo (EquipoLocalID == 1 OR EquipoVisitanteID == 1)
      d <- d[d$EquipoLocalID == 1 | d$EquipoVisitanteID == 1, ]

      if (isTRUE(input$location == "local")) {
        d <- d[d$EquipoLocalID == 1, ]
      } else if (isTRUE(input$location == "visitante")) {
        d <- d[d$EquipoVisitanteID == 1, ]
      }

      sel <- rv$selected_jornadas
      if (length(sel) > 0 && length(sel) < max_jornada) {
        d <- d[d$Jornada %in% sel, ]
      }

      d
    })

    # --- KPI: Máximo Goleador ---
    output$top_scorer <- renderText({
      d <- df_filtered()
      goles <- d[d$Tipo == "Gol" & d$Jugador != "Rival", ]
      if (nrow(goles) == 0) return("--")
      goles_count <- table(goles$Jugador)
      top_name <- names(goles_count)[which.max(goles_count)]
      paste0(top_name, " (", max(goles_count), ")")
    })

    # --- KPI: Jugador con más Tarjetas ---
    output$top_cards <- renderText({
      d <- df_filtered()
      tarjetas <- d[d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador != "Rival", ]
      if (nrow(tarjetas) == 0) return("--")
      tarjetas_count <- table(tarjetas$Jugador)
      top_name <- names(tarjetas_count)[which.max(tarjetas_count)]
      paste0(top_name, " (", max(tarjetas_count), ")")
    })

    # --- KPI: Total de Minutos ---
    output$top_minutes <- renderText({
      d <- df_filtered()
      d_clean <- d[!is.na(d$MinutosTotales) & d$Jugador != "Rival", ]
      if (nrow(d_clean) == 0) return("--")
      mins_por_jugador <- aggregate(MinutosTotales ~ Jugador, data = d_clean, FUN = sum)
      top_idx <- which.max(mins_por_jugador$MinutosTotales)
      top_name <- mins_por_jugador$Jugador[top_idx]
      top_mins <- mins_por_jugador$MinutosTotales[top_idx]
      paste0(top_name, " (", top_mins, "')")
    })

    # --- KPI: Jugador más Revulsivo (Titular=0, Sustituido=1) ---
    output$top_revulsivo <- renderText({
      d <- df_filtered()
      d_rev <- d[d$Titular == 0 & d$Sustituido == 1 & d$Jugador != "Rival", ]
      if (nrow(d_rev) == 0) return("--")
      rev_count <- table(d_rev$Jugador)
      top_name <- names(rev_count)[which.max(rev_count)]
      paste0(top_name, " (", max(rev_count), "x)")
    })

    # --- KPI: Jugador más Cambiado (Titular=1, Sustituido=1) ---
    output$top_changed <- renderText({
      d <- df_filtered()
      d_chg <- d[d$Titular == 1 & d$Sustituido == 1 & d$Jugador != "Rival", ]
      if (nrow(d_chg) == 0) return("--")
      chg_count <- table(d_chg$Jugador)
      top_name <- names(chg_count)[which.max(chg_count)]
      paste0(top_name, " (", max(chg_count), "x)")
    })

    # --- Gráfico: Top 10 Goleadores ---
    output$plot_top_scorers <- plotly::renderPlotly({
      d <- df_filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())

      # Filtrar solo goles y excluir Rival
      goles <- d[d$Tipo == "Gol" & d$Jugador != "Rival", ]
      if (nrow(goles) == 0) return(plotly::plotly_empty())

      # Contar goles por jugador
      goles_por_jugador <- as.data.frame(table(goles$Jugador))
      names(goles_por_jugador) <- c("Jugador", "Goles")
      goles_por_jugador$Goles <- as.numeric(goles_por_jugador$Goles)

      # Top 10
      goles_top <- goles_por_jugador[order(goles_por_jugador$Goles, decreasing = TRUE), ]
      goles_top <- head(goles_top, 10)
      goles_top$Jugador <- factor(goles_top$Jugador, levels = goles_top$Jugador[order(goles_top$Goles)])

      plotly::plot_ly(
        x = goles_top$Goles,
        y = goles_top$Jugador,
        type = "bar",
        orientation = "h",
        marker = list(color = "#198754"),
        text = goles_top$Goles,
        textposition = "outside",
        hoverinfo = "y+x"
      ) |>
        plotly::layout(
          xaxis = list(title = "Goles"),
          yaxis = list(title = "Jugador", tickpad = 15),
          margin = list(l = 280, t = 20),
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # --- Gráfico: Top 10 Tarjetas ---
    output$plot_top_cards <- plotly::renderPlotly({
      d <- df_filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())

      # Filtrar tarjetas (Amarilla y Roja) y excluir Rival
      tarjetas <- d[d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador != "Rival", ]
      if (nrow(tarjetas) == 0) return(plotly::plotly_empty())

      # Contar tarjetas por jugador
      tarjetas_por_jugador <- as.data.frame(table(tarjetas$Jugador))
      names(tarjetas_por_jugador) <- c("Jugador", "Tarjetas")
      tarjetas_por_jugador$Tarjetas <- as.numeric(tarjetas_por_jugador$Tarjetas)

      # Top 10
      tarjetas_top <- tarjetas_por_jugador[order(tarjetas_por_jugador$Tarjetas, decreasing = TRUE), ]
      tarjetas_top <- head(tarjetas_top, 10)
      tarjetas_top$Jugador <- factor(tarjetas_top$Jugador, levels = tarjetas_top$Jugador[order(tarjetas_top$Tarjetas)])

      plotly::plot_ly(
        x = tarjetas_top$Tarjetas,
        y = tarjetas_top$Jugador,
        type = "bar",
        orientation = "h",
        marker = list(color = "#dc3545"),
        text = tarjetas_top$Tarjetas,
        textposition = "outside",
        hoverinfo = "y+x"
      ) |>
        plotly::layout(
          xaxis = list(title = "Tarjetas"),
          yaxis = list(title = "Jugador", tickpad = 15),
          margin = list(l = 280, t = 20),
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # --- Gráfico: Distribución de minutos por jugador ---
    output$plot_minutos <- plotly::renderPlotly({
      d <- df_filtered()
      d <- d[d$Jugador != "Rival", ]
      if (nrow(d) == 0) return(plotly::plotly_empty())

      modo <- input$minutos_mode

      df_plot <- switch(modo,
        "total" = {
          d_mins <- d[!is.na(d$MinutosTotales), ]
          if (nrow(d_mins) == 0) return(plotly::plotly_empty())
          agg <- aggregate(MinutosTotales ~ Jugador, data = d_mins, FUN = sum)
          names(agg) <- c("Jugador", "Valor")
          agg
        },
        "media" = {
          d_mins <- d[!is.na(d$MinutosTotales), ]
          if (nrow(d_mins) == 0) return(plotly::plotly_empty())
          agg <- aggregate(MinutosTotales ~ Jugador, data = d_mins, FUN = mean)
          agg$MinutosTotales <- round(agg$MinutosTotales, 1)
          names(agg) <- c("Jugador", "Valor")
          agg
        },
        "completos" = {
          d_comp <- d[
            !is.na(d$Titular) & !is.na(d$Sustituido) & !is.na(d$MinutosTotales) &
            d$Titular == 1 & d$Sustituido == 0 & d$MinutosTotales == 90, ]
          if (nrow(d_comp) == 0) return(plotly::plotly_empty())
          cnt <- as.data.frame(table(d_comp$Jugador))
          names(cnt) <- c("Jugador", "Valor")
          cnt$Valor <- as.numeric(cnt$Valor)
          cnt
        },
        "titular_sale" = {
          d_ts <- d[
            !is.na(d$Titular) & !is.na(d$Sustituido) &
            d$Titular == 1 & d$Sustituido == 1, ]
          if (nrow(d_ts) == 0) return(plotly::plotly_empty())
          cnt <- as.data.frame(table(d_ts$Jugador))
          names(cnt) <- c("Jugador", "Valor")
          cnt$Valor <- as.numeric(cnt$Valor)
          cnt
        },
        "suplente_entra" = {
          d_se <- d[
            !is.na(d$Titular) & !is.na(d$Sustituido) &
            d$Titular == 0 & d$Sustituido == 1, ]
          if (nrow(d_se) == 0) return(plotly::plotly_empty())
          cnt <- as.data.frame(table(d_se$Jugador))
          names(cnt) <- c("Jugador", "Valor")
          cnt$Valor <- as.numeric(cnt$Valor)
          cnt
        }
      )

      df_plot <- df_plot[order(df_plot$Valor, decreasing = TRUE), ]
      df_plot$Jugador <- factor(df_plot$Jugador, levels = df_plot$Jugador)

  y_max <- max(df_plot$Valor, na.rm = TRUE) * 1.15

  y_title <- switch(modo,
        "total"          = "Minutos totales",
        "media"          = "Media de minutos",
        "completos"      = "Partidos completos (90 min)",
        "titular_sale"   = "Veces titular sustituido",
        "suplente_entra" = "Veces suplente entrando"
      )

      plotly::plot_ly(
        x    = df_plot$Jugador,
        y    = df_plot$Valor,
        type = "bar",
        marker      = list(color = "#0d6efd"),
        text        = df_plot$Valor,
        textposition = "outside",
        hoverinfo   = "x+y"
      ) |>
        plotly::layout(
          xaxis = list(title = "Jugador", tickangle = -45),
          yaxis = list(title = y_title, range = c(0, y_max)),
          margin = list(t = 30, b = 120),
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_players_ui("players_1")

## To be copied in the server
# mod_players_server("players_1")
