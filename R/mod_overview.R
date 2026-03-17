#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id) {
  ns <- NS(id)

  kpi_card <- function(ico, titulo, value_output, delta_output) {
    bslib::card(
      bslib::card_body(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; padding: 1rem;",
        tags$div(
          style = "display: flex; align-items: center; gap: 0.75rem;",
          tags$span(style = "font-size: 2.2rem; line-height: 1; flex-shrink: 0; color: #0d6efd;", ico),
          tags$div(
            tags$div(titulo,       style = "font-size: 0.75rem; font-weight: 600; text-transform: uppercase; color: #6c757d; line-height: 1;"),
            tags$div(value_output, style = "font-size: 2.2rem; font-weight: 700; margin-top: 0.15rem; color: #0d6efd; line-height: 1;")
          )
        ),
        tags$div(style = "margin-top: 0.5rem;", delta_output)
      )
    )
  }

  tagList(
    # CSS cuadros de jornada
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

      # Fila 1: Info equipo + KPIs
      bslib::layout_columns(
        col_widths = c(5, 7),
        fill = FALSE,

        tags$div(
          class = "d-flex align-items-center gap-4 py-2",
          tags$img(src = "www/CazalegasEscudo.png", style = "height: 140px;"),
          tags$div(
            tags$h4(textOutput(ns("club_name"), inline = TRUE), class = "mb-2 fw-bold"),
            tags$div(
              class = "d-flex align-items-center gap-2 mb-1 text-muted",
              icon("calendar-days"),
              tags$span("Temporada"),
              tags$strong(textOutput(ns("season"), inline = TRUE))
            ),
            tags$div(
              class = "d-flex align-items-center gap-2 text-muted",
              icon("user-tie"),
              tags$span("Entrenador:"),
              tags$strong(textOutput(ns("coach"), inline = TRUE))
            )
          )
        ),

        bslib::layout_column_wrap(
          width = 1 / 3,
          heights_equal = "row",
          kpi_card(icon("trophy"), "Puesto",     textOutput(ns("rank"),      inline = TRUE), uiOutput(ns("rank_delta"))),
          kpi_card(icon("star"),   "Puntos",     textOutput(ns("points"),    inline = TRUE), uiOutput(ns("points_delta"))),
          kpi_card(icon("futbol"), "Dif. Goles", textOutput(ns("goal_diff"), inline = TRUE), uiOutput(ns("goal_diff_delta")))
        )
      ),

      # Fila 2: Clasificación + Resultados
      bslib::layout_columns(
        col_widths = c(8, 4),
        fill = FALSE,
        class = "mt-3",
        bslib::card(
          bslib::card_header("Clasificaci\u00f3n por jornada"),
          bslib::card_body(plotly::plotlyOutput(ns("plot_rank"), height = "300px"))
        ),
        bslib::card(
          bslib::card_header("Resultados"),
          bslib::card_body(plotly::plotlyOutput(ns("plot_goals"), height = "300px"))
        )
      ),

      # Fila 3: Goles línea + Pie
      bslib::layout_columns(
        col_widths = c(8, 4),
        fill = FALSE,
        class = "mt-3",
        bslib::card(
          bslib::card_header("Goles a favor y en contra por jornada"),
          bslib::card_body(plotly::plotlyOutput(ns("plot_goals_line"), height = "300px"))
        ),
        bslib::card(
          bslib::card_header("Total goles"),
          bslib::card_body(plotly::plotlyOutput(ns("plot_goals_pie"), height = "300px"))
        )
      )
    )
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Info estática del equipo ---
    team_info <- list(
      season = "2025/26",
      club   = "C.D Cazalegas - Ebora Formaci\u00f3n",
      coach  = "Francisco Javier L\u00f3pez de Francisco"
    )

    # --- Carga y procesado del Excel (estático) ---
    df <- readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Partidos")
    df <- df[order(df$Jornada), ]
    df$goles_favor  <- ifelse(df$ID_EquipoLocal == 1, df$GolesLocal,    df$GolesVisitante)
    df$goles_contra <- ifelse(df$ID_EquipoLocal == 1, df$GolesVisitante, df$GolesLocal)

    max_jornada <- max(df$Jornada)

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
        # Inicio de nueva selección
        rv$j_start <- j
        rv$j_end   <- NULL
        rv$selected_jornadas <- j
      } else {
        # Cierre de rango
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
      d <- df

      if (isTRUE(input$location == "local")) {
        d <- d[d$ID_EquipoLocal == 1, ]
      } else if (isTRUE(input$location == "visitante")) {
        d <- d[d$ID_EquipoVisitante == 1, ]
      }

      sel <- rv$selected_jornadas
      if (length(sel) > 0 && length(sel) < max_jornada) {
        d <- d[d$Jornada %in% sel, ]
      }

      if (nrow(d) > 0) d$DifGoles <- cumsum(d$goles_favor - d$goles_contra)

      d
    })

    # --- Helper: badge de variación ---
    make_delta <- function(current, previous, lower_is_better = FALSE) {
      if (is.null(previous)) {
        return(tags$span(class = "badge text-bg-secondary mt-1", "\u2192 sin jornada anterior"))
      }
      diff        <- current - previous
      improved    <- if (lower_is_better) diff < 0 else diff > 0
      neutral     <- diff == 0
      arrow       <- if (neutral) "\u2192" else if (improved) "\u2191" else "\u2193"
      badge_class <- if (neutral) "secondary" else if (improved) "success" else "danger"
      sign        <- if (diff > 0) "+" else ""
      tags$span(
        class = paste0("badge text-bg-", badge_class, " mt-1"),
        paste0(arrow, " ", sign, diff, " vs jornada ant.")
      )
    }

    # --- Info equipo (estática) ---
    output$club_name <- renderText(team_info$club)
    output$season    <- renderText(team_info$season)
    output$coach     <- renderText(team_info$coach)

    # --- KPIs reactivos al filtro ---
    output$rank <- renderText({
      d <- df_filtered()
      if (nrow(d) == 0) return("--")
      paste0(d$Pos[nrow(d)], "\u00ba")
    })

    output$points <- renderText({
      d <- df_filtered()
      if (nrow(d) == 0) return("--")
      as.character(d$Puntos[nrow(d)])
    })

    output$goal_diff <- renderText({
      d <- df_filtered()
      if (nrow(d) == 0) return("--")
      v <- d$DifGoles[nrow(d)]
      paste0(if (v >= 0) "+" else "", v)
    })

    output$rank_delta <- renderUI({
      d <- df_filtered()
      if (nrow(d) < 2) return(make_delta(NULL, NULL))
      make_delta(d$Pos[nrow(d)], d$Pos[nrow(d) - 1], lower_is_better = TRUE)
    })

    output$points_delta <- renderUI({
      d <- df_filtered()
      if (nrow(d) < 2) return(make_delta(NULL, NULL))
      make_delta(d$Puntos[nrow(d)], d$Puntos[nrow(d) - 1])
    })

    output$goal_diff_delta <- renderUI({
      d <- df_filtered()
      if (nrow(d) < 2) return(make_delta(NULL, NULL))
      make_delta(d$DifGoles[nrow(d)], d$DifGoles[nrow(d) - 1])
    })

    # --- Gráfico: clasificación por jornada ---
    output$plot_rank <- plotly::renderPlotly({
      d <- df_filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())
      plotly::plot_ly(d, x = ~Jornada, y = ~Pos,
        type = "scatter", mode = "lines+markers",
        line   = list(color = "#0d6efd", width = 2),
        marker = list(size = 8, color = "#0d6efd"),
        text      = ~paste0("J", Jornada, " \u2014 ", Pos, "\u00ba"),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis = list(title = "Jornada", dtick = 1, tickmode = "linear"),
          yaxis = list(title = "Posici\u00f3n", autorange = "reversed", dtick = 1),
          hovermode = "x unified"
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # --- Gráfico: victorias / empates / derrotas (desde goles, reacciona al filtro) ---
    output$plot_goals <- plotly::renderPlotly({
      d <- df_filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())
      valores <- c(
        sum(d$goles_favor > d$goles_contra),
        sum(d$goles_favor == d$goles_contra),
        sum(d$goles_favor < d$goles_contra)
      )
      plotly::plot_ly(
        x = c("Victorias", "Empates", "Derrotas"), y = valores,
        type = "bar",
        marker = list(color = c("#198754", "#ffc107", "#dc3545")),
        text = valores, textposition = "outside", hoverinfo = "x+y"
      ) |>
        plotly::layout(
          xaxis      = list(title = ""),
          yaxis      = list(title = "Partidos", dtick = 1, range = c(0, max(valores) + 1.5)),
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # --- Gráfico: goles por jornada (líneas) ---
    output$plot_goals_line <- plotly::renderPlotly({
      d <- df_filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())
      plotly::plot_ly(d, x = ~Jornada) |>
        plotly::add_trace(
          y = ~goles_favor, type = "scatter", mode = "lines+markers", name = "A favor",
          line = list(color = "#198754", width = 2), marker = list(size = 7, color = "#198754"),
          text = ~paste0("J", Jornada, ": ", goles_favor, " goles"), hoverinfo = "text"
        ) |>
        plotly::add_trace(
          y = ~goles_contra, type = "scatter", mode = "lines+markers", name = "En contra",
          line = list(color = "#dc3545", width = 2), marker = list(size = 7, color = "#dc3545"),
          text = ~paste0("J", Jornada, ": ", goles_contra, " goles"), hoverinfo = "text"
        ) |>
        plotly::layout(
          xaxis = list(title = "Jornada", dtick = 1, tickmode = "linear"),
          yaxis = list(title = "Goles", dtick = 1, rangemode = "tozero"),
          legend = list(orientation = "h", y = -0.25),
          hovermode = "x unified"
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # --- Gráfico: pie goles totales ---
    output$plot_goals_pie <- plotly::renderPlotly({
      d <- df_filtered()
      if (nrow(d) == 0) return(plotly::plotly_empty())
      plotly::plot_ly(
        labels = c("A favor", "En contra"),
        values = c(sum(d$goles_favor), sum(d$goles_contra)),
        type = "pie",
        marker = list(colors = c("#198754", "#dc3545")),
        textinfo = "label+percent", hoverinfo = "label+value",
        hole = 0.35
      ) |>
        plotly::layout(showlegend = FALSE) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
