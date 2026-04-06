#' player_comparator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_comparator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style("
      .player-img {
        width: 80px; height: 80px; object-fit: cover; border-radius: 50%; border: 2px solid #dee2e6; margin-bottom: 0.5rem;
      }
      .player-col {
        display: flex; flex-direction: column; align-items: center; gap: 1rem;
      }
      .player-cards-row {
        display: flex; flex-direction: row; gap: 1rem; justify-content: center;
      }
    "),
    bslib::layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
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
      bslib::layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        class = "mb-3",
        # Columna 1
        tags$div(class = "player-col",
          shiny::selectInput(ns("player1"), NULL, choices = NULL, width = "100%"),
          uiOutput(ns("player1_info")),
          div(class = "player-cards-row",
            uiOutput(ns("player1_goals")),
            uiOutput(ns("player1_cards")),
            uiOutput(ns("player1_minutes"))
          ),
          uiOutput(ns("player1_barplot"))
        ),
        # Columna 2
        tags$div(class = "player-col",
          shiny::selectInput(ns("player2"), NULL, choices = NULL, width = "100%"),
          uiOutput(ns("player2_info")),
          div(class = "player-cards-row",
            uiOutput(ns("player2_goals")),
            uiOutput(ns("player2_cards")),
            uiOutput(ns("player2_minutes"))
          ),
          uiOutput(ns("player2_barplot"))
        )
      ),
      # Gráficos de líneas ocupando ambas columnas
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center gap-2",
          tags$span("Minutos jugados por jornada")
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("line_minutes"), height = "320px")
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center gap-2",
          tags$span("Goles/Tarjetas por jornada"),
          shiny::selectInput(ns("line_mode"), NULL, choices = c("Goles" = "goles", "Tarjetas" = "tarjetas"), width = "180px")
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("line_events"), height = "320px")
        )
      )
    )
  )
}
    
#' player_comparator Server Functions
#'
#' @noRd 
mod_player_comparator_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- Carga de datos de Eventos ---
    df_eventos <- tryCatch(
      readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Eventos"),
      error = function(e) data.frame()
    )
    if (nrow(df_eventos) == 0) {
      ruta_inst <- file.path("inst", "app", "data", "CAZALEGAS_B_DATA.xlsx")
      if (file.exists(ruta_inst)) {
        df_eventos <- tryCatch(
          readxl::read_excel(ruta_inst, sheet = "Eventos"),
          error = function(e) data.frame()
        )
      }
    }

    # Helper para encontrar columnas flexibles
    pick_col <- function(data, candidates) {
      found <- candidates[candidates %in% names(data)]
      if (length(found) == 0) return(NULL)
      found[[1]]
    }

    col_ev_player   <- pick_col(df_eventos, c("Jugador", "jugador", "Player"))
    col_ev_jornada  <- pick_col(df_eventos, c("Jornada", "jornada"))
    col_ev_tipo     <- pick_col(df_eventos, c("Tipo", "tipo", "Evento"))
    col_ev_min      <- pick_col(df_eventos, c("Minuto", "minuto", "Min", "Minute"))
    col_ev_loc      <- pick_col(df_eventos, c("Localizacion", "localizacion", "Localización"))
    col_ev_minutos  <- pick_col(df_eventos, c("MinutosTotales", "minutostotales", "Minutos"))
    col_ev_img      <- pick_col(df_eventos, c("Imagen", "imagen", "Img", "img"))

    # Asegura que max_jornada sea al menos 1 y que las jornadas siempre existan
    max_jornada <- suppressWarnings(
      if (nrow(df_eventos) > 0 && !is.null(col_ev_jornada) && !all(is.na(df_eventos[[col_ev_jornada]]))) max(df_eventos[[col_ev_jornada]], na.rm = TRUE) else 1
    )

    # --- Estado de selección de jornadas ---
    rv <- shiny::reactiveValues(
      j_start          = 1L,
      j_end            = max_jornada,
      selected_jornadas = seq_len(max_jornada)
    )

    # --- Cuadros de jornada ---
    output$jornada_squares <- renderUI({
      # Siempre muestra las jornadas del 1 al max_jornada, aunque no haya datos filtrados
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

    observeEvent(input$vuelta, {
      sel <- switch(input$vuelta,
        "all"    = seq_len(max_jornada),
        "first"  = seq_len(min(15L, max_jornada)),
        "second" = if (max_jornada > 15L) seq(16L, max_jornada) else integer(0)
      )
      rv$j_start <- if (length(sel) > 0) min(sel) else NULL
      rv$j_end   <- if (length(sel) > 0) max(sel) else NULL
      rv$selected_jornadas <- sel
    })

    # --- Datos filtrados ---
    df_filtered <- reactive({
      d <- df_eventos
      # Filtro por localización si existe columna
      if (!is.null(col_ev_loc)) {
        if (isTRUE(input$location == "local")) {
          d <- d[d[[col_ev_loc]] == "Local", ]
        } else if (isTRUE(input$location == "visitante")) {
          d <- d[d[[col_ev_loc]] == "Visitante", ]
        }
      }
      sel <- rv$selected_jornadas
      if (!is.null(col_ev_jornada) && length(sel) > 0 && length(sel) < max_jornada) {
        d <- d[d[[col_ev_jornada]] %in% sel, ]
      }
      d
    })

    # --- Jugadores y opción media equipo ---
    jugadores <- reactive({
      d <- df_filtered()
      if (is.null(col_ev_player)) return(character(0))
      jug <- unique(d[[col_ev_player]])
      jug <- jug[jug != "Rival" & !is.na(jug) & nzchar(jug)]
      jug <- sort(jug)
      # Si no hay jugadores, intenta obtener de todos los datos
      if (length(jug) == 0 && nrow(df_eventos) > 0 && !is.null(col_ev_player)) {
        jug_all <- unique(df_eventos[[col_ev_player]])
        jug_all <- jug_all[jug_all != "Rival" & !is.na(jug_all) & nzchar(jug_all)]
        jug <- sort(jug_all)
      }
      jug
    })

    observe({
      jug <- jugadores()
      choices <- c("Media equipo" = "__media__", jug)
      # Selecciona dos jugadores distintos si hay, si no, selecciona media y el primero
      sel1 <- choices[1]
      sel2 <- if (length(choices) > 2) choices[2] else if (length(choices) > 1) choices[2] else choices[1]
      # Si solo hay uno, ambos serán iguales
      updateSelectInput(session, "player1", choices = choices, selected = sel1)
      updateSelectInput(session, "player2", choices = choices, selected = sel2)
    })

    # --- Imagen y nombre jugador ---
    player_info_ui <- function(player) {
      d <- df_filtered()
      if (player == "__media__") {
        img <- "https://cdn-icons-png.flaticon.com/512/847/847969.png"
        nombre <- "Media equipo"
      } else {
        row <- d[d$Jugador == player & !is.na(d$Imagen) & nzchar(d$Imagen), ]
        img <- if (nrow(row) > 0) row$Imagen[1] else "https://cdn-icons-png.flaticon.com/512/847/847969.png"
        nombre <- player
      }
      tags$div(
        tags$img(src = img, class = "player-img"),
        tags$div(nombre, style = "font-weight:600; font-size:1.1rem;")
      )
    }

    output$player1_info <- renderUI({ player_info_ui(input$player1) })
    output$player2_info <- renderUI({ player_info_ui(input$player2) })

    # --- KPIs ---
    kpi_card <- function(icon, value, label) {
      bslib::card(
        bslib::card_body(
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; padding: 0.5rem; min-width: 80px;",
          tags$span(style = "font-size: 1.5rem; color: #0d6efd;", icon),
          tags$div(value, style = "font-size: 1.1rem; font-weight: 700; color: #0d6efd;"),
          tags$div(label, style = "font-size: 0.8rem; color: #6c757d;")
        )
      )
    }

    player_stats <- function(player) {
      d <- df_filtered()
      if (player == "__media__") {
        # Media equipo
        goles <- sum(d$Tipo == "Gol" & d$Jugador != "Rival", na.rm = TRUE) / length(jugadores())
        tarjetas <- sum(d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador != "Rival", na.rm = TRUE) / length(jugadores())
        minutos <- sum(d$MinutosTotales[!is.na(d$MinutosTotales) & d$Jugador != "Rival"], na.rm = TRUE) / length(jugadores())
        list(goles = round(goles,1), tarjetas = round(tarjetas,1), minutos = round(minutos,1))
      } else {
        goles <- sum(d$Tipo == "Gol" & d$Jugador == player, na.rm = TRUE)
        tarjetas <- sum(d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador == player, na.rm = TRUE)
        minutos <- sum(d$MinutosTotales[!is.na(d$MinutosTotales) & d$Jugador == player], na.rm = TRUE)
        list(goles = goles, tarjetas = tarjetas, minutos = minutos)
      }
    }

    output$player1_goals   <- renderUI({ kpi_card(icon("soccer-ball"), player_stats(input$player1)$goles, "Goles") })
    output$player1_cards   <- renderUI({ kpi_card(icon("exclamation"), player_stats(input$player1)$tarjetas, "Tarjetas") })
    output$player1_minutes <- renderUI({ kpi_card(icon("hourglass-end"), player_stats(input$player1)$minutos, "Minutos") })
    output$player2_goals   <- renderUI({ kpi_card(icon("soccer-ball"), player_stats(input$player2)$goles, "Goles") })
    output$player2_cards   <- renderUI({ kpi_card(icon("exclamation"), player_stats(input$player2)$tarjetas, "Tarjetas") })
    output$player2_minutes <- renderUI({ kpi_card(icon("hourglass-end"), player_stats(input$player2)$minutos, "Minutos") })

    # --- Gráfico barras: distribución por minutos de goles/tarjetas ---
    barplot_dist <- function(player, mode = c("goles", "tarjetas")) {
      d <- df_filtered()
      mode <- match.arg(mode)
      if (player == "__media__") {
        # Media equipo
        if (mode == "goles") {
          d_gol <- d[d$Tipo == "Gol" & d$Jugador != "Rival", ]
        } else {
          d_gol <- d[d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador != "Rival", ]
        }
      } else {
        if (mode == "goles") {
          d_gol <- d[d$Tipo == "Gol" & d$Jugador == player, ]
        } else {
          d_gol <- d[d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador == player, ]
        }
      }
      if (nrow(d_gol) == 0) return(plotly::plotly_empty())
      mins <- d_gol$Minuto
      mins <- mins[!is.na(mins)]
      if (length(mins) == 0) return(plotly::plotly_empty())
      tb <- as.data.frame(table(mins))
      names(tb) <- c("Minuto", "Eventos")
      tb$Minuto <- as.numeric(as.character(tb$Minuto))
      plotly::plot_ly(
        x = tb$Minuto,
        y = tb$Eventos,
        type = "bar",
        marker = list(color = if (mode == "goles") "#198754" else "#dc3545"),
        text = tb$Eventos,
        textposition = "outside",
        hoverinfo = "x+y"
      ) |>
        plotly::layout(
          xaxis = list(title = "Minuto"),
          yaxis = list(title = if (mode == "goles") "Goles" else "Tarjetas"),
          margin = list(t = 30, b = 60),
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE)
    }

    output$player1_barplot <- renderUI({
      shiny::selectInput(ns("bar_mode1"), NULL, choices = c("Goles" = "goles", "Tarjetas" = "tarjetas"), width = "120px")
      plotly::plotlyOutput(ns("barplot1"), height = "220px")
    })
    output$player2_barplot <- renderUI({
      shiny::selectInput(ns("bar_mode2"), NULL, choices = c("Goles" = "goles", "Tarjetas" = "tarjetas"), width = "120px")
      plotly::plotlyOutput(ns("barplot2"), height = "220px")
    })

    output$barplot1 <- plotly::renderPlotly({
      barplot_dist(input$player1, input$bar_mode1 %||% "goles")
    })
    output$barplot2 <- plotly::renderPlotly({
      barplot_dist(input$player2, input$bar_mode2 %||% "goles")
    })

    # --- Gráfico de líneas: minutos jugados por jornada ---
    output$line_minutes <- plotly::renderPlotly({
      d <- df_filtered()
      jug1 <- input$player1
      jug2 <- input$player2
      jugadores_sel <- unique(c(jug1, jug2))
      jornadas <- sort(unique(d$Jornada))
      df_lines <- data.frame()
      for (jug in jugadores_sel) {
        if (jug == "__media__") {
          mins_j <- tapply(d$MinutosTotales[!is.na(d$MinutosTotales) & d$Jugador != "Rival"], d$Jornada[!is.na(d$MinutosTotales) & d$Jugador != "Rival"], mean, default = 0)
        } else {
          mins_j <- tapply(d$MinutosTotales[!is.na(d$MinutosTotales) & d$Jugador == jug], d$Jornada[!is.na(d$MinutosTotales) & d$Jugador == jug], sum, default = 0)
        }
        df_lines <- rbind(df_lines, data.frame(Jugador = jug, Jornada = as.numeric(names(mins_j)), Minutos = as.numeric(mins_j)))
      }
      if (nrow(df_lines) == 0) return(plotly::plotly_empty())
      plotly::plot_ly(df_lines, x = ~Jornada, y = ~Minutos, color = ~Jugador, type = 'scatter', mode = 'lines+markers') |>
        plotly::layout(
          xaxis = list(title = "Jornada"),
          yaxis = list(title = "Minutos"),
          margin = list(t = 30, b = 60),
          showlegend = TRUE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # --- Gráfico de líneas: goles/tarjetas por jornada ---
    output$line_events <- plotly::renderPlotly({
      d <- df_filtered()
      jug1 <- input$player1
      jug2 <- input$player2
      jugadores_sel <- unique(c(jug1, jug2))
      mode <- input$line_mode %||% "goles"
      df_lines <- data.frame()
      for (jug in jugadores_sel) {
        if (jug == "__media__") {
          if (mode == "goles") {
            vals <- tapply(d$Tipo == "Gol" & d$Jugador != "Rival", d$Jornada, sum, default = 0) / length(jugadores())
          } else {
            vals <- tapply(d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador != "Rival", d$Jornada, sum, default = 0) / length(jugadores())
          }
        } else {
          if (mode == "goles") {
            vals <- tapply(d$Tipo == "Gol" & d$Jugador == jug, d$Jornada, sum, default = 0)
          } else {
            vals <- tapply(d$Tipo %in% c("Tarjeta Amarilla", "Tarjeta Roja") & d$Jugador == jug, d$Jornada, sum, default = 0)
          }
        }
        df_lines <- rbind(df_lines, data.frame(Jugador = jug, Jornada = as.numeric(names(vals)), Valor = as.numeric(vals)))
      }
      if (nrow(df_lines) == 0) return(plotly::plotly_empty())
      plotly::plot_ly(df_lines, x = ~Jornada, y = ~Valor, color = ~Jugador, type = 'scatter', mode = 'lines+markers') |>
        plotly::layout(
          xaxis = list(title = "Jornada"),
          yaxis = list(title = if (mode == "goles") "Goles" else "Tarjetas"),
          margin = list(t = 30, b = 60),
          showlegend = TRUE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}
    
## To be copied in the UI
# mod_player_comparator_ui("player_comparator_1")
    
## To be copied in the server
# mod_player_comparator_server("player_comparator_1")
