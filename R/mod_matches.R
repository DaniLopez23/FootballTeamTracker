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
  ns <- shiny::NS(id)
  tags <- shiny::tags

  shiny::tagList(
    tags$style(
      "
      .matches-list {
        display: flex;
        flex-direction: column;
        gap: 0.45rem;
      }
      .matches-sidebar-scroll {
        max-height: 100dvh;
        overflow-y: auto;
        padding-right: 0.25rem;
      }
      .match-item {
        width: 100%;
        border: 1px solid #dce3ef;
        border-radius: 10px;
        background: #ffffff;
        text-align: left;
        padding: 0.65rem 0.75rem;
        cursor: pointer;
      }
      .match-item:hover {
        border-color: #0d6efd;
        background: #f4f8ff;
      }
      .match-item-active {
        border-color: #0d6efd;
        background: #eaf2ff;
        box-shadow: inset 0 0 0 1px #0d6efd;
      }
      .match-jornada {
        font-size: 0.72rem;
        font-weight: 700;
        color: #6c757d;
        text-transform: uppercase;
      }
      .match-teams {
        margin-top: 0.2rem;
        font-size: 0.88rem;
        font-weight: 600;
        color: #1f2d3d;
      }
      .match-score {
        margin-top: 0.15rem;
        font-size: 0.86rem;
        font-weight: 700;
        color: #0d6efd;
      }
      .score-line {
        padding: 0.45rem 0;
      }
      .match-title-block {
        border: 1px solid #dce3ef;
        border-radius: 10px;
        background: #ffffff;
        padding: 1rem 1.15rem;
        margin-bottom: 1rem;
      }
      .match-title-line {
        display: flex;
        align-items: baseline;
        justify-content: center;
        gap: 0.7rem;
        text-align: center;
        flex-wrap: wrap;
        text-transform: uppercase;
      }
      .match-team-own {
        color: #0d6efd;
        font-weight: 800;
        font-size: 1.2rem;
        line-height: 1.1;
      }
      .match-team-rival {
        color: #1f2d3d;
        font-weight: 700;
        font-size: 0.95rem;
        line-height: 1.1;
      }
      .match-score-big {
        color: #1f2d3d;
        font-weight: 900;
        font-size: 1.7rem;
        line-height: 1;
      }
      .kpi-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 0.75rem;
      }
      .kpi-block {
        border: 1px solid #dce3ef;
        border-radius: 10px;
        background: #ffffff;
        padding: 0.9rem;
      }
      .kpi-card {
        border: 1px solid #dce3ef;
        border-radius: 10px;
        background: #ffffff;
        padding: 0.75rem;
      }
      .kpi-label {
        font-size: 0.74rem;
        font-weight: 700;
        color: #6c757d;
        text-transform: uppercase;
      }
      .kpi-value {
        margin-top: 0.2rem;
        font-size: 1rem;
        font-weight: 700;
        color: #0d6efd;
      }
      .players-block {
        display: flex;
        flex-direction: column;
        gap: 0.95rem;
      }
      .players-subtitle {
        font-size: 0.82rem;
        font-weight: 700;
        text-transform: uppercase;
        color: #6c757d;
        margin-bottom: 0.35rem;
      }
      .timeline-wrap {
        position: relative;
      }
      .timeline-wrap::before {
        content: '';
        position: absolute;
        top: 0.3rem;
        bottom: 0.3rem;
        left: 50%;
        transform: translateX(-50%);
        width: 3px;
        background: #d9e2ef;
        border-radius: 2px;
      }
      .timeline-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 88px minmax(0, 1fr);
        gap: 0.8rem;
        align-items: center;
        margin-bottom: 0.9rem;
        position: relative;
      }
      .timeline-row:last-child {
        margin-bottom: 0;
      }
      .timeline-center {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        z-index: 1;
      }
      .timeline-dot {
        width: 0.82rem;
        height: 0.82rem;
        border-radius: 999px;
        box-shadow: 0 0 0 4px #ffffff;
      }
      .timeline-dot-goal {
        background: #198754;
      }
      .timeline-dot-card {
        background: #f0ad00;
      }
      .timeline-dot-sub {
        background: #0d6efd;
      }
      .timeline-time {
        font-size: 0.78rem;
        font-weight: 700;
        color: #6c757d;
        margin-top: 0.2rem;
      }
      .timeline-side {
        min-height: 1px;
      }
      .timeline-side-left {
        display: flex;
        justify-content: flex-end;
      }
      .timeline-side-right {
        display: flex;
        justify-content: flex-start;
      }
      .timeline-event-card {
        width: min(100%, 300px);
        border: 1px solid #dce3ef;
        border-radius: 10px;
        padding: 0.55rem 0.65rem;
        background: #ffffff;
      }
      .timeline-event-team {
        font-size: 0.73rem;
        text-transform: uppercase;
        font-weight: 700;
        color: #6c757d;
      }
      .timeline-event-title {
        margin-top: 0.12rem;
        font-size: 0.92rem;
        font-weight: 600;
        color: #1f2d3d;
      }
      .timeline-event-meta {
        margin-top: 0.1rem;
        font-size: 0.82rem;
        color: #6c757d;
      }
      .timeline-event-goal {
        border-left: 4px solid #198754;
        background: #eef9f2;
      }
      .timeline-event-sub {
        border-left: 4px solid #0d6efd;
        background: #edf4ff;
      }
      .timeline-event-card-yellow {
        border-left: 4px solid #f0ad00;
        background: #fff9e8;
      }
      .timeline-event-card-red {
        border-left: 4px solid #dc3545;
        background: #ffeff1;
      }
      @media (max-width: 991px) {
        .kpi-grid {
          grid-template-columns: 1fr;
        }
        .match-team-own {
          font-size: 1.05rem;
        }
        .match-team-rival {
          font-size: 0.85rem;
        }
        .match-score-big {
          font-size: 1.4rem;
        }
        .timeline-row {
          grid-template-columns: 1fr;
          gap: 0.45rem;
          padding-left: 1.1rem;
        }
        .timeline-wrap::before {
          left: 0.2rem;
          transform: none;
        }
        .timeline-center {
          align-items: flex-start;
          margin-left: -0.06rem;
        }
        .timeline-side-left,
        .timeline-side-right {
          justify-content: flex-start;
        }
        .timeline-event-card {
          width: 100%;
        }
      }
      "
    ),

    bslib::layout_sidebar(
      fill = FALSE,
      fillable = FALSE,

      sidebar = bslib::sidebar(
        width = 300,
        open = TRUE,
        tags$p("Partidos", style = "font-weight: 700; margin-bottom: 0.5rem;"),
        tags$div(class = "matches-sidebar-scroll", shiny::uiOutput(ns("matches_list")))
      ),

      tags$div(
        class = "match-title-block",
        tags$div(class = "score-line", shiny::uiOutput(ns("selected_match_title_ui")))
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,

        bslib::card(
          bslib::card_header("Jugadores"),
          bslib::card_body(shiny::uiOutput(ns("players_table")))
        ),

        bslib::card(
          bslib::card_header("Timeline de eventos"),
          bslib::card_body(shiny::uiOutput(ns("timeline_ui")))
        )
      )
    )
  )
}

#' matches Server Functions
#'
#' @noRd
mod_matches_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tags <- shiny::tags

    pick_col <- function(df, candidates) {
      found <- candidates[candidates %in% names(df)]
      if (length(found) == 0) return(NULL)
      found[[1]]
    }

    safe_chr <- function(x) {
      if (is.null(x)) return(character())
      as.character(x)
    }

    clean_txt <- function(x) {
      tolower(trimws(safe_chr(x)))
    }

    read_sheet_safe <- function(sheet_name) {
      out <- tryCatch(
        readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = sheet_name),
        error = function(e) NULL
      )
      if (is.null(out)) data.frame() else out
    }

    df_events <- read_sheet_safe("Eventos")
    df_teams <- read_sheet_safe("Equipos")

    col_team_id <- pick_col(df_teams, c("Id", "ID", "EquipoID", "id"))
    col_team_name <- pick_col(df_teams, c("Nombre", "Equipo", "Name", "nombre"))

    team_lookup <- c()
    if (nrow(df_teams) > 0 && !is.null(col_team_id) && !is.null(col_team_name)) {
      keys <- suppressWarnings(as.integer(df_teams[[col_team_id]]))
      vals <- trimws(as.character(df_teams[[col_team_name]]))
      valid <- !is.na(keys) & nzchar(vals)
      team_lookup <- stats::setNames(vals[valid], as.character(keys[valid]))
    }

    # Columnas base de eventos
    col_ev_pid <- pick_col(df_events, c("PartidoID", "PartidoId", "MatchID", "MatchId"))
    col_ev_jornada <- pick_col(df_events, c("Jornada", "jornada"))
    col_ev_tipo <- pick_col(df_events, c("Tipo", "tipo", "Evento"))
    col_ev_min <- pick_col(df_events, c("Minuto", "minuto", "Min", "Minute"))
    col_ev_mintot <- pick_col(df_events, c("MinutosTotales", "Minutos", "minutostotales"))
    col_ev_player <- pick_col(df_events, c("Jugador", "jugador", "Player"))
    col_ev_obs <- pick_col(df_events, c("Observaciones", "Observacion", "Detalle", "Descripcion"))
    col_ev_in <- pick_col(df_events, c("JugadorEntra", "Entra", "Jugador_Entra"))
    col_ev_out <- pick_col(df_events, c("JugadorSale", "Sale", "Jugador_Sale"))
    col_ev_titular <- pick_col(df_events, c("Titular", "titular"))
    col_ev_sustituido <- pick_col(df_events, c("Sustituido", "sustituido"))
    col_ev_loc_id <- pick_col(df_events, c("EquipoLocalID", "ID_EquipoLocal", "IdEquipoLocal"))
    col_ev_vis_id <- pick_col(df_events, c("EquipoVisitanteID", "ID_EquipoVisitante", "IdEquipoVisitante"))

    event_type_norm <- function(x) {
      x <- clean_txt(x)
      ifelse(grepl("gol", x), "goal",
        ifelse(grepl("tarjeta", x), "card",
          ifelse(grepl("cambio|sustit", x), "sub", "other")
        )
      )
    }

    get_team_name <- function(team_id) {
      key <- as.character(suppressWarnings(as.integer(team_id)))
      if (length(team_lookup) == 0 || !nzchar(key) || is.na(key) || !(key %in% names(team_lookup))) {
        return("")
      }
      team_lookup[[key]]
    }

    own_team_id <- 1L

    build_matches_from_events <- function() {
      if (nrow(df_events) == 0 || is.null(col_ev_pid)) {
        return(data.frame())
      }

      pids <- suppressWarnings(as.integer(df_events[[col_ev_pid]]))
      valid <- !is.na(pids)
      if (!any(valid)) {
        return(data.frame())
      }

      pids_u <- sort(unique(pids[valid]))
      rows <- lapply(pids_u, function(pid) {
        d <- df_events[pids == pid, , drop = FALSE]
        if (nrow(d) == 0) return(NULL)

        jornada <- if (!is.null(col_ev_jornada)) suppressWarnings(as.integer(d[[col_ev_jornada]][[1]])) else NA_integer_
        local_id <- if (!is.null(col_ev_loc_id)) suppressWarnings(as.integer(d[[col_ev_loc_id]][[1]])) else NA_integer_
        visit_id <- if (!is.null(col_ev_vis_id)) suppressWarnings(as.integer(d[[col_ev_vis_id]][[1]])) else NA_integer_

        local_name <- get_team_name(local_id)
        visit_name <- get_team_name(visit_id)

        etype <- if (!is.null(col_ev_tipo)) event_type_norm(d[[col_ev_tipo]]) else rep("other", nrow(d))
        player <- if (!is.null(col_ev_player)) clean_txt(d[[col_ev_player]]) else rep("", nrow(d))
        is_goal <- etype == "goal"
        is_rival_goal <- is_goal & player == "rival"
        is_own_goal <- is_goal & player != "rival"

        own_is_local <- if (!is.na(local_id)) {
          local_id == own_team_id
        } else if (!is.na(visit_id)) {
          visit_id != own_team_id
        } else {
          TRUE
        }

        goals_own <- sum(is_own_goal, na.rm = TRUE)
        goals_rival <- sum(is_rival_goal, na.rm = TRUE)

        goals_local <- if (own_is_local) goals_own else goals_rival
        goals_visit <- if (own_is_local) goals_rival else goals_own

        data.frame(
          PartidoID = pid,
          Jornada = jornada,
          LocalID = local_id,
          VisitanteID = visit_id,
          Local = if (nzchar(local_name)) local_name else "Local",
          Visitante = if (nzchar(visit_name)) visit_name else "Visitante",
          GolesLocal = goals_local,
          GolesVisitante = goals_visit,
          stringsAsFactors = FALSE
        )
      })

      out <- do.call(rbind, rows)
      if (is.null(out) || nrow(out) == 0) return(data.frame())
      out <- out[order(out$Jornada, out$PartidoID), , drop = FALSE]
      out$.__idx__ <- seq_len(nrow(out))
      out
    }

    df_matches <- build_matches_from_events()

    rv <- shiny::reactiveValues(selected_match = if (nrow(df_matches) > 0) 1L else NA_integer_)

    shiny::observeEvent(input$match_click, {
      clicked <- suppressWarnings(as.integer(input$match_click))
      if (!is.na(clicked) && clicked %in% df_matches$.__idx__) {
        rv$selected_match <- clicked
      }
    })

    output$matches_list <- shiny::renderUI({
      if (nrow(df_matches) == 0) {
        return(tags$div(class = "text-muted", "No hay partidos disponibles."))
      }

      items <- lapply(df_matches$.__idx__, function(idx) {
        row <- df_matches[df_matches$.__idx__ == idx, , drop = FALSE]
        jornada <- as.character(row$Jornada)
        score <- paste0(row$GolesLocal, " - ", row$GolesVisitante)
        is_active <- identical(rv$selected_match, idx)

        tags$button(
          type = "button",
          class = paste("match-item", if (is_active) "match-item-active" else ""),
          onclick = paste0("Shiny.setInputValue('", ns("match_click"), "', ", idx, ", {priority:'event'})"),
          tags$div(class = "match-jornada", paste("Jornada", jornada)),
          tags$div(class = "match-teams", paste0(row$Local, " vs ", row$Visitante)),
          tags$div(class = "match-score", score)
        )
      })

      tags$div(class = "matches-list", items)
    })

    selected_match_row <- shiny::reactive({
      shiny::req(!is.na(rv$selected_match))
      df_matches[df_matches$.__idx__ == rv$selected_match, , drop = FALSE]
    })

    selected_match_events <- shiny::reactive({
      shiny::req(nrow(df_events) > 0)
      row <- selected_match_row()
      if (nrow(row) == 0 || is.null(col_ev_pid)) return(df_events[0, , drop = FALSE])
      pid <- suppressWarnings(as.integer(row$PartidoID[[1]]))
      pids <- suppressWarnings(as.integer(df_events[[col_ev_pid]]))
      d <- df_events[pids == pid, , drop = FALSE]
      if (nrow(d) == 0) return(d)

      et <- if (!is.null(col_ev_tipo)) event_type_norm(d[[col_ev_tipo]]) else rep("other", nrow(d))

      titular_num <- if (!is.null(col_ev_titular)) suppressWarnings(as.numeric(d[[col_ev_titular]])) else rep(NA_real_, nrow(d))
      sustituido_num <- if (!is.null(col_ev_sustituido)) suppressWarnings(as.numeric(d[[col_ev_sustituido]])) else rep(NA_real_, nrow(d))
      minutos_totales_num <- if (!is.null(col_ev_mintot)) suppressWarnings(as.numeric(d[[col_ev_mintot]])) else rep(NA_real_, nrow(d))

      is_change_by_flags <- !is.na(sustituido_num) & sustituido_num == 1
      et[is_change_by_flags & et == "other"] <- "sub"

      d$.__sub_kind__ <- ifelse(
        is_change_by_flags & !is.na(titular_num) & titular_num == 1,
        "sale",
        ifelse(
          is_change_by_flags & !is.na(titular_num) & titular_num == 0,
          "entra",
          ""
        )
      )

      d$.__etype__ <- et

      minute_num <- if (is.null(col_ev_min)) rep(NA_real_, nrow(d)) else suppressWarnings(as.numeric(d[[col_ev_min]]))
      minute_calc <- minute_num

      is_sale <- d$.__sub_kind__ == "sale"
      is_entra <- d$.__sub_kind__ == "entra"

      minute_calc[is_sale & !is.na(minutos_totales_num)] <- minutos_totales_num[is_sale & !is.na(minutos_totales_num)]
      minute_calc[is_entra & !is.na(minutos_totales_num)] <- 90 - minutos_totales_num[is_entra & !is.na(minutos_totales_num)]

      d$.__minute_calc__ <- minute_calc
      d$.__minute_show__ <- ifelse(
        !is.na(minute_calc),
        as.character(as.integer(round(minute_calc))),
        if (!is.null(col_ev_min)) as.character(d[[col_ev_min]]) else "--"
      )

      d$.__min_sort__ <- ifelse(is.na(minute_calc), 999, minute_calc)
      d[order(d$.__min_sort__), , drop = FALSE]
    })

    output$selected_match_title <- shiny::renderText({
      row <- selected_match_row()
      paste0(row$Local, " ", row$GolesLocal, " - ", row$GolesVisitante, " ", row$Visitante)
    })

    output$selected_match_title_ui <- shiny::renderUI({
      row <- selected_match_row()

      local_id <- suppressWarnings(as.integer(row$LocalID[[1]]))
      visit_id <- suppressWarnings(as.integer(row$VisitanteID[[1]]))
      is_local_own <- !is.na(local_id) && local_id == own_team_id
      is_visit_own <- !is.na(visit_id) && visit_id == own_team_id

      local_class <- if (is_local_own) "match-team-own" else "match-team-rival"
      visit_class <- if (is_visit_own) "match-team-own" else "match-team-rival"

      tags$div(
        class = "match-title-line",
        tags$span(class = local_class, as.character(row$Local[[1]])),
        tags$span(class = "match-score-big", as.character(row$GolesLocal[[1]])),
        tags$span(class = "match-score-big", "-"),
        tags$span(class = "match-score-big", as.character(row$GolesVisitante[[1]])),
        tags$span(class = visit_class, as.character(row$Visitante[[1]]))
      )
    })

    output$kpi_goals <- shiny::renderText({
      d <- selected_match_events()
      if (nrow(d) == 0) return("0")
      sum(d$.__etype__ == "goal", na.rm = TRUE)
    })

    output$kpi_cards <- shiny::renderText({
      d <- selected_match_events()
      if (nrow(d) == 0) return("0")
      sum(d$.__etype__ == "card", na.rm = TRUE)
    })

    output$kpi_mvp <- shiny::renderText({
      d <- selected_match_events()
      if (nrow(d) == 0 || is.null(col_ev_player)) return("--")
      goals <- d[d$.__etype__ == "goal", , drop = FALSE]
      if (nrow(goals) == 0) return("--")
      players <- trimws(as.character(goals[[col_ev_player]]))
      players <- players[players != "Rival" & nzchar(players)]
      if (length(players) == 0) return("--")
      cnt <- sort(table(players), decreasing = TRUE)
      paste0(names(cnt)[[1]], " (", as.integer(cnt[[1]]), ")")
    })

    players_summary <- shiny::reactive({
      d <- selected_match_events()
      if (nrow(d) == 0 || is.null(col_ev_player)) return(data.frame())

      players <- trimws(as.character(d[[col_ev_player]]))
      valid <- players != "Rival" & nzchar(players)
      d <- d[valid, , drop = FALSE]
      if (nrow(d) == 0) return(data.frame())

      all_players <- sort(unique(trimws(as.character(d[[col_ev_player]]))))

      rows <- lapply(all_players, function(p) {
        dp <- d[trimws(as.character(d[[col_ev_player]])) == p, , drop = FALSE]

        mins <- NA_real_
        if ("MinutosTotales" %in% names(dp)) {
          mins_vec <- suppressWarnings(as.numeric(dp$MinutosTotales))
          if (any(!is.na(mins_vec))) mins <- max(mins_vec, na.rm = TRUE)
        }

        goles <- sum(dp$.__etype__ == "goal", na.rm = TRUE)
        tarjetas <- sum(dp$.__etype__ == "card", na.rm = TRUE)

        titular_flag <- if (!is.null(col_ev_titular)) {
          any(suppressWarnings(as.numeric(dp[[col_ev_titular]])) == 1, na.rm = TRUE)
        } else {
          FALSE
        }

        sale_flag <- if (!is.null(col_ev_titular) && !is.null(col_ev_sustituido)) {
          any(
            suppressWarnings(as.numeric(dp[[col_ev_titular]])) == 1 &
              suppressWarnings(as.numeric(dp[[col_ev_sustituido]])) == 1,
            na.rm = TRUE
          )
        } else {
          FALSE
        }

        entra_flag <- if (!is.null(col_ev_titular) && !is.null(col_ev_sustituido)) {
          any(
            suppressWarnings(as.numeric(dp[[col_ev_titular]])) == 0 &
              suppressWarnings(as.numeric(dp[[col_ev_sustituido]])) == 1,
            na.rm = TRUE
          )
        } else {
          FALSE
        }

        data.frame(
          Nombre = p,
          Minutos = ifelse(is.na(mins), "-", as.character(as.integer(mins))),
          Goles = as.integer(goles),
          Tarjetas = as.integer(tarjetas),
          Titular = titular_flag,
          Sale = sale_flag,
          Entra = entra_flag,
          stringsAsFactors = FALSE
        )
      })

      tb <- do.call(rbind, rows)
      tb$.__sort_mins__ <- suppressWarnings(as.numeric(tb$Minutos))
      tb$.__sort_mins__[is.na(tb$.__sort_mins__)] <- -1
      tb <- tb[order(-tb$Goles, -tb$Tarjetas, -tb$.__sort_mins__, tb$Nombre), , drop = FALSE]
      tb$.__sort_mins__ <- NULL
      tb
    })

    output$players_table <- shiny::renderUI({
      tb <- players_summary()
      if (nrow(tb) == 0) {
        return(tags$div(class = "text-muted", "No hay datos de jugadores para este partido."))
      }

      tags$div(
        class = "players-block",
        tags$div(
          tags$div(class = "players-subtitle", "Titulares"),
          DT::DTOutput(ns("players_table_starters"))
        ),
        tags$div(
          tags$div(class = "players-subtitle", "Suplentes"),
          DT::DTOutput(ns("players_table_subs"))
        )
      )
    })

    output$players_table_starters <- DT::renderDT(
      {
        tb <- players_summary()
        tb <- tb[tb$Titular, c("Nombre", "Minutos", "Goles", "Tarjetas", "Sale"), drop = FALSE]
        if (nrow(tb) == 0) {
          tb <- data.frame(Mensaje = "Sin titulares para este partido")
          return(DT::datatable(tb, rownames = FALSE, options = list(dom = "t")))
        }

        tb$Sale <- ifelse(tb$Sale, as.character(shiny::icon("arrow-right-from-bracket")), "")
        names(tb)[5] <- "Sale"

        DT::datatable(
          tb,
          escape = FALSE,
          rownames = FALSE,
          options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = TRUE,
            language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json")
          )
        )
      }
    )

    output$players_table_subs <- DT::renderDT(
      {
        tb <- players_summary()
        tb <- tb[!tb$Titular, c("Nombre", "Minutos", "Goles", "Tarjetas", "Entra"), drop = FALSE]
        if (nrow(tb) == 0) {
          tb <- data.frame(Mensaje = "Sin suplentes para este partido")
          return(DT::datatable(tb, rownames = FALSE, options = list(dom = "t")))
        }

        tb$Entra <- ifelse(tb$Entra, as.character(shiny::icon("arrow-right-to-bracket")), "")
        names(tb)[5] <- "Entra"

        DT::datatable(
          tb,
          escape = FALSE,
          rownames = FALSE,
          options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = TRUE,
            language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json")
          )
        )
      }
    )

    event_side <- function(event_row, match_row) {
      local_id <- suppressWarnings(as.integer(match_row$LocalID[[1]]))
      visit_id <- suppressWarnings(as.integer(match_row$VisitanteID[[1]]))
      own_is_local <- if (!is.na(local_id)) {
        local_id == own_team_id
      } else if (!is.na(visit_id)) {
        visit_id != own_team_id
      } else {
        TRUE
      }

      player <- if (!is.null(col_ev_player)) clean_txt(event_row[[col_ev_player]]) else ""
      if (identical(player, "rival")) {
        return(if (own_is_local) "visitante" else "local")
      }
      if (own_is_local) "local" else "visitante"
    }

    timeline_row_ui <- function(event_row, match_row) {
      etype <- as.character(event_row$.__etype__)
      minute <- if (".__minute_show__" %in% names(event_row)) as.character(event_row$.__minute_show__) else if (!is.null(col_ev_min)) as.character(event_row[[col_ev_min]]) else "--"
      player <- if (!is.null(col_ev_player)) as.character(event_row[[col_ev_player]]) else ""
      detail <- if (!is.null(col_ev_obs)) as.character(event_row[[col_ev_obs]]) else ""
      type_raw <- if (!is.null(col_ev_tipo)) clean_txt(event_row[[col_ev_tipo]]) else ""
      side <- event_side(event_row, match_row)

      team_lbl <- if (identical(side, "local")) as.character(match_row$Local[[1]]) else as.character(match_row$Visitante[[1]])

      dot_class <- switch(etype,
        goal = "timeline-dot timeline-dot-goal",
        card = "timeline-dot timeline-dot-card",
        sub = "timeline-dot timeline-dot-sub",
        "timeline-dot"
      )

      event_card_class <- switch(etype,
        goal = "timeline-event-card timeline-event-goal",
        sub = "timeline-event-card timeline-event-sub",
        card = if (grepl("roja", type_raw)) "timeline-event-card timeline-event-card-red" else "timeline-event-card timeline-event-card-yellow",
        "timeline-event-card"
      )

      title <- switch(etype,
        goal = paste0("Gol: ", if (nzchar(player)) player else "Equipo"),
        card = {
          card_lbl <- if (grepl("roja", type_raw)) "Tarjeta roja" else if (grepl("amarilla", type_raw)) "Tarjeta amarilla" else "Tarjeta"
          paste0(card_lbl, ": ", if (nzchar(player)) player else "Jugador")
        },
        sub = {
          sub_kind <- if (".__sub_kind__" %in% names(event_row)) as.character(event_row$.__sub_kind__) else ""
          in_txt <- if (!is.null(col_ev_in)) as.character(event_row[[col_ev_in]]) else ""
          out_txt <- if (!is.null(col_ev_out)) as.character(event_row[[col_ev_out]]) else ""
          if (identical(sub_kind, "sale") && nzchar(player)) {
            paste0("Cambio: Sale ", player)
          } else if (identical(sub_kind, "entra") && nzchar(player)) {
            paste0("Cambio: Entra ", player)
          } else if (nzchar(in_txt) || nzchar(out_txt)) {
            paste0("Cambio: ", out_txt, " -> ", in_txt)
          } else if (nzchar(player)) {
            paste0("Cambio: ", player)
          } else {
            "Cambio"
          }
        },
        "Evento"
      )

      card_ui <- tags$div(
        class = event_card_class,
        tags$div(class = "timeline-event-team", team_lbl),
        tags$div(class = "timeline-event-title", title),
        if (nzchar(detail)) tags$div(class = "timeline-event-meta", detail)
      )

      left_ui <- if (identical(side, "local")) card_ui else NULL
      right_ui <- if (identical(side, "visitante")) card_ui else NULL

      tags$div(
        class = "timeline-row",
        tags$div(
          class = "timeline-side timeline-side-left",
          left_ui
        ),
        tags$div(
          class = "timeline-center",
          tags$span(class = dot_class),
          tags$div(class = "timeline-time", paste0(minute, "'"))
        ),
        tags$div(
          class = "timeline-side timeline-side-right",
          right_ui
        )
      )
    }

    output$timeline_ui <- shiny::renderUI({
      d <- selected_match_events()
      if (nrow(d) == 0) {
        return(tags$div(class = "text-muted", "No hay eventos para este partido."))
      }

      d <- d[d$.__etype__ %in% c("goal", "card", "sub"), , drop = FALSE]
      if (nrow(d) == 0) {
        return(tags$div(class = "text-muted", "No hay eventos de goles, tarjetas o cambios para este partido."))
      }

      match_row <- selected_match_row()
      rows <- split(d, seq_len(nrow(d)))
      items <- lapply(rows, timeline_row_ui, match_row = match_row)
      tags$div(class = "timeline-wrap", items)
    })
  })
}