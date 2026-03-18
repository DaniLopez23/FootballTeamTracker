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
			.match-item {
				width: 100%;
				border: 1px solid #dce3ef;
				border-radius: 10px;
				background: #ffffff;
				text-align: left;
				padding: 0.65rem 0.75rem;
				cursor: pointer;
				transition: border-color 0.15s ease, background-color 0.15s ease;
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
			.match-page-title {
				font-weight: 700;
				margin-bottom: 0.8rem;
				color: #1f2d3d;
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
			.empty-right-panel {
				min-height: 520px;
				border: 1px dashed #d6dee9;
				border-radius: 10px;
				background: #fbfdff;
			}
			@media (max-width: 991px) {
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
				shiny::uiOutput(ns("matches_list"))
			),

			tags$div(
				class = "match-page-title",
				shiny::textOutput(ns("selected_match_title"), inline = TRUE)
			),

			bslib::layout_columns(
				col_widths = c(7, 5),
				fill = FALSE,

				bslib::card(
					bslib::card_header("Timeline de eventos"),
					bslib::card_body(
						shiny::uiOutput(ns("timeline_ui"))
					)
				),

				bslib::card(
					bslib::card_header("Panel derecho"),
					bslib::card_body(
						tags$div(class = "empty-right-panel")
					)
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

		df_matches <- readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Partidos")
		df_events  <- readxl::read_excel(app_sys("app/data/CAZALEGAS_B_DATA.xlsx"), sheet = "Eventos")

		# Columnas base de partidos
		col_jornada <- pick_col(df_matches, c("Jornada", "jornada"))
		col_loc_id  <- pick_col(df_matches, c("ID_EquipoLocal", "IdEquipoLocal", "IDEquipoLocal"))
		col_vis_id  <- pick_col(df_matches, c("ID_EquipoVisitante", "IdEquipoVisitante", "IDEquipoVisitante"))
		col_loc     <- pick_col(df_matches, c("EquipoLocal", "Local", "NombreEquipoLocal"))
		col_vis     <- pick_col(df_matches, c("EquipoVisitante", "Visitante", "NombreEquipoVisitante"))
		col_gl      <- pick_col(df_matches, c("GolesLocal", "GolLocal", "MarcadorLocal"))
		col_gv      <- pick_col(df_matches, c("GolesVisitante", "GolVisitante", "MarcadorVisitante"))

		if (is.null(col_jornada)) {
			df_matches$Jornada <- seq_len(nrow(df_matches))
			col_jornada <- "Jornada"
		}

		get_team_names <- function(row) {
			loc <- if (!is.null(col_loc)) as.character(row[[col_loc]]) else "Local"
			vis <- if (!is.null(col_vis)) as.character(row[[col_vis]]) else "Visitante"

			if ((!is.null(col_loc_id) && !is.na(row[[col_loc_id]]) && row[[col_loc_id]] == 1) ||
					(!is.null(col_vis_id) && !is.na(row[[col_vis_id]]) && row[[col_vis_id]] != 1)) {
				own <- loc
				rival <- vis
			} else {
				own <- vis
				rival <- loc
			}

			list(own = own, rival = rival, loc = loc, vis = vis)
		}

		own_is_local <- function(row) {
			if (!is.null(col_loc_id) && !is.na(row[[col_loc_id]])) {
				return(as.integer(row[[col_loc_id]]) == 1)
			}
			if (!is.null(col_vis_id) && !is.na(row[[col_vis_id]])) {
				return(as.integer(row[[col_vis_id]]) != 1)
			}
			TRUE
		}

		build_score <- function(row) {
			gl <- if (!is.null(col_gl)) suppressWarnings(as.integer(row[[col_gl]])) else NA_integer_
			gv <- if (!is.null(col_gv)) suppressWarnings(as.integer(row[[col_gv]])) else NA_integer_
			if (is.na(gl) || is.na(gv)) return("-")
			paste0(gl, " - ", gv)
		}

		df_matches$.__idx__ <- seq_len(nrow(df_matches))

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
				teams <- get_team_names(row)
				jornada <- as.character(row[[col_jornada]])
				score <- build_score(row)
				is_active <- identical(rv$selected_match, idx)

				tags$button(
					type = "button",
					class = paste("match-item", if (is_active) "match-item-active" else ""),
					onclick = paste0("Shiny.setInputValue('", ns("match_click"), "', ", idx, ", {priority:'event'})"),
					tags$div(class = "match-jornada", paste("Jornada", jornada)),
					tags$div(class = "match-teams", paste0(teams$loc, " vs ", teams$vis)),
					tags$div(class = "match-score", score)
				)
			})

			tags$div(class = "matches-list", items)
		})

		selected_match_row <- shiny::reactive({
			shiny::req(!is.na(rv$selected_match))
			df_matches[df_matches$.__idx__ == rv$selected_match, , drop = FALSE]
		})

		output$selected_match_title <- shiny::renderText({
			row <- selected_match_row()
			teams <- get_team_names(row)
			jornada <- as.character(row[[col_jornada]])
			score <- build_score(row)
			paste0("Jornada ", jornada, "  ", teams$loc, "  ", score, "  ", teams$vis)
		})

		# Columnas base de eventos
		col_ev_jornada <- pick_col(df_events, c("Jornada", "jornada"))
		col_ev_tipo    <- pick_col(df_events, c("Tipo", "tipo", "Evento"))
		col_ev_min     <- pick_col(df_events, c("Minuto", "minuto", "Min", "Minute"))
		col_ev_player  <- pick_col(df_events, c("Jugador", "jugador", "Player"))
		col_ev_obs     <- pick_col(df_events, c("Observaciones", "Observacion", "Detalle", "Descripcion"))
		col_ev_in      <- pick_col(df_events, c("JugadorEntra", "Entra", "Jugador_Entra"))
		col_ev_out     <- pick_col(df_events, c("JugadorSale", "Sale", "Jugador_Sale"))
		col_ev_team    <- pick_col(df_events, c("Equipo", "NombreEquipo", "EquipoEvento"))
		col_ev_side    <- pick_col(df_events, c("Localizacion", "Localidad", "Side", "EsLocal"))

		clean_txt <- function(x) tolower(trimws(as.character(x)))

		event_type_norm <- function(x) {
			x <- tolower(trimws(safe_chr(x)))
			ifelse(grepl("gol", x), "goal",
				ifelse(grepl("tarjeta", x), "card",
					ifelse(grepl("cambio|sustit", x), "sub", "other")
				)
			)
		}

		match_events <- shiny::reactive({
			if (nrow(df_events) == 0 || is.null(col_ev_jornada) || is.null(col_ev_tipo)) {
				return(df_events[0, , drop = FALSE])
			}

			row <- selected_match_row()
			jornada_selected <- as.character(row[[col_jornada]])
			d <- df_events[safe_chr(df_events[[col_ev_jornada]]) == jornada_selected, , drop = FALSE]
			if (nrow(d) == 0) return(d)

			d$.__etype__ <- event_type_norm(d[[col_ev_tipo]])
			d <- d[d$.__etype__ %in% c("goal", "card", "sub"), , drop = FALSE]
			if (nrow(d) == 0) return(d)

			minute_num <- if (is.null(col_ev_min)) {
				rep(NA_real_, nrow(d))
			} else {
				suppressWarnings(as.numeric(d[[col_ev_min]]))
			}
			d$.__min_sort__ <- ifelse(is.na(minute_num), 999, minute_num)
			d[order(d$.__min_sort__), , drop = FALSE]
		})

		event_side <- function(event_row, match_row) {
			teams <- get_team_names(match_row)
			own_local <- own_is_local(match_row)

			if (!is.null(col_ev_side)) {
				s <- clean_txt(event_row[[col_ev_side]])
				if (s %in% c("local", "home", "1", "true", "verdadero")) return("local")
				if (s %in% c("visitante", "away", "0", "false", "falso")) return("visitante")
			}

			if (!is.null(col_ev_team)) {
				tm <- clean_txt(event_row[[col_ev_team]])
				loc_name <- clean_txt(teams$loc)
				vis_name <- clean_txt(teams$vis)
				if (nzchar(tm) && grepl(loc_name, tm, fixed = TRUE)) return("local")
				if (nzchar(tm) && grepl(vis_name, tm, fixed = TRUE)) return("visitante")
			}

			player <- if (!is.null(col_ev_player)) clean_txt(event_row[[col_ev_player]]) else ""
			is_rival <- identical(player, "rival")
			if (is_rival) {
				if (own_local) "visitante" else "local"
			} else {
				if (own_local) "local" else "visitante"
			}
		}

		timeline_row_ui <- function(event_row, match_row) {
			etype <- as.character(event_row$.__etype__)
			minute <- if (!is.null(col_ev_min)) as.character(event_row[[col_ev_min]]) else "--"
			player <- if (!is.null(col_ev_player)) as.character(event_row[[col_ev_player]]) else ""
			detail <- if (!is.null(col_ev_obs)) as.character(event_row[[col_ev_obs]]) else ""
			side <- event_side(event_row, match_row)
			teams <- get_team_names(match_row)
			type_raw <- if (!is.null(col_ev_tipo)) clean_txt(event_row[[col_ev_tipo]]) else ""
			is_red <- grepl("roja", type_raw)
			is_yellow <- grepl("amarilla", type_raw)

			dot_class <- switch(etype,
				goal = "timeline-dot timeline-dot-goal",
				card = "timeline-dot timeline-dot-card",
				sub  = "timeline-dot timeline-dot-sub",
				"timeline-dot"
			)

			event_card_class <- switch(etype,
				goal = "timeline-event-card timeline-event-goal",
				sub = "timeline-event-card timeline-event-sub",
				card = if (is_red) "timeline-event-card timeline-event-card-red" else "timeline-event-card timeline-event-card-yellow",
				"timeline-event-card"
			)

			title <- switch(etype,
				goal = paste0("Gol: ", if (nzchar(player)) player else "Equipo"),
				card = {
					card_lbl <- if (is_red) "Tarjeta roja" else if (is_yellow) "Tarjeta amarilla" else "Tarjeta"
					paste0(card_lbl, ": ", if (nzchar(player)) player else "Jugador")
				},
				sub = {
					in_txt <- if (!is.null(col_ev_in)) as.character(event_row[[col_ev_in]]) else ""
					out_txt <- if (!is.null(col_ev_out)) as.character(event_row[[col_ev_out]]) else ""
					if (nzchar(in_txt) || nzchar(out_txt)) {
						paste0("Cambio: ", out_txt, " -> ", in_txt)
					} else if (nzchar(player)) {
						paste0("Cambio: ", player)
					} else {
						"Cambio"
					}
				},
				"Evento"
			)

			team_lbl <- if (identical(side, "local")) teams$loc else teams$vis

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
				tags$div(class = "timeline-side timeline-side-left", left_ui),
				tags$div(
					class = "timeline-center",
					tags$span(class = dot_class),
					tags$div(class = "timeline-time", paste0(minute, "'"))
				),
				tags$div(class = "timeline-side timeline-side-right", right_ui)
			)
		}

		output$timeline_ui <- shiny::renderUI({
			d <- match_events()
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
