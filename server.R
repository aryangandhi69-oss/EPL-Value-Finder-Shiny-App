# app/server.R
source("global.R")
library(shiny)
library(ggplot2)
library(DT)
suppressPackageStartupMessages(library(fmsb))

if (!exists("safe_numeric")) safe_numeric <- function(x) suppressWarnings(as.numeric(x))

server <- function(input, output, session) {
  
  observe({
    seasons <- players %>% dplyr::pull(season) %>% unique() %>% sort()
    if (!length(seasons)) seasons <- seasons_avail
    updateSelectInput(session, "season", choices = seasons, selected = seasons[1])
  })
  
  observeEvent(input$reset_filters, {
    seasons <- players %>% dplyr::pull(season) %>% unique() %>% sort()
    if (!length(seasons)) seasons <- seasons_avail
    updateSelectInput(session, "season",  selected = seasons[1])
    updateSelectInput(session, "position", selected = pos_levels)
    updateSliderInput(session, "age_range",   value = c(age_min, age_max))
    updateSliderInput(session, "minutes_min", value = 300)
    updateSliderInput(session, "mv_range",    value = c(mv_min, mv_max))
    updateCheckboxInput(session, "log_x",     value = TRUE)
  })
  
  dat_filt <- reactive({
    req(input$season, input$position)  # Require both season AND position
    
    df <- players %>% 
      dplyr::filter(season == input$season)
    
    # Position filter - FIXED
    if (!is.null(input$position) && length(input$position) > 0) {
      df <- df %>% dplyr::filter(position_bucket %in% input$position)
    }
    
    # Minutes filter - FIXED (handle character minutes)
    if (!is.null(input$minutes_min)) {
      df <- df %>% dplyr::filter(safe_numeric(minutes) >= input$minutes_min)
    }
    
    # Age filter - FIXED
    if (!is.null(input$age_range)) {
      df <- df %>% dplyr::filter(
        is.na(age) | (safe_numeric(age) >= input$age_range[1] & safe_numeric(age) <= input$age_range[2])
      )
    }
    
    # Market value filter - FIXED
    if (!is.null(input$mv_range)) {
      df <- df %>% dplyr::filter(
        is.na(market_value_mill_gbp) | 
          (safe_numeric(market_value_mill_gbp) >= input$mv_range[1] & 
             safe_numeric(market_value_mill_gbp) <= input$mv_range[2])
      )
    }
    
    # Team filter - ADD THIS if you want team filtering to work too
    if (!is.null(input$team) && length(input$team) > 0) {
      df <- df %>% dplyr::filter(team_name %in% input$team)
    }
    
    df
  })
  
  output$n_players <- renderText({
    paste0(nrow(dat_filt()), " players match current filters")
  })
  
  output$scatter <- renderPlot({
    req(dat_filt())
    # 1) start from filtered data
    df <- dat_filt()
    
    # 2) Safely convert to numeric, replacing uncoercible values with NA
    df <- df %>%
      mutate(
        market_value_mill_gbp = safe_numeric(market_value_mill_gbp),
        goal_contrib_p90      = safe_numeric(goal_contrib_p90),
        pass_accuracy_pct     = safe_numeric(pass_accuracy_pct),
        tackles_p90           = safe_numeric(tackles_p90)
      )
    
    # 3) keep only rows that are plottable (both axes present and finite)
    df_plot <- df %>%
      filter(is.finite(market_value_mill_gbp),
             is.finite(goal_contrib_p90),
             market_value_mill_gbp > 0, # Essential for log scale!
             goal_contrib_p90 >= 0)
    
    if (nrow(df_plot) == 0) {
      plot.new()
      title("No plottable points. Try:\n• Lowering 'Minimum minutes'\n• Widening 'Market value (£m)'\n• Selecting fewer teams/positions")
      return()
    }
    
    p <- ggplot(df_plot, aes(x = market_value_mill_gbp,
                             y = goal_contrib_p90,
                             color = pass_accuracy_pct,
                             size  = tackles_p90,
                             label = player_name)) +
      geom_point(alpha = 0.85, stroke = 0.2) +
      scale_color_viridis_c(option = "D", na.value = "purple", name = "Pass acc (%)") +
      scale_size_continuous(range = c(2, 8), name = "Tackles/90") +
      labs(x = "Market Value (£m)", y = "Goal Contributions per 90",
           title = "Cost vs Performance") +
      theme_minimal(base_size = 12)
    
    if (isTRUE(input$log_x)) {
      p <- p + scale_x_continuous(trans = "log10",
                                  labels = scales::label_number(accuracy = 1))
    }
    
    p
  })
  
  output$scatter_table <- DT::renderDT({
    df <- dat_filt() %>%
      dplyr::mutate(across(c(market_value_mill_gbp, goal_contrib_p90,
                             pass_accuracy_pct, tackles_p90), safe_numeric)) %>%
      dplyr::select(dplyr::any_of(c(
        "player_name","team_name","position_bucket","season","minutes",
        "market_value_mill_gbp","goal_contrib_p90","pass_accuracy_pct","tackles_p90",
        "interceptions_p90","key_passes_p90","shots_on_target_p90"
      ))) %>%
      dplyr::arrange(dplyr::desc(goal_contrib_p90))
    DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  output$dl_scatter_png <- downloadHandler(
    filename = function() sprintf("cost_vs_performance_%s.png", input$season),
    content = function(file) {
      df <- dat_filt() %>%
        dplyr::mutate(
          market_value_mill_gbp = safe_numeric(market_value_mill_gbp),
          goal_contrib_p90      = safe_numeric(goal_contrib_p90),
          pass_accuracy_pct     = safe_numeric(pass_accuracy_pct),
          tackles_p90           = safe_numeric(tackles_p90)
        )
      if (!nrow(df)) {
        grDevices::png(file, width = 1200, height = 800, res = 120)
        plot.new(); title("No players match your filters"); dev.off(); return()
      }
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = market_value_mill_gbp, y = goal_contrib_p90,
        color = pass_accuracy_pct, size = tackles_p90
      )) +
        ggplot2::geom_point(alpha = 0.85, stroke = 0.2) +
        ggplot2::scale_color_viridis_c(option = "D", na.value = "grey70", name = "Pass acc (%)") +
        ggplot2::scale_size_continuous(range = c(2, 8), name = "Tackles/90") +
        ggplot2::labs(x = "Market Value (£m)", y = "Goal Contributions per 90",
                      title = paste("Cost vs Performance —", input$season)) +
        ggplot2::theme_minimal(base_size = 12)
      if (isTRUE(input$log_x)) {
        p <- p + ggplot2::scale_x_continuous(trans = "log10",
                                             labels = scales::label_number(accuracy = 1))
      }
      # trendline + r (same as on-screen)
      df_lm <- df %>% dplyr::filter(is.finite(market_value_mill_gbp),
                                    is.finite(goal_contrib_p90))
      if (nrow(df_lm) >= 3) {
        p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, alpha = 0.9)
        r <- suppressWarnings(cor(df_lm$market_value_mill_gbp, df_lm$goal_contrib_p90, use = "complete.obs"))
        p <- p + ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
                                   label = paste0("r = ", sprintf("%.2f", r)))
      }
      grDevices::png(file, width = 1200, height = 800, res = 120)
      print(p); dev.off()
    }
  )
  
  shortlist <- reactiveVal(tibble::tibble())
  observeEvent(input$add_selected_to_shortlist, {
    df <- dat_filt()
    brush <- input$scatter_brush
    if (!is.null(brush) && nrow(df)) {
      brushed <- brushedPoints(df, brush)
      if (nrow(brushed)) {
        s <- shortlist()
        s_new <- dplyr::bind_rows(
          s,
          brushed %>% dplyr::select(dplyr::any_of(c(
            "player_name","team_name","position_bucket","season","minutes",
            "market_value_mill_gbp","goal_contrib_p90","pass_accuracy_pct","tackles_p90"
          )))
        ) %>% dplyr::distinct()
        shortlist(s_new)
      }
    }
  })
  
  observe({
    df <- dat_filt()
    choices <- df %>% dplyr::pull(player_name) %>% unique() %>% sort()
    if (!length(choices)) choices <- ""
    updateSelectInput(session, "player_a", choices = choices,
                      selected = dplyr::first(choices))
    updateSelectInput(session, "player_b", choices = choices,
                      selected = if (length(choices) > 1) choices[2] else dplyr::first(choices))
  })
  
    output$radar <- renderPlot({
      df <- dat_filt()
      if (nrow(df) == 0 || is.null(input$player_a) || is.null(input$player_b)) {
        plot.new(); title("Select players"); return()
      }
      
      sel <- df %>% dplyr::filter(player_name %in% c(input$player_a, input$player_b))
      if (nrow(sel) < 2) { plot.new(); title("Pick two players"); return() }
      
      # Vars we expect (percentiles 0..1 created in 02_make_features.R)
      radar_cols <- c(
        "goal_contrib_p90_pct",
        "pass_accuracy_pct_pct",
        "tackles_p90_pct",
        "interceptions_p90_pct",
        "shots_on_target_p90_pct"
      )
      
      have <- intersect(radar_cols, names(sel))
      if (length(have) < 3) { plot.new(); title("Not enough percentile columns"); return() }
      
      # 1) one row per player, numeric, no NAs
      radar_df <- sel %>%
        dplyr::select(player_name, dplyr::all_of(have)) %>%
        dplyr::group_by(player_name) %>%              # if a player appears twice, average
        dplyr::summarise(dplyr::across(dplyr::all_of(have), ~ mean(suppressWarnings(as.numeric(.)), na.rm = TRUE)),
                         .groups = "drop") %>%
        tidyr::replace_na(list(
          goal_contrib_p90_pct = 0,
          pass_accuracy_pct_pct = 0,
          tackles_p90_pct = 0,
          interceptions_p90_pct = 0,
          shots_on_target_p90_pct = 0
        )) %>%
        tibble::column_to_rownames("player_name")
      
      # Ensure strictly numeric (guard against character tibbles)
      radar_df[] <- lapply(radar_df, function(x) suppressWarnings(as.numeric(x)))
      
      # 2) bounds 0..1 for fmsb; add max & min rows
      max_row <- rep(1, ncol(radar_df)); min_row <- rep(0, ncol(radar_df))
      plot_df <- rbind(max_row, min_row, radar_df)
      rownames(plot_df)[1:2] <- c("MAX","MIN")
      
      # 3) visible styling
      cols  <- c("purple","black")
      fills <- grDevices::adjustcolor(cols, alpha.f = 0.12)
      
      fmsb::radarchart(
        plot_df,
        axistype = 1,
        cglcol   = "grey80", cglty = 1, cglwd = 0.8,
        axislabcol = "grey50",
        pcol  = cols, plwd = 3, plty = 1,
        pfcol = fills, pdensity = 0,
        vlcex = 0.85,
        title = "Side-by-Side (position percentiles)"
      )
      legend("topright", legend = rownames(radar_df), col = cols, lty = 1, lwd = 3, bty = "n")
    })
  output$compare_table <- DT::renderDT({
    df <- dat_filt() %>%
      dplyr::select(dplyr::any_of(c(
        "player_name","team_name","position_bucket","minutes",
        "market_value_mill_gbp","goal_contrib_p90","pass_accuracy_pct",
        "tackles_p90","interceptions_p90","key_passes_p90","shots_on_target_p90"
      ))) %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
      dplyr::arrange(dplyr::desc(goal_contrib_p90))
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  observeEvent(input$add_both_to_shortlist, {
    df <- dat_filt() %>%
      dplyr::filter(player_name %in% c(input$player_a, input$player_b)) %>%
      dplyr::select(dplyr::any_of(c(
        "player_name","team_name","position_bucket","season","minutes",
        "market_value_mill_gbp","goal_contrib_p90","pass_accuracy_pct","tackles_p90"
      )))
    s <- shortlist()
    shortlist(dplyr::bind_rows(s, df) %>% dplyr::distinct())
  })
  
  output$shortlist_table <- DT::renderDT({
    DT::datatable(shortlist(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$dl_shortlist <- downloadHandler(
    filename = function() sprintf("shortlist_%s.csv", Sys.Date()),
    content  = function(file) readr::write_csv(shortlist(), file)
  )
  
  observeEvent(input$clear_shortlist, shortlist(tibble::tibble()))
}
