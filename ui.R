# app/ui.R
source("global.R")
library(shiny)
library(DT)

theme <- tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")

ui <- fluidPage(
  theme,
  titlePanel("EPL Value Finder — MSDS 456"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("season", "Season", choices = seasons_avail),
      selectInput("position", "Position (General)", choices = pos_levels,
                  selected = pos_levels, multiple = TRUE),
      selectInput("team", "Team (optional)",
                  choices = sort(unique(players$team_name)),
                  selected = NULL, multiple = TRUE),
      sliderInput("age_range", "Age range", min = age_min, max = age_max,
                  value = c(age_min, age_max), step = 1),
      sliderInput("minutes_min", "Minimum minutes",
                  min = mins_min, max = mins_max, value = 300, step = 90),
      sliderInput("mv_range", "Market value (£m)",
                  min = mv_min, max = mv_max, value = c(mv_min, mv_max)),
      checkboxInput("log_x", "Log scale for Market Value", value = TRUE),
      hr(),
      actionButton("reset_filters", "Reset Filters", class = "btn-secondary")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",
                  tabPanel("Cost vs Performance",
                           br(),
                           div(style="display:flex;gap:12px;align-items:center",
                               actionButton("add_selected_to_shortlist","Add brushed to shortlist",
                                            class="btn-primary"),
                               helpText("Brush points on the plot to add them.")
                           ),
                           textOutput("n_players"),
                           downloadButton("dl_scatter_png", "Download plot (PNG)"),
                           plotOutput("scatter", height="520px", brush="scatter_brush"),
                           br(),
                           DTOutput("scatter_table")
                  ),
                  tabPanel("Side-by-Side",
                           fluidRow(
                             column(5,
                                    selectInput("player_a", "Player A", choices = NULL),
                                    selectInput("player_b", "Player B", choices = NULL),
                                    br(),
                                    actionButton("add_both_to_shortlist","Add A & B to shortlist",
                                                 class="btn-primary")
                             ),
                             column(7,
                                    plotOutput("radar", height="420px"),
                                    br(),
                                    DTOutput("compare_table")
                             )
                           )
                  ),
                  tabPanel("Shortlist",
                           div(style="display:flex;gap:12px;align-items:center",
                               downloadButton("dl_shortlist","Download CSV"),
                               actionButton("clear_shortlist","Clear shortlist", class="btn-danger")
                           ),
                           br(),
                           DTOutput("shortlist_table")
                  ),
                  # ---- Tab 4: About/Data ----
                  tabPanel(
                    "About / Data",
                    br(),
                    tags$div(
                      class = "about",
                      
                      h4("About"),
                      p("EPL Value Finder helps lower-tier Premier League clubs identify cost-effective, league-proven player alternatives through cost vs performance analysis and side-by-side position-percentile profiles."),
                      
                      h4("Data Sources"),
                      tags$ul(
                        tags$li("Performance: FBref player Standard + Playing Time CSVs (season end years 2024 & 2025)."),
                        tags$li("Seasons covered: 2023/24 and 2024/25."),
                        tags$li("Position buckets: GK, CB, RB, LB, CDM, CM, CAM, RW, LW, ST/CF.")
                      ),
                      
                      h4("Market Values"),
                      p("I had to scrape ALL the data from fbref and some from Transfermarkt manually into a workbook and then create a .CSV file."),
                      p("I did so and moved all my csv files to Home/Final_Project(MSDS 456)/data/raw folder so that my app could easily scrape whatever information was required through the code."),
                      
                      h4("Methods (Summary)"),
                      tags$ul(
                        tags$li("Per-90 statistics computed from minutes (e.g., goal contributions/90)."),
                        tags$li("Within-position percentiles (0–1) used for radar charts."),
                        tags$li("Interactive filters: season, position, age, minutes, market value (with optional log scale)."),
                        tags$li("Shortlist: add brushed points or selected players; export as CSV.")
                      ),
                      
                      h4("Limitations"),
                      tags$ul(
                        tags$li("Not all players have verified market values; some entries may be missing or approximate due to the special characters in some players' names."),
                        tags$li("Percentiles are calculated within position buckets; cross-position comparisons are interpretive."),
                        tags$li("League play focus; European and domestic competitions not included.")
                      ),
                      
                      h4("Reproducibility"),
                      tags$ol(
                        tags$li("Place FBref CSVs in `data/raw/` (e.g., `fbref_epl_2024_standard.csv`, `fbref_epl_2025_playing_time.csv`)."),
                        tags$li("Optional: add `data/raw/market_values.csv` with columns `player_name, team_name, season, market_value_mill_gbp`."),
                        tags$li("Run `_run_all.R` to build `data/processed/players_clean.rds`."),
                        tags$li("Launch the app with `shiny::runApp('app')`.")
                      ),
                      
                      hr(),
                      p(em("Branding: Styled with Northwestern purple & black."))
                    )
                  )
                  
      )
    )
  )
)

