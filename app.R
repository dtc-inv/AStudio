
astudioApp <- function(db) {
  ui <- navbarPage(
    "AStudio",
    theme = shinytheme('flatly'),
    navbarMenu(
      'Setup',
      tabPanel(
      'Portfolio',
        column(
          width = 2,
          setup_action_buttons('port_action')
        ),
        column(
          width = 10,
          setup_port_select_input('port_select'),
          setup_asset_select_input('asset_select'),
          br(),
          setup_port_tbl_ui('port_tbl')
        )
      ),
      tabPanel(
        "Missing Data",
        tabPanel(
          setup_port_missing_data_ui("miss")
        )
      )
    ),
    navbarMenu(
      'Asset',
      tabPanel(
        'Holdings',
        asset_holdings_tbl_options_input('asset'),
        br(),
        asset_tbl_ui('tbl')
      ),
      tabPanel(
        "Performance",
        navlistPanel(
          tabPanel(
            'Performance Summary',
            asset_perf_summ_options_input("asset_perf"),
            br(),
            verbatimTextOutput("test", TRUE),
            asset_tbl_ui("ap_tbl")
          ),
          tabPanel(
            "Wealth Index",
            asset_plot_ui("plot")
          ),
          tabPanel(
            "Data",
            asset_tbl_ui("asset_data")
          )
        )
      )
    ),
    navbarMenu(
      "Portfolio"
    )
  )
  
  server <- function(input, output, session) {
    rv <- set_reactive_vals()
    output$test <- renderText(rv$asset_date_start)
    rv <- setup_port_select_server('port_select', db, rv)
    rv <- setup_asset_select_server('asset_select', db, rv)
    rv <- setup_action_button_server('port_action', db, rv)
    rv <- setup_hot_tbl_server('port_tbl', rv)
    rv <- asset_analysis_server('asset', db, rv)
    rv <- asset_select_bench_server("asset_perf", db, rv)
    rv <- asset_perf_summ_server("asset_perf", db, rv)
    rv <- asset_perf_summ_options_server("asset_perf", rv)
    
    setup_port_tbl_server('port_tbl', rv)
    asset_holdings_tbl_output('tbl', rv)
    asset_perf_summ_tbl_output("ap_tbl", rv)
    asset_perf_data_output("asset_data", rv)
    asset_wealth_output("plot", rv)
  }
  
  shinyApp(ui, server)
}
