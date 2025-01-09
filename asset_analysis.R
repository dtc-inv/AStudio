# input ----

#' @title Input to run and change group by analysis of the holdinds
#' @param id namespace
#' @export
asset_holdings_tbl_options_input <- function(id) {
  tagList(
    div(
      style = 'display:inline-block',
      actionButton(
        inputId = NS(id, 'run'),
        label = 'Run Analysis',
        class = 'btn-btn primary',
        width = '150px'
      )
    ),
    div(
      style = 'display:inline-block; margin-left:10px',
      selectInput(
        inputId = NS(id, 'select'),
        label = 'Group by',
        choices = c('Ticker', 'Sector', 'Country', 'Parent'),
        selected = 'Ticker'
      ) 
    )
  )
}

#' @title Performance Summary Parameters
asset_perf_summ_options_input <- function(id) {
  tagList(
    div(
      style = "display:inline-block",
      dateRangeInput(
        inputId = NS(id, "dt_range"),
        label = "Analysis Time Period",
        start = Sys.Date() - years(1),
        end = Sys.Date()
      ),
      div(
        style = "display:inline-block",
        selectizeInput(
          inputId = NS(id, "select_bench"),
          label = "Select Benchmark(s)",
          choices = NULL,
          multiple = TRUE
        )
      ),
      div(
        radioButtons(
          inputId = NS(id, "dt_button"),
          label = "Select Common Period",
          choices = c("MTD", "QTD", "YTD", "TTM", "3Yr", "5Yr", "10Yr", "Max"),
          inline = TRUE,
          selected = "TTM"
        )
      ),
      div(
        actionButton(
          inputId = NS(id, "run"),
          label = "Run Analysis",
          class = "btn btn-success"
        )
      )
    )
  )
}

# ui ----
asset_tbl_ui <- function(id) {
  reactableOutput(NS(id, 'table'))
}

asset_plot_ui <- function(id) {
  plotlyOutput(NS(id, "plot"))
}

# server ----

asset_perf_summ_server <- function(id, db, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run, {
      if (!is.null(input$select_bench)) {
        res <- dtc_name_match_ret(input$select_bench, db$ret)
        rv$asset_bench_res <- res
      }
      rv$asset_date_start <- substr(input$dt_range, 1, 10)
      rv$asset_date_end <- substr(input$dt_range, 15, 24)
      mdf <- merge_msl(rv$port_tbl, db$msl)
      res <- dtc_name_match_ret(mdf$match$DTCName, db$ret)
      rv$asset_ret_res <- res

      rf <- db$pull_ret("BIL")
      x <- rv$asset_ret_res$r
      first_days <- rep(NA, ncol(x))
      last_days <- first_days
      for (i in 1:ncol(rv$asset_ret_res$r)) {
        first_days[i] <- zoo::index(na.omit(x[, i]))[1]
        last_days[i] <- zoo::index(na.omit(x[, i]))[nrow(na.omit(x[, i]))]
      }
      rv$asset_data_tbl <- reactable(
        data.frame(
          Asset = colnames(x),
          Start = as.Date(first_days, origin = '1970-01-01'),
          End = as.Date(last_days, origin = '1970-01-01')
        )
      )
      if (!is.null(input$select_bench)) {
        rv$asset_bench_res$r <- cut_time(rv$asset_bench_res$r,
                                       rv$asset_date_start, 
                                       rv$asset_date_end)
      }
      rv$asset_ret_res$r <- cut_time(rv$asset_ret_res$r,
                                     rv$asset_date_start,
                                     rv$asset_date_end)
      rf <- cut_time(rf, rv$asset_date_start, rv$asset_date_end)
      rv$asset_perf_summ <- perf_summary(rv$asset_ret_res$r, rf, 
                                         rv$asset_bench_res$r)
      rv$viz_asset_wealth <- viz_wealth_index(rv$asset_ret_res$r)
    })
  })
  return(rv)
}

asset_select_bench_server <- function(id, db, rv) {
  moduleServer(id, function(input, output, session) {
    name_tick <- as.list(db$msl$DTCName)
    names(name_tick) <- paste0(db$msl$Ticker, ' - ', db$msl$DTCName)
    updateSelectizeInput(
      session,
      "select_bench",
      choices = name_tick,
      selected = NULL,
      server = TRUE
    )
  })
  return(rv)
}

asset_perf_summ_options_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$dt_button, {
      if (input$dt_button == "MTD") {
        sd <- lubridate::floor_date(Sys.Date(), "months")
      } else if (input$dt_button == "QTD") {
        sd <- lubridate::floor_date(Sys.Date(), "quarters")
      } else if (input$dt_button == "YTD") {
        sd <- lubridate::floor_date(Sys.Date(), "years")
      } else if (input$dt_button == "TTM") {
        sd <- Sys.Date() - years(1) + 1
      } else if (input$dt_button == "3Yr") {
        sd <- Sys.Date() - years(3) + 1
      } else if (input$dt_button == "5Yr") {
        sd <- Sys.Date() - years(5) + 1
      } else if (input$dt_button == "10Yr") {
        sd <- Sys.Date() - years(10) + 1
      } else if (input$dt_button == "Max") {
        sd <- as.Date("1970-01-01")
      } else {
        stop("asset analysis date radio button input went awry")
      }
      updateDateRangeInput(
        session = session,
        inputId = "dt_range",
        start = sd
      )
      rv$asset_date_start <- substr(input$dt_range, 1, 10)
      rv$asset_date_end <- substr(input$dt_range, 15, 24)
    })
    return(rv)
  })
}


asset_analysis_server <- function(id, db, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run, {
      if (input$run > 0) {
        if ("character" %in% class(rv$port_tbl$returnInfo)) {
          rv$port_tbl$returnInfo <- Sys.Date()
        }
        mdf <- merge_msl(rv$port_tbl, db$msl)
        ddf <- drill_down(mdf, db$msl, db$bucket)
        xdf <- asset_reactable(ddf, db$bucket, input$select)
        rv$asset_hold_tbl <- xdf
      }
    })
    observeEvent(input$select, {
      if (input$run > 0) {
        mdf <- merge_msl(rv$port_tbl, db$msl)
        ddf <- drill_down(mdf, db$msl, db$bucket)
        xdf <- asset_reactable(ddf, db$bucket, input$select)
        rv$asset_hold_tbl <- xdf
        rv$asset_hold_tbl_sel <- input$select
      }
    })
    return(rv)
  })
}

# output ----

asset_wealth_output <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      ggplotly(rv$viz_asset_wealth)
    })
  })
}

asset_holdings_tbl_output <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({
      if (!is.null(rv$asset_hold_tbl)) {
        rv$asset_hold_tbl
      } else {
        return(NULL)
      }
    })
  })
}

asset_perf_summ_tbl_output <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({
      if (!is.null(rv$asset_perf_summ)) {
        reactable(rv$asset_perf_summ)
      } else {
        return(NULL)
      }
    })
  }) 
}


asset_perf_data_output <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({rv$asset_data_tbl})
  })
}

# util ----

asset_reactable <- function(ddf, bucket, group_by, recons_wgt = TRUE) {
  x <- ddf$match  
  xw <- pivot_wider(x, id_cols = DTCName, values_from = pctVal,
                    names_from = Parent, values_fn = sum)
  xw$Total <- rowSums(xw[, -1], na.rm = TRUE)
  if (recons_wgt) {
    tot <- colSums(xw[, 2:(ncol(xw)-1)], na.rm = TRUE)
    xw[, 2:(ncol(xw)-1)] <- xw[, 2:(ncol(xw)-1)] /
      matrix(tot, nrow = nrow(xw), ncol = length(tot), byrow = TRUE)
  }
  sec <- read_feather(bucket$path('co-data/arrow/sector.arrow'))
  xw <- left_join(xw, sec[, c('FactsetSector', 'DTCName')], by = 'DTCName')
  if (group_by == "Ticker") {
    # rewrite - doesn't need expandable group by
    res <- reactable(xw, searchable = TRUE)  
  }
  if (group_by == "Sector") {
    xw <- relocate(xw, FactsetSector)
    col_fmt <- lapply(
      colnames(xw)[-1], 
      \(x) colDef(
        aggregate = "sum", 
        format = colFormat(percent = TRUE, digits = 2))
    )
    names(col_fmt) <- colnames(xw)[-1]
    res <- reactable(
      xw,
      groupBy = "FactsetSector",
      defaultSorted = list(Total = "desc"),
      columns = col_fmt,
      searchable = TRUE
    )
  }
  return(res)
}

asset_fmt_ticker_tbl <- function(dat) {
  n_cols <- ncol(dat)
  per_cols <- colnames(dat)[3:(n_cols-4)]
  per_col_defs <- lapply(
    per_cols, 
    \(x) colDef(format = colFormat(percent = TRUE, digits = 2))
  )
  names(per_col_defs) <- per_cols
  num_cols <- colnames(dat)[(n_cols-3):n_cols]
  num_col_defs <- lapply(
    num_cols, 
    \(x) colDef(format = colFormat(digits = 2))
  )
  names(num_col_defs) <- num_cols
  col_defs <- c(per_col_defs, num_col_defs)
  col_defs$DTCName <- colDef(sticky = "left", minWidth = 150)
  return(col_defs)
}

asset_fmt_per_tbl <- function(dat) {
  n_cols <- ncol(dat)
  per_cols <- colnames(dat)[2:n_cols]
  per_col_defs <- lapply(
    per_cols, 
    \(x) colDef(format = colFormat(percent = TRUE, digits = 2))
  )
  names(per_col_defs) <- per_cols
  return(per_col_defs)
}

