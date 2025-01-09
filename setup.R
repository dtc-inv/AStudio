# input ----

#' @title Select Portfolio Input
#' @param id namespace
#' @param style CSS style input, default displays inline
#' @details build portfolio by selecting an option, e.g., CTF or mutual fund.
#'   selectizeInput with choices to be filled with `setup_port_select_server`
#' @export
setup_port_select_input <- function(id, style = 'display:inline-block') {
  div(
    style = style,
    selectizeInput(
      inputId = NS(id, 'select_port'),
      label = 'Load Portfolio',
      choices = NULL
    )
  )
}

#' @title Select Asset Input
#' @param id namespace
#' @param style_1 CSS style, default adds left margin
#' @param style_2 CSS style, default adds left margin
#' @details build or add onto a portfolio by selecting assets, e.g., stocks,
#'   ETFs, mutual funds. selectizeInput choices filled with 
#'   `setup_asset_select_server`. The action button adds after one or multiple
#'   assets have been selected. 
#' @export
setup_asset_select_input <- function(
  id, 
  style_1 = 'display:inline-block; margin-left:10px',
  style_2 = 'display:inline-block; margin-left:5px') 
{
  tagList(
    div(
      style = style_1,
      selectizeInput(
        inputId = NS(id, 'select_asset'),
        label = 'Select Investment(s)',
        choices = NULL,
        multiple = TRUE
      )
    ),
    div(
      style = style_2,
      actionButton(
        inputId = NS(id, 'add_asset'),
        label = 'Add',
        icon = icon('plus')
      )
    )
  )
}

#' @title Setup Action Buttons for Portfolio Selection Input
#' @param id namespace
#' @param width width of action buttons
#' @export
setup_action_buttons <- function(id, width = '150px') {
  tagList(
    radioButtons(
      inputId = NS(id, 'edit_radio'),
      label = 'Edit',
      choices = c('Value', 'Percent')
    ),
    actionButton(
      inputId = NS(id, 'update'),
      label = 'Update Weights',
      class = 'btn btn-primary',
      width = width,
      style = 'margin-top:10px'
    ),
    actionButton(
      inputId = NS(id, 'clear'),
      label = 'Clear Portfolio',
      class = 'btn btn-danger',
      width = width,
      style = 'margin-top:10px'
    ),
    actionButton(
      inputId = NS(id, 'save'),
      label = 'Save Portfolio',
      class = 'btn btn-success',
      width = width,
      style = 'margin-top:10px'
    ),
    actionButton(
      inputId = NS(id, "load"),
      label = "Load Portfolio",
      class = "btn btn-primary",
      width = width,
      style = "margin-top:10px"
    ),
    actionButton(
      inputId = NS(id, 'upload'),
      label = 'Upload Excel',
      class = 'btn btn-primary',
      width = width,
      style = 'margin-top:10px'
    ),
    actionButton(
      inputId = NS(id, 'template'),
      label = 'Excel Template',
      class = 'btn btn-info',
      width = width,
      style = 'margin-top:10px'
    )
  )
}

# ui ----

#' @title rhandsontable user interface
#' @param id namespace
#' @export
setup_port_tbl_ui <- function(id) {
  rHandsontableOutput(NS(id, 'tbl'), width = '1000px', height = '600px')
}

setup_port_missing_data_ui <- function(id) {
  reactableOutput(NS(id, "tbl"))
}

# server ----

#' @title Add choices to select port and update relative values with selection
#' @param id namespace
#' @param db Database Object
#' @param rv list of relative values
#' @return `rv` updated with selection
#' @details
#' `rv` is updated with `read_holdings_file` with the portfolio selection as
#'   the `DTC_Name`
#' @export
setup_port_select_server <- function(id, db, rv) {
  moduleServer(id, function(input, output, session) {
    ix <- db$msl$Layer >= 2
    ix[is.na(ix)] <- FALSE
    updateSelectizeInput(
      session,
      'select_port',
      choices = db$msl$DTCName[ix],
      selected = character(0),
      server = TRUE
    )
    observeEvent(input$select_port, {
      if (input$select_port != "") {
        rv$port_tbl <- read_holdings_file(db$bucket, input$select_port, TRUE)
        updateSelectizeInput(
          session,
          'select_port',
          selected = character(0)
        )
      }  
    })
    return(rv)
  })
}

#' @title Add choices to select asset and update relative values with selection
#' @param id namespace
#' @param db Database Object
#' @param rv list of relative values
#' @return `rv` updated with selection
#' @details
#' `rv` is updated with a data.frame with data from the `MSL`: name, ticker,
#'   cusip, and isin. pctVal, returnInfo, and Parent get default values.
#' @export
setup_asset_select_server <- function(id, db, rv) {
  moduleServer(id, function(input, output, session) {
    name_tick <- as.list(db$msl$DTCName)
    names(name_tick) <- paste0(db$msl$Ticker, ' - ', db$msl$DTCName)
    updateSelectizeInput(
      session,
      "select_asset",
      choices = name_tick,
      selected = NULL,
      server = TRUE
    )
    observeEvent(input$add_asset, {
      if (input$select_asset[1] != "") {
        ix <- match(input$select_asset, db$msl$DTCName)
        x <- data.frame(
          name = db$msl$DTCName[ix],
          ticker = db$msl$Ticker[ix],
          cusip = db$msl$CUSIP[ix],
          isin = db$msl$ISIN[ix],
          emv = 0,
          pctVal = 0,
          returnInfo = Sys.Date(),
          Parent = "Portfolio"
        )
        rv$port_tbl <- rob_rbind(rv$port_tbl, x)
        updateSelectizeInput(
          session,
          "select_asset",
          selected = ""
        )
      }
    })
    return(rv)
  })
}

#' @title Read hot values from portfolio set up table
#' @param id namespace
#' @param rv relative values
#' @details `rv` gets updated with data in the rhandsontable
#' @export
setup_hot_tbl_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$hot <- hot_to_r(input$tbl)
    })
    return(rv)
  })
}

#' @title Handle action buttons of portfolio set up
#' @param id namesapce
#' @param db Database Object
#' @param rv relative value list
#' @export
setup_action_button_server <- function(id, db, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$clear,  {
      rv$port_tbl <- data.frame()
    })
    observeEvent(input$update, {
      rv$port_tbl <- rv$hot[-1, ]
      if (input$edit_radio == 'Value') {
        rv$port_tbl$pctVal <- rv$port_tbl$emv / sum(rv$port_tbl$emv)
        rv$port_tbl <- add_total(rv$port_tbl)
      } else {
        tot <- sum(rv$port_tbl$emv)
        if (tot <= 0) {
          showModal(
            modalDialog(
              title = 'Warning',
              'Portfolios are not set up to handle net zero or lower values
               at the total portfolio level. Edit the emv (ending market value)
               column to set up positive weights.',
              footer = modalButton('Ugh, got it...')
            )
          )
        } else {
          rv$port_tbl <- rv$hot[-1, ]
          rv$pctVal <- rv$emv / tot
        }
      }
    })
    return(rv)
  })
}

#' @title Render the rhandsontable
#' @param id namespace
#' @param rv relative value list
#' @export
setup_port_tbl_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- renderRHandsontable({
      port_tbl <- column_handler(rv$port_tbl)
      port_tbl <- add_total(port_tbl)
      format_htbl(port_tbl)
    })
  })
}

# util ----

#' @title Create and format handsontable from data.frame
#' @param xdf data.frame with portfolio holdings
#' @return rhandsontable
#' @export
format_htbl <- function(xdf) {
  if (nrow(xdf) == 0) {
    return(NULL)
  }
  res <- rhandsontable(xdf) %>%
    hot_col('emv', format = '0,0.00') %>%
    hot_col('pctVal', format = '0.00%') %>%
    hot_col('returnInfo', type = 'date', dateFormat = "YYYY-MM-DD") %>%
    hot_rows(fixedRowsTop = 1)
}

#' @title Standardize Columns of the Portfolio Holdings data.frame
#' @param xdf portfolio holdings data.frame
#' @return formatted data.frame with specific columns
#' @export
column_handler <- function(xdf) {
  if (nrow(xdf) == 0) {
    return(xdf)
  }
  xcol <- colnames(xdf)
  if ("valUSD" %in% xcol) {
    if (! "emv" %in% xcol) {
      xdf$emv <- xdf$valUSD
    } else {
      xdf$emv[is.na(xdf$emv)] <- xdf$valUSD[is.na(xdf$emv)]
    }
  }
  if (! "emv" %in% colnames(xdf)) {
    warning("no value columns found")
    xdf$emv <- 0
  }
  xdf$emv <- as.numeric(xdf$emv)
  if (! "pctVal" %in% xcol) {xdf$pctVal <- xdf$emv / sum(xdf$emv)}
  if (! "name" %in% xcol) {xdf$name <- ""}
  if (! "ticker" %in% xcol) {xdf$ticker <- ""}
  if (! "isin" %in% xcol) {xdf$isin <- ""}
  if (! "cusip" %in% xcol) {xdf$cusip <- ""}
  if (! "returnInfo" %in% xcol) {xdf$returnInfo <- Sys.Date()}
  if (! "Parent" %in% xcol) {xdf$Parent <- "Portfolio"}
  xdf <- xdf[, c("name", "ticker", "cusip", "isin", "emv", 
                 "pctVal", "returnInfo", "Parent")]
  dt <- try(as.Date(xdf$returnInfo))
  if ('try-error' %in% class(dt)) {
    dt <- try(as.Date(as.numeric(xdf$returnInfo)))
    if ('try-error' %in% class(dt)) {
      stop("error in column_handler, can't convert returnInfo into date")
    }
  }
  xdf$returnInfo <- dt
  return(xdf)
}

#' @title Add total row to portfolio holdings data.frame
#' @param xdf portfolio holdings data.frame
#' @return data.frame with total in first row
#' @export
add_total <- function(xdf) {
  if (nrow(xdf) == 0) {
    return(xdf)
  }
  if (xdf$name[1] == "Total") {
    xdf <- xdf[-1, ]
  }
  tot <- data.frame(
    name = "Total",
    ticker = "",
    cusip = "",
    isin = "",
    emv = sum(xdf$emv),
    pctVal = sum(xdf$pctVal),
    returnInfo = NA,
    Parent = NA
  )
  rbind(tot, xdf)
}