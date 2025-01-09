#' @title Initiate Reactive Values
#' @details
#' `port_tbl` = user input of port data from setup
#' `port_tbl_miss` = user input port data that's not mapped to msl
#' `hot` = temporary store of hot data coming from rhandsontable ui during 
#'   setup, hot gets saved as port_tbl when user clicks update button
#' `asset_hold_tbl` = port_tbl grouped by ticker, sector, geography, etc
#' `asset_hold_tbl_sel` = category that's asset_hold table is grouped by
#' @export
set_reactive_vals <- function() {
  reactiveValues(
    port_tbl = data.frame(),
    port_tbl_miss = data.frame(),
    hot = NULL,
    asset_hold_tbl = NULL,
    asset_hold_tbl_sel = NULL,
    asset_date_start = NULL,
    asset_date_end = NULL,
    asset_ret_res = NULL,
    asset_bench_res = NULL,
    asset_perf_summ = NULL,
    asset_data_tbl = NULL,
    viz_asset_wealth = NULL,
    p = NULL
  )
}