

write_equity <- function(dtc_name, bench, pres, db, dict, descr, col, locater,
                            slide_title, is_us = TRUE, is_ctf = TRUE) {
  set_flextable_defaults(font.size = 8)
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  rf <- db$read_ret_ticker("BIL")
  rf <- change_freq(na.omit(rf), "months")
  if (is_ctf) {
    fund <- dtc_name_match_ret(dtc_name, db$ret, TRUE)$r
    fund <- change_freq(na.omit(fund))
  }
  bench <- change_freq(na.omit(bench))
  combo <- clean_asset_bench_rf(fund, bench, rf)
  
  # need work around for mutual fund models
  hdf <- read_holdings_file(db$bucket, dtc_name, TRUE)
  mdf <- merge_msl(hdf, db$msl)
  ddf <- drill_down(mdf, db$msl, db$bucket)
  if (is_us) {
    bench_df <- read_holdings_file(db$bucket, "iShares Russell 3000 (IWV)", TRUE)
  } else {
    bench_df <- read_holdings_file(db$bucket, "iShares ACWI Ex US (ACWX)", TRUE)
  }
  trail_perf_ft <- create_trail_perf_tbl(combo, col)
  perf_stat_ft <- create_perf_tbl(combo, col)
  char_ft <- create_char_tbl(ddf, db, col)
  wealth_cht <- create_wealth_cht(combo, col)
  capm_cht <- create_capm_cht(combo, dict, db, col)
  sect_cht <- create_sector_cht(ddf, bench_df, db, col)
  
  alloc_tbl <- create_alloc_tbl(dict)
  descr_tbl <- create_descr_tbl(descr)
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      trail_perf_ft, 
      ph_location(
        left = locater$trail_perf_ft["left"],
        top = locater$trail_perf_ft["top"])) |>
    ph_with(
      perf_stat_ft, 
      ph_location(
        left = locater$perf_stat_ft["left"], 
        top = locater$perf_stat_ft["top"])) |>
    ph_with(
      char_ft, 
      ph_location(
        left = locater$char_ft["left"], 
        top = locater$char_ft["top"])) |>
    ph_with(
      wealth_cht, 
      ph_location(
        left = locater$wealth_cht["left"], 
        top = locater$wealth_cht["top"], 
        height = locater$wealth_cht["height"], 
        width = locater$wealth_cht["width"])) |>
    ph_with(
      capm_cht, 
      ph_location(
        left = locater$capm_cht["left"], 
        top = locater$capm_cht["top"], 
        height = locater$capm_cht["height"],
        width = locater$capm_cht["width"])) |>
    ph_with(
      sect_cht, 
      ph_location(
        left = locater$sect_cht["left"], 
        top = locater$sect_cht["top"], 
        width = locater$sect_cht["width"], 
        height = locater$sect_cht["height"])) |>
    ph_with(
      alloc_tbl, 
      ph_location(
        left = locater$alloct_tbl["left"], 
        top = locater$alloct_tbl["top"], 
        height = locater$alloct_tbl["height"])) |>
    ph_with(
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = locater$descr_tbl["top"]))
  
  return(pres)
}