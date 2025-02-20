library(officer)
library(flextable)
library(scales)
library(rtic)

xfile <-dir("imb-writer/scripts/")
for (i in 1:length(xfile)) {
  if (tools::file_ext(xfile[i]) == "R") {
    source(paste0("imb-writer/scripts/", xfile[i]))
  }
}

db <- Database$new("~/api_keys.RData", "C:/Users/asotolongo/AppData/Local/anaconda3/")

col <- diversifieR::dtc_col()
dict_file <- "N:/Investment Team/REPORTING/IMB/imb-writer/imb-data-input.xlsx"
dict <- readxl::read_excel(dict_file, "data")
descr <- readxl::read_excel(dict_file, "descr")

locater <- list(
  pie_cht = c(left = 6.47, top = 0.95, height = 2.04, width = 2.76),
  trail_perf_ft = c(left = 0.45, top = 6),
  perf_stat_ft = c(left = 0.45, top = 3.17),
  char_ft = c(left = 0.45, top = 4.91),
  wealth_cht = c(left = 2.2, top = 3.17, height = 2.78, width = 3.6),
  capm_cht = c(left = 6, top = 3.17, height = 2.78, width = 3.6),
  sect_cht = c(left = 4.81, top = 1.10, width = 4.79, height = 1.95),
  alloct_tbl = c(left = 0.45, top = 2.05, height = 1.15),
  descr_tbl = c(left = 0.45, top = 1.10)
)
# temp index workaround
ind <- read_parquet(db$bucket$path("returns/monthly/index.parquet"))
ind <- dataframe_to_xts(ind)

bond_pos <- locater
bond_pos$perf_stat_ft <- c(left = 0.34, top = 2.39)
bond_pos$descr_tbl <- c(left = 0.34, top = 0.92)
bond_pos$sect_cht <- c(left = 2.46, top = 1.97, height = 1.31, width = 4.2)
bond_pos$char_ft <- c(left = 0.34, top = 4.21)
bond_pos$wealth_cht <- c(left = 2.56, top = 3.28, height = 2.84, width = 3.36)
bond_pos$capm_cht <- c(left = 6.17, top = 3.28, height = 2.83, width = 3.36)
bond_pos$trail_perf_ft <- c(left = 0.34, top = 6.05)


# start pres
pres <- read_pptx("N:/Investment Team/REPORTING/IMB/imb-writer/template.pptx")
pres <- write_bond(
  dtc_name = "Vanguard Short-Term Tax Exempt (VWSUX)",
  bench = ind$`BofAML Municipals 1-3 Yr`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Vanguard Short-Term Tax Exempt",
  is_ctf = FALSE)

pres <- write_bond(
  dtc_name = "Fidelity Intermediate Muni (FLTMX)",
  bench = ind$`BofAML Municipals 1-12 Yr`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Fidelity Intermediate Muni",
  is_ctf = FALSE)

pres <- write_bond(
  dtc_name = "Short Duration",
  bench = ind$`BofAML U.S. Treasuries 1-3 Yr`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Short Duration CTF",
  is_ctf = FALSE,
  pie_type = "Sector")

pres <- write_bond(
  dtc_name = "Touchstone Total Return Bond (TCPNX)",
  bench = ind$`Bloomberg Barclays U.S. Aggregate`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Touchstone Total Return Bond",
  is_ctf = FALSE,
  pie_type = "Sector"
)

pres <- write_bond(
  dtc_name = "Pimco Low Duration II (PLDTX)",
  bench = ind$`BofAML Municipals 1-12 Yr`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Pimco Low Duration II",
  is_ctf = FALSE,
  pie_type = "Sector")

pres <- write_bond(
  dtc_name = "Pimco Total Return (PTTRX)",
  bench = ind$`Bloomberg Barclays U.S. Aggregate`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Pimco Total Return",
  is_ctf = FALSE,
  pie_type = "Sector"
)

pres <- write_bond(
  dtc_name = "Loomis Sayles Floating Rate (LSFYX)",
  bench = ind$`S&P / LSTA Leveraged Loan`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Loomis Sayles Floating Rate",
  is_ctf = FALSE,
  pie_type = "Sector"
)

pres <- write_bond(
  dtc_name = "Columbia Strategic Income (CPHUX)",
  bench = ind$`Bloomberg Barclays U.S. Aggregate`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Columbia Strategic Income",
  is_ctf = FALSE,
  pie_type = "Sector"
)

pres <- write_bond(
  dtc_name = "Vanguard Short Term Inflation Protected (VTIP)",
  bench = ind$`Bloomberg Barclays U.S. TIPS 0-5 Yr`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "TIPS",
  is_ctf = FALSE,
  pie_type = "Sector"
)

pres <- write_bond(
  dtc_name = "iShares 20 Year Treasury (TLT)",
  bench = ind$`Bloomberg Barclays U.S. Treasury 20+ Yr`,
  pres = pres,
  db = db,
  dict = dict,
  descr = descr,
  col = col,
  locater = bond_pos,
  slide_title = "Long Treasuries",
  is_ctf = FALSE,
  pie_type = "Sector"
)


# us active equity ctf
pres <- write_equity("US Active Equity", ind$`Russell 3000`,  pres, db, dict, 
                      descr, col, locater, "U.S. Active Equity CTF")
core_loc <- locater
core_loc$alloct_tbl["top"] <- 2.2
pres <- write_equity("US Core Equity", ind$`Russell 3000`, pres, db, dict, 
                      descr, col, core_loc, "U.S. Core Equity CTF")
pres <- write_equity("International Equity", ind$`MSCI ACWI ex-US`, pres, db, dict,
                     descr, col, locater, "International Equity CTF")
# write out
print(pres, "C:/Users/asotolongo/Downloads/test.pptx")
