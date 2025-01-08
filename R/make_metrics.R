#' Calculate core and secondary CGM endpoints.
#'
#' Summarise continuous glucose monitoring (CGM) data to produce core and secondary continuous endpoints
#'   as specified in *Continuous glucose monitoring and metrics for clinical trials: an intertional consensus statement* by
#'   Bettelino *et al* (2023).
#'
#' @param data A data.table prepared for analysis using `prep_data` command.
#' @param id_vars Additional subject level variables to include in the final dataset.
#' @param time_var Time variables to group and summarise statistics by. If missing, assumes all character or factor columns.
#' @param output What type of dataset of output. 'Wide' (default) or 'long'.
#'
#' @return A data.table with metrics named with the prefix 'cgm'.
#' @import data.table
#'
#'

make_metrics <- function(data, id_vars, time_var = NULL, output = NULL) {

  ## Due to NSE notes in R CMD check
  events    <- cgm_measures <- value <- obs_n <- NULL

  # checkmate::assertFactor(data[[time_var]])

  by_vars <- c(id_vars, time_var)

  # Calculate core summary statistics and secondary continuous outcomes
  dat_sums  <- calc_summaries(data, by_vars = by_vars)
  # Convert counts to percentages
  calc_percent(dat_sums)

  # Calculate event rates
  dat_hypo  <- calc_events(data, by_vars = by_vars, threshold = "<70", duration = 120)
  dat_hyper <- calc_events(data, by_vars = by_vars, threshold = ">250", duration = 120)

  dat_events <- merge(
    dat_hypo[,  list(cgm2_03_ehypo  = sum(events)), by_vars],
    dat_hyper[, list(cgm2_04_ehyper = sum(events)), by_vars],
    by = by_vars, all = TRUE)

  # Combine core and secondary continuous measures
  dat_mwide <- merge(dat_sums, dat_events, by = by_vars, all = TRUE)

  setkeyv(dat_mwide, by_vars)

  # Calculate secondary binary endpoints
  dat_mwide[, `:=` (
    # Change in glucose management indicator
    cgm2_02_dgmi   = cgm2_02_gmi - cgm2_02_gmi[[1]],
    # Time in range > 70%
    cgm3_01_tir70  = cgm1_01_tir > 70,
    # Change in time in range >= 5% improvement
    cgm3_02_dtir5  = (cgm1_01_tir - cgm1_01_tir[[1]]) >= 5,
    # Change in time in range >= 10% improvement
    cgm3_03_dtir10 = (cgm1_01_tir - cgm1_01_tir[[1]]) >= 10,
    # Time below range < 4%
    cgm3_04_tbr4   = cgm1_02_tbr70 < 4,
    # Time below low range (54mg/dL) < 1%
    cgm3_05_tblr1  = cgm1_03_tbr54 < 1,
    # Time above range < 25%
    cgm3_06_tar25  = cgm1_04_tar180 < 25,
    # Time above high range (250mg/dL) < 5%
    cgm3_07_tahr5  = cgm1_05_tar250 < 5,
    # Change in time below low range less than 0.5% increase
    cgm3_08_dtblr5  = (cgm1_03_tbr54 - cgm1_03_tbr54[[1]]) > 0.5
    # Change in HbA1c of >0.5% points
    # cgm3_09_dhba = (hba1c - hba1c[[1]]) > 0.5

  ), by = id_vars]

  # Calculate secondary composite outcomes
  dat_mwide[, `:=` (

    # cgm4_01_dhbatbr = cgm3_09_dhba & !cgm3_08_dtblr5,
    cgm4_02_dtir10  = cgm3_03_dtir10 & !cgm3_08_dtblr5,
    cgm4_03_mntbr1  = cgm1_08_mn < 154 & cgm3_05_tblr1,
    cgm4_04_tirtbr4 = cgm3_01_tir70 & cgm3_04_tbr4,
    cgm4_05_tirtbr1 = cgm3_01_tir70 & cgm3_05_tblr1

  )]

  # Reshape long
  if(output == "long") {

    ## Core and secondary continuous endpoints
    dat_clong <- data.table::melt(
      dat_mwide[,.SD, .SDcols = unique(c(by_vars, "obs_N", "obs_n", grep("cgm(1|2).*", names(dat_mwide), value = TRUE)))]
      , measure.vars = patterns("cgm"),
      variable.name = "cgm_measures", value_name = "value")


    ## Calculate times
    dat_clong[grepl("tir|tbr|tar|ttr", cgm_measures), time := as.ITime(value * 24 * 60 * 60)]

    ## Binary and composite endpoints
    dat_blong <-  data.table::melt(
      dat_mwide[,.SD, .SDcols = unique(c(by_vars, "obs_N", "obs_n", grep("cgm(3|4).*", names(dat_mwide), value = TRUE)))]
      , measure.vars = patterns("cgm"),
      variable.name = "cgm_measures", value_name = "value")

    return(list(continuous = dat_clong[], binary = dat_blong[]))

  } else {

    dat_times <- dat_sums[, .SD, .SDcols = c(by_vars, time_var, grep("obs|tir|tbr|tar|ttr", names(dat_sums), value = TRUE))]
    calc_times(dat_times)

    return(list(metrics = dat_mwide[], times = dat_times[]))

  }

}



#' Calculate continuous CGM summary metrics
#'
#' @description
#' Calculates summary CGM metrics including:
#' * Proportions of times spent in, above, or below, consensus ranges.
#' * Summary measures (mean, standard deviation, coefficient of variation).
#'
#' @param data A data.table prepared for analysis using `prep_data` command.
#' @param by_vars Grouping variables to summarise statistics by. If missing, assumes all character or factor columns.
#'
#' @return A data.table containing the CGM summary metrics in long form.
#' @import data.table
#' @importFrom stats sd
#'
#'
calc_summaries <- function(data, by_vars = NULL) {

  ## Due to NSE notes in R CMD check
  glu <- NULL

  if(is.null(by_vars))
    by_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

  data.table::setkeyv(data, by_vars)

  dat_wide <- data[
    , list(
      ## Observations
      obs_N = .N,
      obs_n = sum(!is.na(glu)),

      ## Core endpoints
      # Time in range 70–180 mg/dL (3·9–10·0 mmol/L)
      cgm1_01_tir    = sum(data.table::between(glu, 70, 180, incbounds = TRUE), na.rm = TRUE),
      # Time below range <70 mg/dL (<3·9 mmol/L), including readings of <54 mg/dL (<3·9 mmol/L)
      cgm1_02_tbr70  = sum(glu < 70, na.rm = TRUE),
      # Time below range <54 mg/dL (<3·0 mmol/L)
      cgm1_03_tbr54  = sum(glu < 54, na.rm = TRUE),
      # Time above range >180 mg/dL (>10·0 mmol/L), including readings of >250 mg/dL (<13·9 mmol/L)
      cgm1_04_tar180 = sum(glu > 180, na.rm = TRUE),
      # Time above range >250 mg/dL (>13·9 mmol/L)
      cgm1_05_tar250 = sum(glu > 250, na.rm = TRUE),
      # Coefficient of variation
      cgm1_06_cv     = stats::sd(glu, na.rm = TRUE) / mean(glu, na.rm = TRUE) * 100,
      # SD of mean glucose
      cgm1_07_sd     = stats::sd(glu, na.rm = TRUE),
      # Mean sensor glucose
      cgm1_08_mn     = mean(glu, na.rm = TRUE),

      ## Secondary endpoints (continuous outcomes)
      # Time in tight range 70–140 mg/dL (3·9–7·8 mmol/L)
      cgm2_01_ttr  = sum(data.table::between(glu, 70, 140, incbounds = TRUE), na.rm = TRUE),
      # (Change in) Glucose Management Indicator
      cgm2_02_gmi  = 3.31 + (0.02392 * mean(glu, na.rm = TRUE))

    ), by = by_vars]


  return(dat_wide[])


}


#' Identify and count number of hypoglycaemia or hyperglycaemia events
#'
#' Identify and count instances (events) when an individual experience extended hypoglycaemia or hyperglycaemia. I.e. occasions when glucose measurements
#'   crossed a specified threshold for an extended period of time. The event is only considered to have ended when glucose values return to normal limits for 15 minutes or more.
#'   Missing data is ignored if lasting 10 minutes or less, otherwise it interrupts the event.
#'   Very long extended events may be counted as multiple consecutive events.
#'
#' @param data A data.table prepared for analysis using `prep_data` command.
#' @param by_vars Grouping variables to summarise events by. If missing, assumes all character or factor columns.
#' @param threshold Character variable indicating threshold and direction for assigning an event (i.e. <70 or >250).
#' @param duration Continuous duration in minutes for time spent above or below threshold to be considered an event.
#' @param event_name Character specifying name to be given to new column containing number of events.
#'
#' @return A data table.
#' @import data.table
#' @export
#'
#'
calc_events <- function(data = NULL, by_vars, threshold = "<70", duration = 120, event_name = "events") {


  ## Due to NSE notes in R CMD check
  events <- mins <- glu_event <- glu_event_id <- N <- NULL


  if(is.null(by_vars))
    by_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

  dat <- data.table::copy(data)

  ## Identify out of range observations when threshold is crossed
  expr <- paste0("dat[, glu_event := glu", threshold ,"]")
  eval(parse(text=expr))

  ## Assign consecutive out of range observations to single events.
  dat[, `:=` (glu_event_id  = data.table::rleid(glu_event)), by = by_vars]

  ## Calculate duration of events (duration in 5 minute intervals)
  by_vars_glu <- c(by_vars, "glu_event", "glu_event_id")
    dat_t1  <- dat[, .N, by = by_vars_glu]

  ## Drop events that last < 15 minutes, including missing values and return to normal limits.
  dat_t1b <- dat_t1[N >= 3, ]

  ## Merge adjacent similar events
  dat_t1b[, glu_event_id := rleid(glu_event), by = by_vars_glu]
  dat_t1c <- dat_t1b[, .(N = sum(N)), by = by_vars_glu ]

  ## Convert number of observations to minutes, and define events.
  dat_t1c[, mins := N*5]

  dat_t1c[, (event_name) := floor((glu_event == TRUE) * mins/duration)]

  dat_t1d <- merge(
    unique(dat_t1c[, .SD, .SDcols = by_vars]),
    dat_t1c[events >= 1, .SD, .SDcols = c(by_vars, "events", "mins")],
    by = by_vars, all = TRUE
  )

  dat_t1d[is.na(events), `:=` (events = 0, mins = 0)]

  return(dat_t1d)

}

#' Convert counts to percentages
#'
#' @param data Data table with columns to convert to percentages
#' @param cols Character vector of names of columns containing counts to convert to percentages. If not supplied, columns containing the text 'tir', 'tbr', 'tar', or 'ttr' are converted.
#' @param denom Character name of column containing denominator counts, or integer vector of counts to use as a denominator. If not supplied, 'obs_n' is used.
#'
#' @return Data table is modified in place, the function does not copy or return the data table.
#' @export
#'
#' @examples

calc_percent <- function(data, cols = NULL, denom = NULL) {

  if(is.null(cols))
    cols <- grep("tir|tbr|tar|ttr", names(data), value = TRUE)

  if(is.null(denom))
    denom = data$obs_n

  if(is.character(denom) & length(denom) == 1)
    denom = data[[denom]]

  for(col in cols)
    set(data, j = col, value = data[[col]]/denom*100)

}


#' Convert percentages to hours and minutes
#'
#' @param data Data table with columns to convert from percentages to hours and minutes.
#' @param cols Character vector of names of columns containing percentages to convert to times. If not supplied, columns containing the text 'tir', 'tbr', 'tar', or 'ttr' are converted.
#' @param hours Integer denoting maximum number of hours. Default is 24 hours.
#'
#' @return Data table is modified in place, the function does not copy or return the data table.
#' @export
#'
#' @examples

calc_times <- function(data, cols = NULL, hours = 24) {

  if(is.null(cols))
    cols <- grep("tir|tbr|tar|ttr", names(data), value = TRUE)

  for(col in cols)
    data.table::set(data, j = col, value = data.table::as.ITime(data[[col]]/100*hours*60*60))

}
