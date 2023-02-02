#' Calculate core and secondary CGM endpoints.
#'
#' Summarise continuous glucose monitoring (CGM) data to produce core and secondary continuous endpoints
#'   as specified in *Continuous glucose monitoring and metrics for clinical trials: an intertional consensus statement* by
#'   Bettelino *et al* (2023).
#'
#' @param data A data.table prepared for analysis using `prep_data` command.
#' @param by_vars Grouping variables to summarise statistics by. If missing, assumes all character or factor columns.
#' @param longform Logical indicating whether to reshape data long, or leave in wide form (the default).
#'
#' @return A data.table with metrics named with the prefix 'cgm'.
#' @import data.table
#' @export
#'
#'

make_metrics <- function(data, by_vars, longform = FALSE) {

  ## Due to NSE notes in R CMD check
  events <- cgm_measures <- value <- obs_n <- NULL

  dat_sums  <- calc_summaries(data, by_vars = by_vars)

  dat_hypo  <- calc_events(data, by_vars = by_vars, threshold = "<70", duration = 120)

  dat_hyper <- calc_events(data, by_vars = by_vars, threshold = ">250", duration = 120)

  dat_events <- merge(
    dat_hypo[, list(cgm2_03_ehypo  = sum(events)), by_vars],
    dat_hypo[, list(cgm2_04_ehyper = sum(events)), by_vars],
    by = by_vars, all = TRUE)

  dat_mwide <- merge(dat_sums, dat_events, by = by_vars, all = TRUE)

  if(longform) {

    dat_mlong <- data.table::melt(
      dat_mwide, measure.vars = patterns("cgm"),
      variable.name = "cgm_measures", value_name = "value")

    dat_mlong[grepl("cgm.*(t)", cgm_measures), c("prop", "lower", "upper") := as.list(
      Hmisc::binconf(value, obs_n, return.df = TRUE))]

    return(dat_mlong[])

  } else {

    return(dat_mwide[])

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
#' @export
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
      cgm1_01_tir    = sum(data.table::between(glu, 70, 180, incbounds = TRUE), na.rm = TRUE), # 3.9 to 10.0 PRIMARY OUTCOME
      cgm1_02_tbr70  = sum(glu < 70, na.rm = TRUE), # 3.9 mmol/L
      cgm1_03_tbr54  = sum(glu < 54, na.rm = TRUE),  # 3.0 mmol/L
      cgm1_04_tar180 = sum(glu > 180, na.rm = TRUE), # 10.0 mmol/L
      cgm1_05_tar250 = sum(glu > 250, na.rm = TRUE), # 14.0 mmol/L
      cgm1_06_cv     = stats::sd(glu, na.rm = TRUE) / mean(glu, na.rm = TRUE) * 100,
      cgm1_07_sd     = stats::sd(glu, na.rm = TRUE),
      cgm1_08_mn     = mean(glu, na.rm = TRUE),

      ## Secondary endpoints (continuous outcomes)
      cgm2_01_ttr  = sum(data.table::between(glu, 70, 140, incbounds = TRUE), na.rm = TRUE) # 3.9 to 7.8

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

