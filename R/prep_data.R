#' Prepare raw CGM data for analysis
#'
#' @description
#' Edit a simple dataset contain grouping variables, a datetime variable, and glucose measurements by:
#' * Rounding times down to the closest 5 minute interval.
#' * Splitting the datetime variable into separate date and time variables.
#' * Adding extra rows to indicate missing observations, so each day has 288 observations and all days are included between the first and last observed day.
#' * Adding an indicator of daytime vs nocturnal observations.
#'
#' @param data A data.frame or data.table with at least two columns indicating the datetime and glucose reading.
#' @param id_vars A character vector of the names of any grouping variables to keep in the dataset.
#' @param var_datetime The name of the datetime column. If missing, assumes the second last column.
#' @param var_glucose The name of the column containing the glucose measurements. If missing, assumes the last column.
#' @param var_keep The names of any other columns to keep in the final dataset.
#' @param tz The timezone of the datetime field. Defaults to the system timezone if not specified.
#' @param accuracy Interval in seconds to round time to, defaults to 5 minutes (300 seconds).
#'
#' @return A data.table with the following columns; grouping variables (as indicated), datetime variables (obs_dttm, obt_dttmr, obs_dt, obs_tm), glucose measurements (glu).
#' @import data.table
#' @importFrom clock date_floor
#' @export
#'
#'
prep_data <- function(
  data, id_vars = NULL, var_datetime = NULL, var_glucose = NULL, var_keep = NULL,
  tz = Sys.timezone(), accuracy = 300L) {

 ## Due to NSE notes in R CMD check
  glu <- obs_dn <- obs_itime <- obs_1time <- obs_idate <- obs_dttm <- obs_dttmr <- NULL

  if(is.null(var_datetime))
    var_datetime = rev(names(data))[2]

  if(is.null(var_glucose))
    var_glucose = rev(names(date))[1]

  keep_cols <- c(id_vars, var_keep, var_datetime, var_glucose)


  ## Make standardised data.table of specified columns and column names
  dat <- data.table::setDT(data)[, .SD, .SDcols = keep_cols]

  if(!is.null(id_vars))
    data.table::setkeyv(dat, id_vars)

  data.table::setnames(dat, c(var_datetime, var_glucose), c("obs_dttm", "glu"))

  ### Split into date and time variables
  dat[, c("obs_idate", "obs_itime") := data.table::IDateTime(obs_dttm, tz = tz)]

  data.table::setkeyv(dat, c(id_vars, "obs_idate", "obs_itime"))

  ## Make date and time variables
  ### Round datetime down to required accuracy
  # dat[, obs_dttmr := clock::date_floor(obs_dttm, "minute", n = 5)]

  ### Round first observation of each day down according to specified accuracy
  ### then add time to all other observations rounded to nearest specified accuracy
  dat[, obs_1time := min(obs_itime), by = c(id_vars, "obs_idate")]
  dat[, obs_rtime := (obs_1time %/% accuracy) * accuracy +
        round( as.numeric(obs_itime - obs_1time) / (accuracy) ) * (accuracy)]

  ## Need to paste to get correct value for daylight savings.
  dat[, obs_dttmr := as.POSIXct(paste(obs_idate, obs_rtime), tz = tz, format = "%Y-%m-%d %T")]

  # ## Correct missed date caused by daylight savings.
  # dat[is.na(obs_dttmr) & hour(obs_dttm) = 3
  #     , obs_dttmr := as.POSIXct(paste0(as.Date(obs_dttm), "03:00:00"))]

  ## Completed missing days and times for day between the first and last
  ## Dropping empty data.tables
  datx <- Filter(function(x) nrow(x) > 0, split(dat, by = id_vars)) |>
    lapply(completeDT, cols = c(id_vars, "obs_idate", "obs_rtime")) |>
    data.table::rbindlist()

  ## Paste to force NA for non-existant dates.
  datx[is.na(obs_dttmr), obs_dttmr := as.POSIXct(paste(
    obs_idate, obs_rtime), tz = tz, format = "%Y-%m-%d %T")]

  ## Identify observations by Nocturnal (0000 h to 0559 h) or Daytime (0600 h to 2359 h).
  datx[, obs_dn := factor(
    obs_rtime < as.ITime("06:00:00")
    , levels = c(FALSE, TRUE), labels = c("Day", "Night"))]


  ## Categorise glucose readings according to cutpoints specified by Battelino et al. 2023.
  datx[, `:=` (
    glu_fct6 = cut(glu, breaks = c(0, 53, 69, 140, 180, 250, 501))
    # , labels = c("Very low", "Low", "In tight range", "In range", "High", "Very high")
    , glu_fct3 = cut(glu, breaks = c(0,69,180,501))
  )]

  return(datx[])

}
