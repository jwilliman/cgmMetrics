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
#' @param tz The timezone of the datetime field. Defaults to the system timezone if not specified.
#'
#' @return A data.table with the following columns; grouping variables (as indicated), datetime variables (obs_dttm, obt_dttmr, obs_dt, obs_tm), glucose measurements (glu).
#' @import data.table
#' @importFrom lubridate dminutes
#' @export
#'
#'
prep_data <- function(
  data, id_vars = NULL, var_datetime = NULL, var_glucose = NULL,
  tz = Sys.timezone()) {

 ## Due to NSE notes in R CMD check
  glu <- obs_dn <- obs_itime <- obs_idate <- obs_dttm <- obs_dttmr <- NULL

  if(is.null(var_datetime))
    var_datetime = rev(names(data))[2]

  if(is.null(var_glucose))
    var_glucose = rev(names(date))[1]

  keep_cols <- c(id_vars, var_datetime, var_glucose)


  ## Make standardised data.table of specified columns and column names
  dat <- data.table::setDT(data)[, .SD, .SDcols = keep_cols]

  if(!is.null(id_vars))
    data.table::setkeyv(dat, id_vars)

  data.table::setnames(dat, c(var_datetime, var_glucose), c("obs_dttm", "glu"))

  ## Make date and time variables
  ### Round datetime down to nearest 5 minute.
  dat[, obs_dttmr := xts::align.time(obs_dttm, 5 * 60) - lubridate::dminutes(5)]

  # ## Correct missed date caused by daylight savings.
  # dat[is.na(obs_dttmr) & hour(obs_dttm) = 3
  #     , obs_dttmr := as.POSIXct(paste0(as.Date(obs_dttm), "03:00:00"))]

  ### Split into date and time variables
  dat[, c("obs_idate", "obs_itime") := data.table::IDateTime(obs_dttmr, tz = tz)]

  data.table::setkeyv(dat, c(id_vars, "obs_idate"))

  ## Completed missing days and times for day between the first and last
  datx <- data.table::rbindlist(
    lapply(split(dat, by = id_vars),
           completeDT, cols = c(id_vars, "obs_idate", "obs_itime"))
  )

  datx[is.na(obs_dttmr), obs_dttmr := as.POSIXct(obs_idate, obs_itime, tz = tz)]

  ## Identify observations by Nocturnal (0000 h to 0559 h) or Daytime (0600 h to 2359 h).
  datx[, obs_dn := factor(
    obs_itime < as.ITime("06:00:00")
    , levels = c(FALSE, TRUE), labels = c("Day", "Night"))]


  ## Categorise glucose readings according to cutpoints specified by Battelino et al. 2023.
  datx[, `:=` (
    glu_fct6 = cut(glu, breaks = c(0, 53, 69, 140, 180, 250, 501))
    # , labels = c("Very low", "Low", "In tight range", "In range", "High", "Very high")
    , glu_fct3 = cut(glu, breaks = c(0,69,180,501))
  )]

  return(datx[])

}
