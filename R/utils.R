globalVariables(c(".", "patterns"))


#' Complete a data table with missing combinations of data
#'
#' Turns implicit missing values into explicit missing values by adding empty rows. A simpler data.table version of `tidyr::complete()`. Modified from post on stackoverflow (https://stackoverflow.com/a/43483804/4241780).
#'
#'
#' @param DT A data.table.
#' @param cols Vector of column names to be completed. Date variables are filled in as a full sequence from first to last.
#' @param fill A named list that for each variable supplies a single value to use instead of NA for missing combinations.
#'
#' @return A `data.table`
#' @import data.table
#' @importFrom lubridate is.Date
#' @export
#'
#'

completeDT <- function(DT, cols, fill = NULL){

  make_vals <- function(col) {

    if(is.factor(col)) factor(levels(col))
    if(lubridate::is.Date(col))
      seq(min(col), max(col), by = 1)
    else unique(col)

  }

  mDT = do.call(data.table::CJ, c(lapply(DT[, cols, with = FALSE], make_vals), list(unique = TRUE)))

  res = DT[mDT, on=names(mDT)]
  if (length(fill))
    res[, names(fill) := Map(replace, .SD, lapply(.SD, is.na), fill), .SDcols=names(fill)]
  res[]
}
