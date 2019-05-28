#' Methods that power time_decompose()
#'
#' @inheritParams time_decompose
#'
#' @return A `tbl_time` object containing the time series decomposition.
#'
#' @seealso [time_decompose()]
#'
#' @examples
#'
#' library(dplyr)
#'
#' tidyverse_cran_downloads %>%
#'     ungroup() %>%
#'     filter(package == "tidyquant") %>%
#'     decompose_stl(count)
#'
#'
#' @references
#' - The "twitter" method is used in Twitter's [`AnomalyDetection` package](https://github.com/twitter/AnomalyDetection)
#'
#' @name decompose_methods


# STL & Twitter
#' @importFrom stlplus stlplus
#' @export
#' @rdname decompose_methods
decompose_stl <- function(data, target, twitter = FALSE, frequency = "auto", trend = "auto", message = TRUE) {

  # Checks
  if (missing(target)) stop('Error in decompose_stl(): argument "target" is missing, with no default', call. = FALSE)

  data <- prep_tbl_time(data)
  date_col_vals <- tibbletime::get_index_col(data)

  target_expr <- dplyr::enquo(target)

  date_col_name <- timetk::tk_get_timeseries_variables(data)[[1]]
  date_col_expr <- rlang::sym(date_col_name)

  freq <- anomalize::time_frequency(data, period = frequency, message = message)
  trnd <- anomalize::time_trend(data, period = trend, message = message)

  # Create time-series
  decomp_tbl <- data %>%
    dplyr::pull(!!target_expr) %>%
    stats::ts(frequency = freq)

  # Possibly decompose with STL
  decomp_tbl <- tryCatch(
    stlplus(
      decomp_tbl,
      s.window = "periodic",
      t.window = trnd,
      inner = 1,
      outer = 15,
      robust = TRUE
    ),
    error = function(e) {
      # Make errors non-halting, and return NULL instead
      # so we can continue processing
      warning("STL decomposition failed with error:", as.character(e))
      return(NULL)
    }
  )

  # Stop early if error
  if (is.null(decomp_tbl)) {
    return(decomp_tbl)
  }

  decomp_tbl <- decomp_tbl[["data"]] %>%
    tibble::add_column(!!date_col_name := date_col_vals, .after = 0) %>%
    dplyr::select(!!date_col_expr, observed = raw, season = seasonal, trend, remainder)


  # Using Twitter-based median spans
  if (twitter) {

    # Median Span Logic
    median_spans_needed <- round(nrow(data) / trnd)

    decomp_tbl <- decomp_tbl %>%
      dplyr::mutate(
        .period_groups = sort(rep(1:median_spans_needed, length.out = nrow(.)))
      ) %>%
      dplyr::group_by(.period_groups) %>%
      dplyr::mutate(median_spans = median(observed, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.period_groups)

    if (message) {
      med_span <- decomp_tbl %>%
        dplyr::count(median_spans) %>%
        dplyr::pull(n) %>%
        median(na.rm = TRUE)

      med_scale <- decomp_tbl %>%
        timetk::tk_index() %>%
        timetk::tk_get_timeseries_summary() %>%
        dplyr::pull(scale)

      message(glue::glue("median_span = {med_span} {med_scale}s"))
    }

    # Remainder calculation
    decomp_tbl <- decomp_tbl %>%
      dplyr::mutate(
        remainder = observed - season - median_spans
      ) %>%
      dplyr::select(!!date_col_expr, observed, season, median_spans, remainder)
  }

  decomp_tbl <- anomalize::prep_tbl_time(decomp_tbl)

  return(decomp_tbl)
}
