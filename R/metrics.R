#' aggregate data and evaluate selected metric
#'
#' @param data a dataframe having colums date, observation and forecast
#' @param metric ma etric function - any function taking the /code{data} and
#'   returning one number.
#' @param unit a character string specifying the time unit to be rounded to.
#'   Should be one of "second","minute","hour","day", "week", "month", or
#'   "year." If NULL, no aggregation is performed.
#' @param aggregation_function a function for aggregating the observation and
#'   forecast. If NULL, no aggregation is performed.
#'
#' @return metric value for given data
#' @export
evaluateMetric <- function(data,
                           metric = metrics.mape,
                           unit = NULL,
                           aggregation_function = sum) {
  aggregationForMetrics(data,
                        unit = unit,
                        aggregation_function = aggregation_function) %>%
    metric()
}


#' aggregate data for metrics calculation
#'
#' @param data a dataframe having colums date, observation and forecast
#' @param unit a character string specifying the time unit to be rounded to.
#'   Should be one of "second","minute","hour","day", "week", "month", or
#'   "year." If NULL, no aggregation is performed.
#' @param aggregation_function a function for aggregating the observation and
#'   forecast. If NULL, no aggregation is performed.
#'
#' @return tibble with columns date, forecast and observation - aggregated
#' @export
aggregationForMetrics <- function(data,
                                  unit = "days",
                                  aggregation_function = sum) {
  if (is.null(unit) | is.null(aggregation_function)) {
    return(data)
  }

  data %>%
    group_by(date = floor_date(date,
                               unit = unit)) %>%
    summarise(forecast = aggregation_function(forecast),
              observation = aggregation_function(observation))
}


#' Metric MSE - mean square error
#'
#' @param data a dataframe having colums date, observation and forecast
#'
#' @return metric value for given data
#' @export
metrics.mse <- function(data) {
  data %>%
    summarise(mse = mean( (forecast  - observation)^2 )) %>%
    pull(mse)
}

#' Metric MAPE - mean absolute percentage error
#'
#' @param data a dataframe having colums date, observation and forecast
#'
#' @return metric value for given data
#' @export
metrics.mape <- function(data) {
  data %>%
    summarise(mape = mean( abs(forecast  - observation) / observation )) %>%
    pull(mape)
}


#' Metric MPE - mean percentage error
#'
#' @param data a dataframe having colums date, observation and forecast
#'
#' @return metric value for given data
#' @export
metrics.mpe <- function(data) {
  data %>%
    summarise(mpe = mean( (forecast  - observation) / observation )) %>%
    pull(mpe)
}
