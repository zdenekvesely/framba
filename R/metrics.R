#' evaluate all defined metrics on all trained models
#'
#' @param evaluated_models nested tibble with evaluated model outputs (output of
#'   \code{evaluateModels})
#' @param observed_data dataframe with columns date and \code{response_var_name}
#'   with the real observations
#' @param metrics_definition list of metric functions - any function taking the
#'   dataframe with columns date, forecasted and observation and returning one
#'   number
#' @param response_var_name name of the response variable (column name in
#'   \code{evaluated_models})
#'
#' @return nested tibble with columns train_date, model_name and metrics.
#'   Metrics is tibble with columns metric_name and metrics_value
#' @export
evaluateAllMetrics <- function(evaluated_models,
                               observed_data,
                               metrics_definition,
                               response_var_name) {
  observed_data <-
    observed_data %>%
    select(date, observation = response_var_name)

  #internal function
  joinObserved <- function(forecasted_data) {
    forecasted_data %>%
      select(date, forecast = response_var_name) %>%
      inner_join(observed_data, by = "date")
  }

  evaluated_models %>%
    select(train_date, model_name, forecasted_data) %>%
    mutate(
      forecasted_data = map(forecasted_data,  joinObserved),
      metrics = map(
        forecasted_data,
        ~ evaluateMetricsList(data = .,
                              metrics_definition = metrics_definition)
      )
    ) %>%
    select(train_date, model_name, metrics)
}


#' aggregate data and evaluate one selected metric
#'
#' @param data a dataframe having colums date, observation and forecast
#' @param metric metric function - any function taking the \code{data} and
#'   returning one number
#' @param unit a character string specifying the time unit to be rounded to.
#'   Should be one of "second","minute","hour","day", "week", "month", or
#'   "year." If NULL, no aggregation is performed.
#' @param aggregation_function a function for aggregating the observation and
#'   forecast. If NULL, no aggregation is performed.
#'
#' @return metric value for given data
#' @export
evaluateOneMetric <- function(data,
                           metric = metrics.mape,
                           unit = NULL,
                           aggregation_function = sum) {
  aggregationForMetrics(data,
                        unit = unit,
                        aggregation_function = aggregation_function) %>%
    metric()
}


#' evaluate selected metrics list on one data
#'
#' @param data a dataframe having colums date, observation and forecast
#' @param metrics_definition list of metric functions - any function taking the
#'   \code{data} and returning one number.
#'
#' @return tibble with columns metric_name and metric_value
evaluateMetricsList <- function(data,
                                metrics_definition) {
  lapply(metrics_definition, function(f) f(data)) %>%
    as_tibble() %>%
    gather(key = "metric_name",
           value = "metric_value")
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
