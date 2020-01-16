#' divide input data to train and test data
#'
#' @param input_data data frame including columns 'date'
#' @param response_var_name name of the response variable (it will be omitted
#'   from test data)
#' @param train_date date - the moment of the training
#' @param training_length period object, length of the training period
#' @param test_length period object,length of the test period
#' @param unavailability_period_length period object, length of data
#'   unavailability before train_date
#'
#' @return list with two tibbles - train_data, test_data
#' @export
divideTrainTest <- function(input_data,
                            response_var_name,
                            train_date,
                            training_length,
                            test_length,
                            unavailability_period_length = days(0)) {
  train_start <-
    train_date %m-%
    unavailability_period_length %m-%
    training_length

  train_end <-
    train_date %m-%
    unavailability_period_length

  test_start <- train_date
  test_end <-
    train_date %m+%
    test_length

  train_data <-
    input_data %>%
    filter(date >= train_start,
           date <= train_end)

  test_data <-
    input_data %>%
    filter(date > test_start,
           date <= test_end) %>%
    select(-response_var_name)

  list(train_data = train_data,
       test_data = test_data)
}


#' Create data division for the backtest
#'
#' @param input_data data frame including columns 'date'
#' @param response_var_name name of the response variable (it will be omitted
#'   from test data)
#' @param train_date_points vector of dates - the moments of the training
#' @param training_length period object, length of the training period
#' @param test_length period object,length of the test period
#' @param unavailability_period_length period object, length of data
#'   unavailability before train_date
#'
#' @return nessted tible with columns: train_date (date), train_data (tibble),
#'   test_data (tibble)
#' @export
createTrainTestTempate <- function(input_data,
                                   response_var_name,
                                   train_date_points,
                                   training_length,
                                   test_length,
                                   unavailability_period_length = days(0)) {
  data_list <- NULL

  tibble(
    train_date = train_date_points,
    data_list = map(
      train_date_points,
      ~ divideTrainTest(
        input_data = input_data,
        response_var_name = response_var_name,
        train_date = .,
        training_length = training_length,
        test_length = test_length,
        unavailability_period_length = unavailability_period_length
      )
    ),
    train_data = map(data_list, ~ .$train_data),
    test_data = map(data_list, ~ .$test_data)
  ) %>%
    select(-data_list)

}


