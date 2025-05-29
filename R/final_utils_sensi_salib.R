.si_to_df <- function(Si) {
  df <- data.frame(
    names =   unlist(Si["names"]),
    S1 =      unlist(Si["S1"]),
    S1_conf = unlist(Si["S1_conf"]),
    ST =      unlist(Si["ST"]),
    ST_conf = unlist(Si["ST_conf"])
  )
  return(df)
}

.si_to_df_sobol <- function(Si, names) {
  df <- data.frame(
    names =   names,
    S1 =      Si$S1,
    S1_conf = Si$S1_conf,
    ST =      Si$ST,
    ST_conf = Si$ST_conf
  )
  return(df)
}

.salib_for_one_field_and_param <- function(
  field,
  df,
  param,
  number_of_simulations,
  problem,
  salib_sobol,
  fix_NAs_with_mean = FALSE,
  dry_run = FALSE) {

  if (dry_run) {
    cli::cli_alert_success("Computing SALib for field '{field}' and param '{param}'")
    return(data.frame())
  }

  if (!is.integer(number_of_simulations)) {
    cli::cli_alert_danger("'number_of_simulations' must be an integer (e.g. '100L')")
    stop()
  }

  arr <- df |>
    filter(field == !!field) |>
    pull(!!param)

  if (number_of_simulations < length(arr)) {
    cli::cli_alert_danger("field={field} param={param} -> number_of_simulations [{number_of_simulations}] is less than summarized array length for this field [{length(arr)}]")
    stop()
  } else if (number_of_simulations > length(arr)) {
    number_of_NAs_to_fill <- number_of_simulations - length(arr)
    cli::cli_alert_warning("field={field} param={param} -> number_of_simulations [{number_of_simulations}] is greater than array length [{length(arr)}]. Filling with {number_of_NAs_to_fill} NAs...")
    arr <- c(arr, rep(NA, number_of_NAs_to_fill))
  }

  if (fix_NAs_with_mean) {
    arr[is.na(arr)] <- mean(arr, na.rm = TRUE)
  }

  np <- import("numpy")
  analyze <- import("SALib.analyze.fast")
  if (salib_sobol) {
    analyze <- import("SALib.analyze.sobol")
  }

  np_arr <- np$array(arr)
  si_df <- NA
  if (salib_sobol) {
    Si <- analyze$analyze(problem, np_arr, calc_second_order = TRUE)
    si_df <- .si_to_df_sobol(Si, problem$names)
  } else {
    Si <- analyze$analyze(problem, np_arr)
    si_df <- .si_to_df(Si)
  }

  si_df <- si_df |>
    mutate(field = !!field) |>
    mutate(param = !!param) |>
    select(field, param, everything())

  return(si_df)
}
