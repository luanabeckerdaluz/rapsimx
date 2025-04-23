si_to_df <- function(Si) {
  df <- data.frame(
    names =   unlist(Si["names"]),
    S1 =      unlist(Si["S1"]),
    S1_conf = unlist(Si["S1_conf"]),
    ST =      unlist(Si["ST"]),
    ST_conf = unlist(Si["ST_conf"])
  )
  return(df)
}

si_to_df_sobol <- function(Si, names) {
  df <- data.frame(
    names =   names,
    S1 =      Si$S1,
    S1_conf = Si$S1_conf,
    ST =      Si$ST,
    ST_conf = Si$ST_conf
  )
  return(df)
}

salib_for_one_field_and_param <- function(
  field,
  df,
  param,
  number_of_simulations,
  problem,
  salib_sobol,
  fix_NAs_with_mean = FALSE,
  dry_run = FALSE
  ) {

  if (dry_run) {
    custom_cat(paste0("Computing SALib for field '", field, "' and param '", param, "'"))
    return(data.frame())
  }

  if (!is.integer(number_of_simulations)) {
    custom_stop("'number_of_simulations' must be an integer (e.g. '100L')")
  }

  arr <- df %>%
    filter(field == !!field) %>%
    pull(!!param)

  if (number_of_simulations < length(arr)) {
    custom_stop(paste0("field=", field, " param=", param, " -> number_of_simulations [", number_of_simulations, "] is less than summarized array length for this field [", length(arr), "]"))
  } else if (number_of_simulations > length(arr)) {
    number_of_NAs_to_fill <- number_of_simulations - length(arr)
    custom_warning(paste0("field=", field, " param=", param, " -> number_of_simulations [", number_of_simulations, "] is greater than array length [", length(arr), "]. Filling with ", number_of_NAs_to_fill, " NAs..."))
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
    si_df <- si_to_df_sobol(Si, problem$names)
  } else {
    Si <- analyze$analyze(problem, np_arr)
    si_df <- si_to_df(Si)
  }

  si_df <- si_df %>%
    mutate(field = !!field) |>
    mutate(param = !!param) |>
    select(field, param, everything())

  return(si_df)
}

compute_salib_for_all_params_and_fields <- function(
  sensi_tmp_folder,
  salib_sobol,
  columns_to_exclude = c("id", "field"),
  params = NA,
  fields = NA,
  fix_NAs_with_mean = FALSE,
  dry_run = FALSE,
  parallel = TRUE,
  overwrite = FALSE
  ) {

  # Check if salib.csv already exists
  salib_csv_filepath <- file.path(sensi_tmp_folder, "salib.csv")
  if (file.exists(salib_csv_filepath)) {
    if (overwrite) {
      custom_warning(paste0("'salib.csv' file already exists on ", sensi_tmp_folder, " folder! However, it will be overwritten because 'overwrite' parameter was set as TRUE!"))
    } else {
      custom_stop(paste0("'salib.csv' file already exists on ", sensi_tmp_folder, " folder! Please, if you want to create a new salib csv or overwrite it, set 'overwrite' to TRUE, create a new sensi folder or delete existing file!"))
    }
  } else {
    custom_cat("'salib.csv' file doesn't exist. Generating...")
    overwrite <- FALSE
  }

  # Print parallel status
  if (parallel) {
    custom_cat(paste0("Running in parallel with ", CONFIG_MULTICORES, " cores"))
  } else {
    custom_cat("Not using parallel")
  }

  # Get number of simulations (samples nrow)
  custom_cat(paste0("Loading 'samples.csv' from ", file.path(sensi_tmp_folder, "samples.csv")))
  df_samples <- read.csv(file.path(sensi_tmp_folder, "samples.csv"))
  number_of_simulations <- nrow(df_samples)

  # Read summarized csv
  custom_cat(paste0("Loading 'summarized.csv' from ", file.path(sensi_tmp_folder, "summarized.csv")))
  df <- read.csv(file.path(sensi_tmp_folder, "summarized.csv"))
  # print(head(df))

  custom_cat(paste0("Loading 'problem.R' from ", file.path(sensi_tmp_folder, "problem.R")))
  source(file.path(sensi_tmp_folder, "problem.R"))
  # print(problem)

  # Get only params to compute salib
  if (anyNA(params)) {
    params <- colnames(df)[!colnames(df) %in% columns_to_exclude]
  }

  # Get fields
  if (anyNA(fields)) {
    fields <- unique(df$field)
  }

  # Generate all combinations of fields and params
  all_combinations <- expand.grid(fields, params)
  # print(all_combinations)

  # For each combination of fields and params, compute SALib

  apply_parameters <- list(
    X = all_combinations,
    MARGIN = 1,
    future.seed = TRUE,
    FUN = function(x) {
      field <- x[1]
      param <- x[2]
      salib_for_one_field_and_param(
        df = df,
        field = field,
        param = param,
        number_of_simulations = number_of_simulations,
        problem = problem,
        salib_sobol = salib_sobol,
        fix_NAs_with_mean = fix_NAs_with_mean,
        dry_run = dry_run
      )
    }
  )

  if (parallel) {
    cl <- parallel::makeCluster(CONFIG_MULTICORES)
    future::plan(future::cluster, workers = cl)
    list_dfs_salibs <- do.call(future.apply::future_apply, apply_parameters)
    parallel::stopCluster(cl)
  } else {
    list_dfs_salibs <- do.call(apply, apply_parameters)
  }

  if (dry_run) {
    return(data.frame())
  }

  # rbind all combination df
  rbind_salibs <- Reduce(function(x, y) rbind(x, y, all = TRUE), list_dfs_salibs) |>
    filter(param != TRUE, field != TRUE)

  # Save csv
  write.csv(rbind_salibs, salib_csv_filepath, row.names = FALSE)
  if (overwrite) {
    custom_cat(paste0("File 'salib.csv' was overwriten!"))
  } else {
    custom_cat(paste0("File 'salib.csv' was created!"))
  }

  return(rbind_salibs)
}
