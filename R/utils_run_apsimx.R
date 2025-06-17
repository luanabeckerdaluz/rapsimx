just_run_apsimx <- function(
  apsimx_filepath,
  read_output = FALSE,
  simulations_names = NA,
  from_config_file = NA,
  xlsx_or_met_folder = NA,
  dry_run = FALSE) {

  # Check inputs
  if (!file.exists(apsimx_filepath)){
    cli::cli_alert_danger("ERROR! apsimx simulation {apsimx_filepath} does not exist!")
    stop()
  }

  # Copy all .xlsx or .met files to the same folder of simulation
  if (!is.na(xlsx_or_met_folder)) {
    files_to_copy <- list.files(xlsx_or_met_folder, pattern = "\\.(xlsx|met)$", full.names = TRUE)
    file.copy(files_to_copy, dirname(apsimx_filepath), overwrite = TRUE)
  }

  command <- NA
  if(!is.na(from_config_file)) {
    command <- glue::glue("{CONFIG_MODELS_COMMAND} --apply {from_config_file}")
  }
  else {
    command <- glue::glue("{CONFIG_MODELS_COMMAND} {apsimx_filepath}")
  }
  command <- glue::glue("{command} --single-threaded=FALSE --cpu-count={CONFIG_MULTICORES}")
  # If setting field names, concat names
  if (is.character(simulations_names)) {
    command <- glue::glue("{command} --simulation-names='{paste0(simulations_names, collapse = '|')}'")
  }

  # command <- (
  #     "docker run -i --rm --cpus='1.0' -v ",
  #     ":/ApsimX apsiminitiative/apsimng /ApsimX/",
  #     filename_sim,
  # )

  # Fake running file
  if (dry_run) {
    cli::cli_alert_success("It will run command '{command}'")
    return(NULL)
  }

  systemtime <- tryCatch(
    {
      systemtime <- system.time(system(command, intern = TRUE))
      systemtime
    },
    warning = function(w) {
      cli::cli_alert_warning(w$message)
      return(NULL)
    },
    error = function(e) {
      custom_error(e$message)
      return(NULL)
    }
  )

  if (is.null(systemtime)) {
    return(NULL)
  }

  if (read_output) {
    db_filepath <- sub("\\.apsimx$", ".db", apsimx_filepath)
    return(summarize_sim_db(db_filepath))
  } else {
    return(NA)
  }
}


run_apsimx <- function(
  apsimx_filepath,
  read_output = FALSE,
  simulations_names = NULL,
  from_config_file = NULL,
  xlsx_or_met_folder = NULL,
  models_command = NULL,
  multicores_cpu_count_for_command = NULL,
  dry_run = FALSE) {

  # Check inputs
  if (!file.exists(apsimx_filepath)){
    cli::cli_alert_danger("ERROR! apsimx simulation {apsimx_filepath} does not exist!")
    stop()
  }

  # Copy all .xlsx or .met files to the same folder of simulation
  if (!is.null(xlsx_or_met_folder)) {
    files_to_copy <- list.files(xlsx_or_met_folder, pattern = "\\.(xlsx|met)$", full.names = TRUE)
    file.copy(files_to_copy, dirname(apsimx_filepath), overwrite = TRUE)
  }

  command <- NULL
  if (!is.null(from_config_file)) {
    command <- glue::glue("{models_command} --apply {from_config_file}")
  } else {
    command <- glue::glue("{models_command} {apsimx_filepath}")
  }

  if (is.null(multicores_cpu_count_for_command)) {
    command <- glue::glue("{command} --single-threaded=FALSE --cpu-count=1")
  } else {
    command <- glue::glue("{command} --single-threaded=FALSE --cpu-count={multicores_cpu_count_for_command}")
  }

  # If setting field names, concat names
  if (is.character(simulations_names)) {
    command <- glue::glue("{command} --simulation-names='{paste0(simulations_names, collapse = '|')}'")
  }

  # command <- (
  #     "docker run -i --rm --cpus='1.0' -v ",
  #     ":/ApsimX apsiminitiative/apsimng /ApsimX/",
  #     filename_sim,
  # )

  # Fake running file
  if (dry_run) {
    cli::cli_alert_success("It will run command '{command}'")
    return(NULL)
  }

  systemtime <- tryCatch(
    {
      systemtime <- system.time(system(command, intern = TRUE))
      systemtime
    },
    warning = function(w) {
      cli::cli_alert_warning(w$message)
      return(NULL)
    },
    error = function(e) {
      cli::cli_alert_danger(e$message)
      return(NULL)
    }
  )

  if (is.null(systemtime)) {
    return(NULL)
  }

  if (read_output) {
    db_filepath <- sub("\\.apsimx$", ".db", apsimx_filepath)
    return(sensi_summarize_harvest_db(db_filepath))
  } else {
    return(NULL)
  }
}

run_apsimxs <- function(
  sims_folder,
  models_command,
  runs_only_some_n = NULL,
  simulations_names = NULL,
  ids_to_run = NULL,
  multicores = NULL,
  dry_run = FALSE) {

  # Check if simulation folder exists on sensi folder
  if (!file.exists(sims_folder)) {
    cli::cli_alert_danger("{basename(sims_folder)} folder does not exist!")
    stop()
  }

  # List files
  apsimx_filepaths <- list.files(
    path = sims_folder,
    pattern = ".apsimx",
    full.names = TRUE
  )

  # If ids_to_run is defined, summarize just for these ids
  if (is.numeric(ids_to_run)) {
    apsimx_filepaths <- apsimx_filepaths[grepl(
      paste(glue::glue("simulation{ids_to_run}.apsimx"), collapse = "|"),
      apsimx_filepaths
    )]
  }

  if (length(apsimx_filepaths) == 0) {
    cli::cli_alert_success("0 simulations to run. Returning...")
    return(NULL)
  }

  # If necessary, filter df to run just N sims
  if (!is.null(runs_only_some_n)) {
    if (!is.integer(runs_only_some_n)) {
      cli::cli_alert_danger("'runs_only_some_n' must be an integer number (e.g. '5L')")
      stop()
    }
    if (runs_only_some_n > length(apsimx_filepaths)) {
      cli::cli_alert_warning("runs_only_some_n parameter [{runs_only_some_n}] must be lower or equal than apsimx files count [{length(apsimx_filepaths)}]")
    }
    if (length(apsimx_filepaths) > runs_only_some_n) {
      apsimx_filepaths <- apsimx_filepaths[1:runs_only_some_n]
    }
  }

  cli::cli_alert_success("Running {length(apsimx_filepaths)} apsimx simulations...")

  res <- .lapply_parallel_progressbar(
    x_must_be_num_array = seq_along(apsimx_filepaths),
    FUN = function(i) {
      run_apsimx(
        apsimx_filepath = apsimx_filepaths[i],
        simulations_names = simulations_names,
        dry_run = dry_run,
        models_command = models_command,
        multicores_cpu_count_for_command = multicores
      )
    },
    multicores = multicores
  )
}

generate_apsimx <- function(
  list_params_values,
  id,
  folder,
  sensit_base_sim_filepath,
  dry_run = FALSE) {

  # Check if base sim exists
  if (!file.exists(sensit_base_sim_filepath)) {
    stop("ERROR! Base sim does not exist!")
  }

  # Copy base simulation to tmp folder
  filepath_sim <- file.path(folder, glue::glue("simulation{id}.apsimx"))

  # If dry_run, print and return
  if (dry_run) {
    cli::cli_alert_success("It will generate file {filepath_sim}")
    return(NULL)
  }

  # Copy base simulation to new sim that will be modified
  file.copy(
    sensit_base_sim_filepath,
    filepath_sim,
    overwrite = TRUE
  )

  # Replace parameters
  rapsimx::replace_values(
    apsimx_path = filepath_sim,
    VERBOSE = FALSE,
    list_params_values = list_params_values
  )

  return(filepath_sim)
}

.lapply_parallel_progressbar <- function(x_must_be_num_array, FUN, multicores = NULL) {
  if (!is.null(multicores)) {
    cli::cli_alert_success("Running in parallel with {multicores} cores")
  }

  # Create progress bar
  pb_generate <- NULL
  if (is.null(multicores)) {
    pb_generate <- txtProgressBar(min = 0, max = length(x_must_be_num_array), style = 3)
  }

  # Run lapply
  lapply_arguments <- list(
    X = x_must_be_num_array,
    FUN = function(i) {
      res <- FUN(i)
      if (is.null(multicores)) setTxtProgressBar(pb_generate, i)
      return(res)
    }
  )

  # Configure parallel or sequential
  if (!is.null(multicores)) {
    cl <- parallel::makeCluster(multicores)
    future::plan(future::cluster, workers = cl)
    res <- do.call(future.apply::future_lapply, lapply_arguments)
    parallel::stopCluster(cl)
  } else {
    # future::plan(future::sequential)
    # res <- do.call(future.apply::future_lapply, lapply_arguments)
    res <- do.call(lapply, lapply_arguments)
  }

  # Close progress bar
  if (is.null(multicores)) close(pb_generate)

  return(res)
}