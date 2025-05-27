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

run_apsimxs <- function(
  sensi_folder,
  runs_only_some_n = NA,
  simulations_names = NA,
  ids_to_run = NA,
  parallel = TRUE,
  dry_run = FALSE) {

  # Check if simulation folder exists on sensi folder
  sims_folder <- file.path(sensi_folder, "sims_and_met")
  if (!file.exists(sims_folder)) {
    cli::cli_alert_danger("{basename(sims_folder)} folder does not exist on sensi folder {sensi_folder}!")
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
  if (!is.na(runs_only_some_n)) {
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

  res <- lapply_parallel_progressbar(
    x_must_be_num_array = seq_along(apsimx_filepaths),
    FUN = function(i) {
      just_run_apsimx(
        apsimx_filepath = apsimx_filepaths[i],
        simulations_names = simulations_names,
        dry_run = dry_run
      )
    },
    parallel = parallel
  )

  # Print folder stats
  print_stats_of_folder(sims_folder)
}