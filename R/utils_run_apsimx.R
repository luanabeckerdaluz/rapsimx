rapsimx_wrapper_options <- function(
  apsimx_path,
  apsimx_file,
  variable_names = character(0),
  predicted_table_name = 'Report',
  observed_table_name = 'Observed',
  xlsx_or_met_files_path = character(0),
  multicores = NULL,
  verbose = FALSE,
  ...) {

  #
  # Default Options
  #
  options <- list()
  options$apsimx_path <- apsimx_path
  options$apsimx_file <- apsimx_file
  options$variable_names <- variable_names
  options$predicted_table_name <- predicted_table_name
  options$observed_table_name <- observed_table_name

  #
  # Custom Options
  #
  options$xlsx_or_met_files_path <- xlsx_or_met_files_path
  options$verbose <- verbose
  options$multicores <- multicores

  #
  # Legacy Options from apsimx_wrapper_options
  #
  options$warning_display <- TRUE
  options$multi_process <- FALSE
  options$time_display <- FALSE
  if ( ! is.element("Clock.Today", options$variable_names ) ) {
    if (length(options$variable_names) > 0 && grepl('Predicted.', options$variable_names[1])) {
      options$variable_names <- c(options$variable_names, "Predicted.Clock.Today")
    } else {
      options$variable_names <- c(options$variable_names, "Clock.Today")
    }
  }

  return(options)
}

rapsimx_wrapper <- function(
  model_options,
  sit_names = NULL,
  param_values = NULL,
  ...) {

  # Fetch inputs
  apsimx_path <- model_options$apsimx_path
  apsimx_file <- model_options$apsimx_file
  variable_names <- model_options$variable_names
  predicted_table_name <- model_options$predicted_table_name
  # observed_table_name <- model_options$observed_table_name
  xlsx_or_met_files_path <- model_options$xlsx_or_met_files_path
  multicores <- model_options$multicores
  verbose <- model_options$verbose

  if (verbose) {
    cli::cli_alert_warning("rapsimx_wrapper | sit_names = {sit_names}")
    cli::cli_alert_warning("rapsimx_wrapper | param_values = {param_values}")
    dots <- list(...)
    if (length(dots) > 0) {
      for (nm in names(dots)) {
        cli::cli_alert_warning("rapsimx_wrapper | ... {nm} = {dots[[nm]]}")
      }
    }
  }

  # Check multicores input
  if (!(is.null(multicores) || (is.numeric(multicores) && multicores == as.integer(multicores)))) {
    cli::cli_alert_danger("run_apsimxs | Input Error: When setting multicores, it must be a integer number (e.g. '5' or '5L')")
    stop()
  }

  start_time <- Sys.time()

  tmp_dir <- tempdir()
  if (verbose) cli::cli_alert_success("rapsimx_wrapper | Creating tmp dir {tmp_dir}")

  new_apsimx_filepath <- generate_apsimx(
    list_params_values = param_values,
    id = paste0(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = ""),
    folder = tmp_dir,
    sensit_base_sim_filepath = apsimx_file,
    verbose = verbose
  )

  ret <- run_apsimx(
    apsimx_filepath = new_apsimx_filepath,
    read_output = FALSE,
    simulations_names = sit_names,
    xlsx_or_met_folder = xlsx_or_met_files_path,
    models_command = apsimx_path,
    multicores = multicores,
    verbose = verbose
  )
  if (verbose) cli::cli_alert_success("run_apsimx function returned object class {class(ret)}!")

  #
  # Read results
  #
  db_filepath <- gsub(".apsimx", ".db", new_apsimx_filepath)

  res <- NULL
  res$db_file_name <- db_filepath
  # res$sim_list <- read_db_table(
  res$sim_list <- read_apsimx_output(
    db_filepath = db_filepath,
    table_name = predicted_table_name,
    verbose = verbose,
    variables = variable_names
  )

  # If defined, display time
  if (model_options$time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  #
  # Legacy
  #
  #   Add the attribute cropr_simulation for using CroPlotR package
  #
  if (length(res$sim_list) > 0) {
    attr(res$sim_list, "class") <- "cropr_simulation"
  }

  if (verbose) cli::cli_alert_success("Returning res object class {class(res)}")

  return(res)
}


read_apsimx_output <- function(db_filepath, table_name, variables, sim_names = NULL, verbose = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_filepath)

  vars <- paste(sprintf("%s.[%s], ",table_name, variables), collapse = "")
  vars <- paste(vars,"_Simulations.Name as SimulationName")

  sql <- paste0('SELECT ', vars, ' FROM ', table_name, ', _Simulations WHERE _Simulations.ID = ', table_name, '.SimulationID')
  data <- DBI::dbGetQuery(con, sql)
  DBI::dbDisconnect(con)
  simulationNames <- unique(data$SimulationName)

  # Selecting simulations
  if (!is.null(sim_names)) {
    sim_idx <- simulationNames %in% sim_names
    simulationNames <- simulationNames[sim_idx]
  }

  # Creating empty list
  sim_nb <- length(simulationNames)
  tables <- vector("list", sim_nb)

  # Filling it with results
  for (i in 1:sim_nb) {
    sim <- simulationNames[i]
    tables[[i]] <- data[which(data$SimulationName == sim), ] %>% select(one_of(variables))
    if ("Clock.Today" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Clock.Today)) %>%
        select(-Clock.Today)
    } else if ("Date" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Date))
    } else if ("Predicted.Clock.Today" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Predicted.Clock.Today)) %>%
        select(-Predicted.Clock.Today)
    }
  }

  names(tables) <- simulationNames
  return(tables)
}


run_apsimx <- function(
  apsimx_filepath,
  read_output = FALSE,
  simulations_names = NULL,
  from_config_file = NULL,
  xlsx_or_met_folder = NULL,
  models_command = NULL,
  multicores = NULL,
  verbose = FALSE,
  dry_run = FALSE) {

  # Check inputs
  if (!file.exists(apsimx_filepath)){
    cli::cli_alert_danger("ERROR! apsimx simulation {apsimx_filepath} does not exist!")
    stop()
  }
  if (!(is.null(multicores) || (is.numeric(multicores) && multicores == as.integer(multicores)))) {
    cli::cli_alert_danger("run_apsimxs | Input Error: When setting multicores, it must be a integer number (e.g. '5' or '5L')")
    stop()
  }

  # # Test APSIMx Models executable
  # cmd <- paste(models_command, "--version")
  # val <- system(cmd, wait = TRUE, intern = TRUE)
  # if (!is.null(attr(val, "status"))) {
  #   cli::cli_alert_danger("ERROR! {apsimx_path} is not executable or is not an apsimx executable!")
  #   stop()
  # }

  # Copy all .xlsx or .met files to the same folder of simulation
  if (!is.null(xlsx_or_met_folder)) {
    files_to_copy <- list.files(xlsx_or_met_folder, pattern = "\\.(xlsx|met)$", full.names = TRUE)
    file.copy(files_to_copy, dirname(apsimx_filepath), overwrite = TRUE)
    if (verbose) cli::cli_alert_success("run_apsimx | Copied xlsx|met from folder {xlsx_or_met_folder} to folder {dirname(apsimx_filepath)}")
  }

  # Set .db filename
  db_filepath <- sub("\\.apsimx$", ".db", apsimx_filepath)
  if (verbose) cli::cli_alert_success("run_apsimx | db_filepath = {db_filepath}")

  # Remove .db file if it already exists
  if (file.exists(db_filepath)) {
    file.remove(db_filepath)
    if (verbose) cli::cli_alert_success("run_apsimx | {db_filepath} exists! So, deleting...")
  }

  command <- glue::glue("{models_command} {apsimx_filepath}")

  # Check if multicores parameter was set
  if (is.null(multicores)) {
    command <- glue::glue("{command} --single-threaded=TRUE --cpu-count=1")
  } else {
    command <- glue::glue("{command} --single-threaded=FALSE --cpu-count={multicores}")
  }

  # If setting field names, concat names
  if (is.character(simulations_names)) {
    command <- glue::glue("{command} --simulation-names='{paste0(simulations_names, collapse = '|')}'")
    if (verbose) cli::cli_alert_success("run_apsimx | Added simulations_names")
  }
  # If using conf file, use --apply
  if (!is.null(from_config_file)) {
    command <- glue::glue("{command} --apply {from_config_file}")
    if (verbose) cli::cli_alert_success("run_apsimx | Added --apply to command")
  }

  # command <- (
  #     "docker run -i --rm --cpus='1.0' -v ",
  #     ":/ApsimX apsiminitiative/apsimng /ApsimX/",
  #     filename_sim,
  # )

  # Fake running file
  if (dry_run) {
    return(NULL)
  }
  if (verbose) cli::cli_alert_success("run_apsimx | It will run command '{command}'")

  # Run CMD
  result <- NULL
  systemtime <- system.time({
    result <- tryCatch(
      system(command, intern = TRUE),
      warning = function(w) { cli::cli_alert_warning(w$message); NULL },
      error   = function(e) { cli::cli_alert_danger(e$message); NULL }
    )
  })
  if (verbose) cli::cli_alert_success("run_apsimx | command was executed!")

  if (is.null(result)) {
    cli::cli_alert_danger("ERROR while running simulation!")
    return(NULL)
  }

  if (read_output) {
    if (verbose) cli::cli_alert_success("run_apsimx | Reading output...")
    return(sensi_summarize_harvest_db(db_filepath))
  }

  return(TRUE)
}

run_apsimxs <- function(
  sims_folder,
  models_command,
  runs_only_some_n = NULL,
  simulations_names = NULL,
  ids_to_run = NULL,
  multicores = NULL,
  verbose = FALSE,
  dry_run = FALSE) {

  # Check if simulation folder exists on sensi folder
  if (!file.exists(sims_folder)) {
    cli::cli_alert_danger("{basename(sims_folder)} folder does not exist!")
    stop()
  }

  # Check multicores parameter
  if (!(is.null(multicores) || (is.numeric(multicores) && multicores == as.integer(multicores)))) {
    cli::cli_alert_danger("run_apsimxs | Input Error: When setting multicores, it must be a integer number (e.g. '5' or '5L')")
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
    if (verbose) cli::cli_alert_success("run_apsimxs | Setting multiple ids")
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
        multicores = multicores
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
  verbose = FALSE,
  dry_run = FALSE) {

  # Check if base sim exists
  if (!file.exists(sensit_base_sim_filepath)) {
    stop("ERROR! Base sim does not exist!")
  }

  # Copy base simulation to tmp folder
  filepath_sim <- file.path(folder, glue::glue("simulation{id}.apsimx"))
  if (verbose) cli::cli_alert_success("generate_apsim | filepath_sim = {filepath_sim}")

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
  if (verbose) cli::cli_alert_success("generate_apsim | copied {sensit_base_sim_filepath} to {filepath_sim}")

  # Replace parameters
  replace_values(
    apsimx_path = filepath_sim,
    VERBOSE = FALSE,
    list_params_values = list_params_values
  )
  if (verbose) cli::cli_alert_success("generate_apsim | replaced values")

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
    # Set parallel cluster
    cl <- parallel::makeCluster(multicores)
    future::plan(future::cluster, workers = cl)
    # Set future seed true
    lapply_arguments$future.seed <- TRUE
    # Run parallel lapply
    res <- do.call(future.apply::future_lapply, lapply_arguments)
    # Stop parallel cluster
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