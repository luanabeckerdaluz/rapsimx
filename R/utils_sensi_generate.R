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
  rapsimx.run::replace_values(
    apsimx_path = filepath_sim,
    VERBOSE = FALSE,
    list_params_values = list_params_values
  )

  return(filepath_sim)
}

generate_apsimxs <- function(
  sensi_folder,
  sensit_base_sim_filepath,
  runs_only_some_n = NA,
  parallel = TRUE,
  dry_run = FALSE) {

  # Stop if base sim does not exist
  if (!file.exists(sensit_base_sim_filepath)){
    cli::cli_alert_danger("Base simulation {sensit_base_sim_filepath} does not exist!")
    stop()
  }

  # Check if sensi_folder exist
  if (!file.exists(sensi_folder)){
    cli::cli_alert_danger("Sensi folder {sensi_folder} does not exist!")
    stop()
  }

  # Load samples.csv
  samples_csv_filepath <- file.path(sensi_folder, "samples.csv")
  if (!file.exists(samples_csv_filepath)) {
    cli::cli_alert_danger("File 'samples.csv' does not exist on {sensi_folder} folder! Aborting loading...")
    stop()
  } else {
    cli::cli_alert_success("Loading 'samples.csv' from folder {sensi_folder}")
    samples_df <- read.csv(samples_csv_filepath, row.names = NULL)
  }

  sims_folder <- file.path(sensi_folder, "sims_and_met")
  # Stop if simulations folder does not exist on sensi folder
  if (!file.exists(sims_folder)) {
    cli::cli_alert_danger("sensi folder {sims_folder} does not exist!")
    stop()
  }

  if (nrow(samples_df) == 0) {
    cli::cli_alert_success("Samples df has 0 rows. Returning...")
    return(NULL)
  }
  # If necessary, filter df to run just N sims
  if (!is.na(runs_only_some_n)) {
    if (!is.integer(runs_only_some_n)) {
      cli::cli_alert_danger("'runs_only_some_n' must be an integer number (e.g. '5L')")
      stop()
    }
    if (runs_only_some_n > nrow(samples_df)) {
      cli::cli_alert_danger("runs_only_some_n parameter [{runs_only_some_n}] must be lower or equal than nrow of samples [{nrow(samples_df)}]")
      stop()
    }
    if (nrow(samples_df) > runs_only_some_n) {
      samples_df <- samples_df[1:runs_only_some_n, ]
    }
  }

  cli::cli_alert_success("Generating {nrow(samples_df)} samples...")

  res <- .lapply_parallel_progressbar(
    x_must_be_num_array = seq_len(nrow(samples_df)),
    FUN = function(i) {
      list_params_values <- samples_df[i, , drop = TRUE]
      generate_apsimx(
        list_params_values = list_params_values[2:length(list_params_values)],
        id = as.numeric(list_params_values[["id"]]),
        folder = sims_folder,
        sensit_base_sim_filepath = sensit_base_sim_filepath,
        dry_run = dry_run
      )
    },
    parallel = parallel
  )

  # Print folder stats
  print_stats_of_folder(sims_folder)
}
