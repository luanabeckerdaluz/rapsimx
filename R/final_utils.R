print_stats_of_folder <- function(folder_path) {
  cli::cli_alert_info("Summary:")
  cli::cli_alert_info("  Folder = {folder_path}")

  # Check number of dbs, csvs and apsimx files in folder
  cli::cli_alert_info("  Number of csvs: {length(list.files(folder_path, pattern = 'HarvestReport'))}")
  cli::cli_alert_info("  Number of dbs: {length(list.files(folder_path, pattern = '.db'))}")
  apsimx_filepaths <- list.files(folder_path, pattern = ".apsimx", full.names = TRUE)
  cli::cli_alert_info("  Number of apsimxs: {length(apsimx_filepaths)}")
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
  rapsimx.run::replace_values(
    apsimx_path = filepath_sim,
    VERBOSE = FALSE,
    list_params_values = list_params_values
  )

  return(filepath_sim)
}

.lapply_parallel_progressbar <- function(x_must_be_num_array, FUN, multicores = NA) {
  if (is.na(multicores)) {
    cli::cli_alert_success("Running in parallel with {multicores} cores")
  } else {
    cli::cli_alert_success("Not using parallel")
  }

  # Create progress bar
  pb_generate <- NULL
  if (is.na(multicores)) {
    pb_generate <- txtProgressBar(min = 0, max = length(x_must_be_num_array), style = 3)
  }

  # Run lapply
  lapply_arguments <- list(
    X = x_must_be_num_array,
    FUN = function(i) {
      res <- FUN(i)
      if (is.na(multicores)) setTxtProgressBar(pb_generate, i)
      return(res)
    }
  )

  # Configure parallel or sequential
  if (!is.na(multicores)) {
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
  if (is.na(multicores)) close(pb_generate)

  return(res)
}