create_tmp_dir <- function(folderpath, copy_met_data_from, overwrite = FALSE) {
  # normalize folderpath
  folderpath <- normalizePath(folderpath)

  # If tmp folder doesn't exist, create and copy met data
  if (overwrite || !file.exists(folderpath)) {
    dir.create(folderpath, recursive = TRUE, showWarnings = FALSE)
    # Create met data folder
    met_data_folderpath <- file.path(folderpath, "met_data")
    dir.create(met_data_folderpath, recursive = TRUE, showWarnings = FALSE)
    # Copy met data
    met_filepaths <- list.files(copy_met_data_from, full.names = TRUE)
    file.copy(
      met_filepaths,
      met_data_folderpath,
      recursive = TRUE,
      overwrite = TRUE
    )
  }
  else {
    custom_stop(paste0(folderpath, " folder already exists! Please choose a new folder name or set overwrite parameter to TRUE!"))
  }

  return(folderpath)
}

print_stats_of_folder <- function(folder_path){
  custom_summary("Summary:")
  custom_summary(paste0("  Folder = ", folder_path))

  # Check number of dbs, csvs and apsimx files in folder
  custom_summary(paste("  Number of csvs:", length(list.files(folder_path, pattern = "HarvestReport"))))
  custom_summary(paste("  Number of dbs:", length(list.files(folder_path, pattern = ".db"))))
  apsimx_filepaths <- list.files(folder_path, pattern = ".apsimx", full.names = TRUE)
  custom_summary(paste("  Number of apsimxs:", length(apsimx_filepaths)))
}

lapply_parallel_progressbar <- function(X_must_be_num_array, FUN, parallel = FALSE) {
  if (parallel) {
    custom_cat(paste0("Running in parallel with ", CONFIG_MULTICORES, " cores"))
  } else {
    custom_cat("Not using parallel")
  }

  # Create progress bar
  pb_generate <- NULL
  if (!parallel) {
    pb_generate <- txtProgressBar(min = 0, max = length(X_must_be_num_array), style = 3)
  }

  # Run lapply
  lapply_arguments <- list(
    X = X_must_be_num_array,
    FUN = function(i) {
      FUN(i)
      if (!parallel) setTxtProgressBar(pb_generate, i)
    }
  )

   # Configure parallel or sequential
  if (parallel) {
    cl <- parallel::makeCluster(CONFIG_MULTICORES)
    future::plan(future::cluster, workers = cl)
    res <- do.call(future.apply::future_lapply, lapply_arguments)
    parallel::stopCluster(cl)
  } else {
    # future::plan(future::sequential)
    # res <- do.call(future.apply::future_lapply, lapply_arguments)
    res <- do.call(lapply, lapply_arguments)
  }

  # Close progress bar
  if (!parallel) close(pb_generate)

  return(res)
}
