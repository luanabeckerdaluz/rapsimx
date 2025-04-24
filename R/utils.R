print_stats_of_folder <- function(folder_path) {
  custom_summary("Summary:")
  custom_summary(paste0("  Folder = ", folder_path))

  # Check number of dbs, csvs and apsimx files in folder
  custom_summary(paste("  Number of csvs:", length(list.files(folder_path, pattern = "HarvestReport"))))
  custom_summary(paste("  Number of dbs:", length(list.files(folder_path, pattern = ".db"))))
  apsimx_filepaths <- list.files(folder_path, pattern = ".apsimx", full.names = TRUE)
  custom_summary(paste("  Number of apsimxs:", length(apsimx_filepaths)))
}

lapply_parallel_progressbar <- function(x_must_be_num_array, FUN, parallel = FALSE) {
  if (parallel) {
    custom_cat(paste0("Running in parallel with ", CONFIG_MULTICORES, " cores"))
  } else {
    custom_cat("Not using parallel")
  }

  # Create progress bar
  pb_generate <- NULL
  if (!parallel) {
    pb_generate <- txtProgressBar(min = 0, max = length(x_must_be_num_array), style = 3)
  }

  # Run lapply
  lapply_arguments <- list(
    X = x_must_be_num_array,
    FUN = function(i) {
      res <- FUN(i)
      if (!parallel) setTxtProgressBar(pb_generate, i)
      return(res)
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