sensi_load_problem <- function(folder = NA) {
  # Try loading problem from folder
  problem_filepath <- file.path(folder, "problem.R")
  if (file.exists(problem_filepath)) {
    cli::cli_alert_success("'problem.R' file exists on {folder} folder! Loading...")
    source(problem_filepath)
  } else {
    cli::cli_alert_danger("'problem.R' file doesn't exist on {folder} folder! Please, specify a valid folder!")
    stop()
  }
  return(problem)
}

sensi_load_problem_current_folder <- function() {
  folder <- getwd()
  sensi_load_problem(folder)
}



sensi_generate_samples_csv <- function(
  problem = NA,
  method = NA,
  N_SAMPLES = NA,
  overwrite = FALSE,
  save_csv_to_folder) {

  # Skip generating samples if "samples.csv" already exists on sensi folder
  samples_csv_filepath <- file.path(save_csv_to_folder, "samples.csv")
  if (file.exists(samples_csv_filepath)) {
    if (overwrite) {
      cli::cli_alert_warning("'samples.csv' file already exists on {save_csv_to_folder} folder! However, it will be overwritten because 'overwrite' parameter was set as TRUE!")
    } else {
      cli::cli_alert_danger("'samples.csv' file already exists on {save_csv_to_folder} folder! Please, if you want to create new samples or overwrite, set 'overwrite' to TRUE, create a new sensi folder or delete existing file!")
      stop()
    }
  } else {
    cli::cli_alert_success("'samples.csv' file doesn't exist. Generating...")
    overwrite <- FALSE
  }

  # Check if required parameters are missing
  if (is.na(problem) || is.na(method) || is.na(N_SAMPLES)) {
    cli::cli_alert_danger("ERROR: Parameters 'problem', 'method' and 'N_SAMPLES' should not be NA.")
    stop()
  }

  if (!is.integer(N_SAMPLES)) {
    cli::cli_alert_danger("'N_SAMPLES' must be an integer (e.g. '100L')")
    stop()
  }

  # Set seed
  set.seed(1111)

  # method = "LHS", "FAST", "SOBOL"
  if (method == "LHS") {
    convert_row_to_var_bounds <- function(row) {
      for (i in seq_len(length(row))) {
        lb <- variable_bounds[[i]][1]
        ub <- variable_bounds[[i]][2]
        range <- ub - lb
        row[i] <- lb + row[i] * range
      }
      return(row)
    }
    N_VARS <- as.integer(problem$num_vars)
    # df_samples_norm <- import("scipy.stats.qmc")$LatinHypercube(d = N_VARS)$random(n = N_SAMPLES)
    df_samples_norm <- lhs::randomLHS(N_SAMPLES, N_VARS)
    # Convert df to var bounds
    for (i in seq_len(nrow(df_samples_norm))) {
      df_samples_norm[i, ] <- convert_row_to_var_bounds(df_samples_norm[i, ])
    }
    samples <- df_samples_norm
  } else if (method == "FAST") {
    sampler <- reticulate::import("SALib.sample.fast_sampler")
    samples <- sampler$sample(problem, N_SAMPLES)
  } else if (method == "SOBOL") {
    sampler <- reticulate::import("SALib.sample.sobol")
    samples <- sampler$sample(problem, N_SAMPLES, calc_second_order = TRUE)
  } else {
    stop("invalid option! Available: LHS, FAST and SOBOL")
  }
  head(samples)

  samples_df <- as.data.frame(samples) |>
    `colnames<-`(problem$names) |>
    dplyr::mutate(id = as.integer(rownames(.))) |>
    dplyr::select(id, everything())
  dim(samples_df)
  head(samples_df)

  # Save csv
  write.csv(samples_df, samples_csv_filepath, row.names = FALSE)
  if (overwrite) {
    cli::cli_alert_success("File 'samples.csv' was overwriten!")
  } else {
    cli::cli_alert_success("File 'samples.csv' was created!")
  }

  # Copy problem to sensi folder
  saveRDS(problem, file.path(save_csv_to_folder, "problem.rds"))
  cat("problem <- ", deparse(problem), file = file.path(save_csv_to_folder, "problem.R"))
  if (overwrite) {
    cli::cli_alert_success("Files 'problem.R' and 'problem.rds' were overwriten!")
  } else {
    cli::cli_alert_success("Files 'problem.R' and 'problem.rds' were created!")
  }
}


