
RApsimxSensitivity <- function(folder, copy_met_data_from = NULL, multicores = NULL, models_command = NULL) {
  RApsimxSensitivityClass$new(folder, copy_met_data_from, multicores, models_command)
}

RApsimxSensitivityClass <- R6::R6Class("RApsimxSensitivityClass",
  public = list(
    folder = NULL,
    sims_and_mets_folderpath = NULL,
    problem_filepath = NULL,
    samples_csv_filepath = NULL,
    salib_csv_filepath = NULL,
    summarize_csv_filepath = NULL,

    problem = NULL,
    samples_df = NULL,
    salib_df = NULL,
    summarize_df = NULL,
    multicores = NULL,
    models_command = NULL,

    teste = NULL,

    initialize = function(folder, copy_met_data_from = NULL, multicores = NULL, models_command = NULL) {
      self$folder <- normalizePath(folder, mustWork = FALSE)
      self$problem_filepath <- file.path(self$folder, "problem.rds")
      self$samples_csv_filepath <- file.path(self$folder, "samples.csv")
      self$summarize_csv_filepath <- file.path(self$folder, "summarized.csv")
      self$salib_csv_filepath <- file.path(self$folder, "salib.csv")
      self$sims_and_mets_folderpath <- file.path(self$folder, "sims_and_mets")

      # Config multicores
      if (!is.null(multicores)) {
        if (multicores >= parallel::detectCores()) {
          cli::cli_alert_danger("ERROR: Multicores parameter ({multicores}) is greater or equal than detected multicores ({parallel::detectCores()})")
          stop()
        }
        self$multicores <- multicores
      }

      # Set apsimx Models command
      if (!is.null(models_command)) {
        self$models_command <- models_command
      } else {
        # Set apsimx Models command path based on system info
        if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "Darwin") {
          self$models_command <- "/usr/local/bin/Models"
        } else if (Sys.info()["sysname"] == "Windows") {
          # TODO: Detect Windows apsimx path
          self$models_command <<- "C:\\APSIM2024.5.7504.0\\bin\\Models.exe"
        }
      }

      # Create sensi folder
      if (!dir.exists(self$folder)) {
        cli::cli_alert_success("Generating {basename(self$folder)}...")
        private$generate_base_folder()
        private$generate_sims_subfolder()
      } else {
        cli::cli_alert_success("Loading {basename(self$folder)} folder...")
        if (!dir.exists(self$sims_and_mets_folderpath)) {
          private$generate_sims_subfolder()
        }
        private$try_load_problem()
        private$try_load_samples_csv()
        private$try_load_summarize_csv()
        private$try_load_salib_csv()
      }

      # Copy met data to "sims_and_met" folder
      if (!is.null(copy_met_data_from)) {
        private$import_met_data_to_sims_folder(copy_met_data_from)
      }


      self$info()
    },

    info = function() {
      cli::cli_alert_success("Folder: {self$folder}")
      if (!is.null(self$multicores)) {
        cli::cli_alert_success("Parallel enabled! Multicores = {self$multicores}")
      }
      cli::cli_alert_success("ApsimX command = {self$models_command}")
      private$check_file_exist_with_cli(self$problem_filepath)
      private$check_file_exist_with_cli(self$samples_csv_filepath)
      private$check_file_exist_with_cli(self$summarize_csv_filepath)
      private$check_file_exist_with_cli(self$salib_csv_filepath)

      # Print sims_and_mets folder summary
      private$files_summary()
    },

    import_or_overwrite_problem = function(filepath) {
      if (private$check_file_exist_with_cli(filepath)) {
        source(filepath)
        self$problem <- problem
        saveRDS(problem, self$problem_filepath)
        cli::cli_alert_success("problem was updated!")
      }
    },

    generate_samples = function(method, n_samples) {
      # Check if problem exists
      if (is.null(self$problem)) {
        cli::cli_alert_danger("ERROR: Problem was not detected to generate samples. Please run import_or_overwrite_problem function before generate samples.")
        stop()
      }

      # Skip generating samples if "samples.csv" already exists on sensi folder
      if (file.exists(self$samples_csv_filepath)) {
        cli::cli_alert_warning("Samples file {basename(self$samples_csv_filepath)} already exists! However, it will be overwritten.")
      } else {
        cli::cli_alert_success("{basename(self$samples_csv_filepath)} file doesn't exist. Generating...")
      }

      if (!is.integer(n_samples)) {
        cli::cli_alert_danger("'n_samples' parameter must be an integer (e.g. '100L')")
        stop()
      }

      # Set seed
      set.seed(1111)

      # method = "LHS", "FAST", "SOBOL"
      if (method == "LHS") {
        convert_row_to_var_bounds <- function(row) {
          for (i in seq_len(length(row))) {
            lb <- self$problem$bounds[[i]][1]
            ub <- self$problem$bounds[[i]][2]
            range <- ub - lb
            row[i] <- lb + row[i] * range
          }
          return(row)
        }
        N_VARS <- as.integer(self$problem$num_vars)
        # df_samples_norm <- import("scipy.stats.qmc")$LatinHypercube(d = N_VARS)$random(n = n_samples)
        df_samples_norm <- lhs::randomLHS(n_samples, N_VARS)
        # Convert df to var bounds
        for (i in seq_len(nrow(df_samples_norm))) {
          df_samples_norm[i, ] <- convert_row_to_var_bounds(df_samples_norm[i, ])
        }
        samples <- df_samples_norm
      } else if (method == "FAST") {
        sampler <- reticulate::import("SALib.sample.fast_sampler")
        samples <- sampler$sample(self$problem, n_samples)
      } else if (method == "SOBOL") {
        sampler <- reticulate::import("SALib.sample.sobol")
        samples <- sampler$sample(self$problem, n_samples, calc_second_order = TRUE)
      } else {
        stop("invalid option! Available: LHS, FAST and SOBOL")
      }
      # print(head(samples))
      # print(self$problem$names)

      self$samples_df <- as.data.frame(samples) |>
        setNames(self$problem$names) |>
        tibble::rownames_to_column(var = "id") |>
        dplyr::select(id, everything())

      # Save csv
      write.csv(self$samples_df, self$samples_csv_filepath, row.names = FALSE)
      cli::cli_alert_success("File {basename(self$samples_csv_filepath)} saved!")
    },

    samples_df.plot = function() {
      if (is.null(self$samples_df)) {
        cli::cli_alert_danger("Samples were not loaded! Please run generate_samples().")
      }

      plt <- self$samples_df %>%
        tidyr::pivot_longer(cols = -id, names_to = "variable", values_to = "value") %>%
        ggplot2::ggplot(ggplot2::aes(x = seq_len(nrow(.)), y = value)) +
          ggplot2::facet_wrap(variable ~ ., scale = "free_y") +
          ggplot2::geom_point(size = 1)

      return(plt)
    },

    generate_apsimxs = function(sensit_base_sim_filepath, runs_only_some_n = NULL, dry_run = FALSE) {
      # Stop if base sim does not exist
      if (!file.exists(sensit_base_sim_filepath)){
        cli::cli_alert_danger("Base simulation {sensit_base_sim_filepath} does not exist!")
        stop()
      }

      if (nrow(self$samples_df) == 0) {
        cli::cli_alert_success("Samples df has 0 rows. Returning...")
        return(NULL)
      }

      samples_df_to_run <- self$samples_df

      # If necessary, filter df to run just N sims
      if (!is.null(runs_only_some_n)) {
        if (!is.integer(runs_only_some_n)) {
          cli::cli_alert_danger("'runs_only_some_n' must be an integer number (e.g. '5L')")
          stop()
        }
        if (runs_only_some_n > nrow(self$samples_df)) {
          cli::cli_alert_danger("runs_only_some_n parameter [{runs_only_some_n}] must be lower or equal than nrow of samples [{nrow(self$samples_df)}]")
          stop()
        }
        if (nrow(self$samples_df) > runs_only_some_n) {
          samples_df_to_run <- self$samples_df[1:runs_only_some_n, ]
        }
      }

      cli::cli_alert_success("Generating {nrow(samples_df_to_run)} samples...")

      res <- .lapply_parallel_progressbar(
        x_must_be_num_array = seq_len(nrow(samples_df_to_run)),
        FUN = function(i) {
          list_params_values <- samples_df_to_run[i, , drop = TRUE]
          generate_apsimx(
            list_params_values = list_params_values[2:length(list_params_values)],
            id = as.numeric(list_params_values[["id"]]),
            folder = self$sims_and_mets_folderpath,
            sensit_base_sim_filepath = sensit_base_sim_filepath,
            dry_run = dry_run
          )
        },
        multicores = self$multicores
      )

      # Print sims_and_mets folder summary
      private$files_summary()
    },

    run = function(runs_only_some_n = NULL, simulations_names = NULL, ids_to_run = NULL, dry_run = FALSE) {
      run_apsimxs(
        sims_folder = self$sims_and_mets_folderpath,
        runs_only_some_n = runs_only_some_n,
        simulations_names = simulations_names,
        ids_to_run = ids_to_run,
        multicores = self$multicores,
        models_command = self$models_command,
        dry_run = dry_run
      )

      # Print sims_and_mets folder summary
      private$files_summary()
    },

    summarize = function(ids_to_summarize = NULL, number_of_fields_to_check = NULL, runs_only_some_n = NULL, dry_run = FALSE) {
      # Check if parameters are integer
      if (!is.null(number_of_fields_to_check) && !is.integer(number_of_fields_to_check)) {
        cli::cli_alert_danger("'number_of_fields_to_check' must be an integer (e.g. '32L')")
        stop()
      }
      if (!is.null(runs_only_some_n) && !is.integer(runs_only_some_n)) {
        cli::cli_alert_danger("'runs_only_some_n' must be an integer (e.g. '5L')")
        stop()
      }

      # Skip summarize if "summarized.csv" already exists on folder
      if (file.exists(self$summarize_csv_filepath)) {
        cli::cli_alert_warning("{basename(self$summarize_csv_filepath)} file already exists. Overwriting...")
      } else {
        cli::cli_alert_success("{basename(self$summarize_csv_filepath)} file doesn't exist. Generating...")
      }

      # List .apsimx files
      files_list <- list.files(
        path = self$sims_and_mets_folderpath,
        pattern = ".db$",
        full.names = TRUE
      )

      # If ids_to_summarize is defined, summarize just for these ids
      if (is.numeric(ids_to_summarize)) {
        files_list <- files_list[grepl(
          paste0("simulation", ids_to_summarize, ".db", collapse = "|"),
          files_list
        )]
      }

      # If necessary, summarize just N sims
      if (!is.null(runs_only_some_n)) {
        if (runs_only_some_n > length(files_list)) {
          cli::cli_alert_warning("WARNING: runs_only_some_n parameter [{runs_only_some_n}] must be lower or equal than db files count [{length(files_list)}]. Updating its value to {length(files_list)}")
          runs_only_some_n <- length(files_list)
        }
        if (length(files_list) > runs_only_some_n) {
          files_list <- files_list[1:runs_only_some_n]
        }
      }

      res <- .lapply_parallel_progressbar(
        x_must_be_num_array = seq_along(files_list),
        FUN = function(i) {
          filepath <- files_list[i]
          if (dry_run) {
            cli::cli_alert_success("It will read file {filepath}")
          } else {
            sensi_summarize_harvest_db(
              db_filepath = filepath,
              number_of_fields_to_check = number_of_fields_to_check
            )
          }
        },
        multicores = self$multicores
      )

      if (dry_run) {
        return(NULL)
      }

      # Make big df
      self$summarize_df <- dplyr::bind_rows(res)

      # Save csv
      write.csv(self$summarize_df, self$summarize_csv_filepath, row.names = FALSE)
      cli::cli_alert_success("File 'summarized.csv' saved!")
    },

    compute_salib_for_all_params_and_fields = function(field_column_name, salib_sobol = FALSE, columns_to_exclude = c("id", "field"), fix_NAs_with_mean = FALSE, dry_run = FALSE, overwrite = FALSE) {

      # Check if salib.csv already exists
      salib_csv_filepath <- file.path(self$folder, "salib.csv")
      if (file.exists(salib_csv_filepath)) {
        if (overwrite) {
          cli::cli_alert_warning("'salib.csv' file already exists on {self$folder} folder! However, it will be overwritten because 'overwrite' parameter was set as TRUE!")
        } else {
          cli::cli_alert_danger("'salib.csv' file already exists on {self$folder} folder! Please, if you want to create a new salib csv or overwrite it, set 'overwrite' to TRUE, create a new sensi folder or delete existing file!")
          stop()
        }
      } else {
        cli::cli_alert_success("'salib.csv' file doesn't exist. Generating...")
        overwrite <- FALSE
      }

      # Get number of simulations (samples nrow)
      number_of_simulations <- nrow(self$samples_df)

      # Select some columns to compute salib
      columns_to_include <- colnames(self$summarize_df)[!colnames(self$summarize_df) %in% columns_to_exclude]

      # Get fields
      fields <- unique(self$summarize_df[[field_column_name]])

      # Generate all combinations of fields and columns_to_include
      all_combinations <- expand.grid(fields, columns_to_include)

      # Compute salib for each combination field and columns_to_include
      res <- .lapply_parallel_progressbar(
        x_must_be_num_array = seq_len(nrow(all_combinations)),
        FUN = function(i) {
          field <- all_combinations[i, 1]
          param <- all_combinations[i, 2]
          if (dry_run) {
            cli::cli_alert_success("It will Compute salib with field={field} and param={param}")
          } else {
            salib_for_one_field_and_param(
              df = self$summarize_df,
              field = field,
              param = param,
              number_of_simulations = number_of_simulations,
              problem = self$problem,
              salib_sobol = salib_sobol,
              fix_NAs_with_mean = fix_NAs_with_mean,
              dry_run = dry_run
            )
          }
        },
        multicores = self$multicores
      )

      if (dry_run) {
        return(NULL)
      }

      # rbind all combination df
      rbind_salibs <- dplyr::bind_rows(res) %>%
        dplyr::filter(param != TRUE, field != TRUE)

      # Save csv
      write.csv(rbind_salibs, self$salib_csv_filepath, row.names = FALSE)
      if (overwrite) {
        cli::cli_alert_success("File {basename(self$salib_csv_filepath)} was overwriten!")
      } else {
        cli::cli_alert_success("File {basename(self$salib_csv_filepath)} was created!")
      }
    }
  ),

  private = list(
    check_file_exist_with_cli = function(filepath) {
      if (file.exists(filepath)) {
        cli::cli_alert_success("File {basename(filepath)} is available!")
        return(TRUE)
      } else {
        cli::cli_alert_warning("File {basename(filepath)} does not exist!")
        return(FALSE)
      }
    },

    try_load_problem = function() {
      if (file.exists(self$problem_filepath)) {
        self$problem <- readRDS(self$problem_filepath)
      }
    },

    try_load_samples_csv = function() {
      if (file.exists(self$samples_csv_filepath)) {
        self$samples_df <- read.csv(self$samples_csv_filepath, row.names = NULL)
      }
    },

    try_load_salib_csv = function() {
      if (file.exists(self$salib_csv_filepath)) {
        self$salib_df <- read.csv(self$salib_csv_filepath, row.names = NULL)
      }
    },

    try_load_summarize_csv = function() {
      if (file.exists(self$summarize_csv_filepath)) {
        self$summarize_df <- read.csv(self$summarize_csv_filepath, row.names = NULL)
      }
    },

    import_met_data_to_sims_folder = function(copy_met_data_from) {
      files_met_to_copy <- list.files(copy_met_data_from, pattern = "\\.(xlsx|met)$", full.names = TRUE)

      if (!dir.exists(copy_met_data_from)) {
        cli::cli_alert_danger("{copy_met_data_from} folder does not exist!")
        stop()
      }
      if (length(files_met_to_copy) == 0) {
        cli::cli_alert_danger("{copy_met_data_from} don't have any xlsx or met file!")
        stop()
      }
      file.copy(
        files_met_to_copy,
        self$sims_and_mets_folderpath,
        recursive = TRUE,
        overwrite = TRUE
      )
    },

    generate_base_folder = function() {
      dir.create(self$folder, recursive = TRUE, showWarnings = FALSE)
      cli::cli_alert_success("Folder {self$folder} was created!")
    },

    generate_sims_subfolder = function() {
      dir.create(self$sims_and_mets_folderpath, showWarnings = FALSE)
      cli::cli_alert_success("Subfolder {self$sims_and_mets_folderpath} was created!")
    },

    files_summary = function() {
      cli::cli_alert_info("Summary:")
      cli::cli_alert_info("  Folder = {self$folder}")
      db_filepaths <- list.files(self$sims_and_mets_folderpath, pattern = '.db$')
      cli::cli_alert_info("  Number of dbs: {length(db_filepaths)}")
      apsimx_filepaths <- list.files(self$sims_and_mets_folderpath, pattern = ".apsimx$")
      cli::cli_alert_info("  Number of apsimxs: {length(apsimx_filepaths)}")
    }
  )
)
