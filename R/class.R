RApsimxSensitivityClass <- R6::R6Class("RApsimxSensitivity",
  public = list(
    folder = NULL,
    sims_and_mets_folderpath = NULL,
    samples_df = NULL,
    samples_csv_filepath = NULL,
    salib_df = NULL,
    salib_csv_filepath = NULL,
    summarize_df = NULL,
    summarize_csv_filepath = NULL,
    multicores = parallel::detectCores() - 2,
    models_command = NA,

    initialize = function(folder, copy_met_data_from = NA, multicores, models_command) {
      self$samples_csv_filepath <- file.path(self$folder, "samples.csv")
      self$summarize_csv_filepath <- file.path(self$folder, "summarized.csv")
      self$salib_csv_filepath <- file.path(self$folder, "salib.csv")
      self$sims_and_mets_folderpath <- file.path(self$folder, "sims_and_mets")
      self$multicores <- multicores

      # Config multicores
      if (self$multicores >= parallel::detectCores()) {
        cli::cli_alert_danger("ERROR: Multicores parameter ({multicores}) is greater or equal than detected multicores ({parallel::detectCores()})")
        stop()
      }

      # Set apsimx Models command
      if (!is.na(models_command)) {
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

      # Show summary
      cli::cli_alert_success("ApsimX Models folder = {CONFIG_MODELS_COMMAND}")
      cli::cli_alert_success("Multicores = {CONFIG_MULTICORES}")

      # Create folder
      self$folder <- normalizePath(folder, mustWork = FALSE)
      if (!dir.exists(self$folder)) {
        message("[+] Criando nova estrutura de pasta em: ", self$folder)
        self$generate_folder(copy_met_data_from)
      } else {
        message("[+] Carregando pasta existente: ", self$folder)
        self$load_folder()
        self$try_load_samples_csv()
        self$try_load_salib_csv()
        self$try_load_summarize_csv()
      }
    },

    generate_folder = function(copy_met_data_from) {
      dir.create(self$folder, recursive = TRUE, showWarnings = FALSE)
      dir.create(self$sims_and_mets_folderpath, showWarnings = FALSE)
      cli::cli_alert_success("Folder {self$folder} was created!")

      # Copy met data to "sims_and_met" folder
      if (!is.na(copy_met_data_from)) {
        if (!dir.exists(copy_met_data_from)) {
          cli::cli_alert_danger("{copy_met_data_from} folder does not exist!")
          stop()
        } 
        if (length(list.files(copy_met_data_from, full.names = TRUE)) == 0) {
          cli::cli_alert_danger("{copy_met_data_from} does folder is empty! If you want to create the folder anyway, please remove the parameter 'copy_met_data_from'.")
          stop()
        } else {
          file.copy(
            list.files(copy_met_data_from, full.names = TRUE),
            self$sims_and_mets_folderpath,
            recursive = TRUE,
            overwrite = TRUE
          )
        }
      }
    },

    load_folder = function() {
      # Aqui você pode verificar arquivos essenciais, validar estrutura, etc.
      message("[+] Estrutura carregada.")
    },

    try_load_samples_csv = function() {
      samples_csv_filepath <- file.path(self$folder, "samples.csv")
      if (file.exists(samples_csv_filepath)) {
        self$samples_df <- read.csv(samples_csv_filepath, row.names = NULL)
        cli::cli_alert_success("'samples.csv' loaded!")
      } else {
        stop("[✖] samples.csv não encontrado em ", path)
      }
    },

    try_load_salib_csv = function() {
      salib_csv_filepath <- file.path(self$folder, "salib.csv")
      if (file.exists(salib_csv_filepath)) {
        self$salib_df <- read.csv(salib_csv_filepath)
        message("[✔] salib.csv carregado.")
      } else {
        warning("[!] salib.csv não encontrado. Pulei.")
      }
    },

    try_load_summarize = function() {
      summarize_csv_path <- file.path(self$folder, "summarize.csv")
      if (file.exists(summarize_csv_path)) {
        self$summarize_df <- read.csv(summarize_csv_path)
        message("[✔] summarize.csv carregado.")
      } else {
        warning("[!] summarize.csv não encontrado. Pulei.")
      }
    },

    generate_samples = function(n = 100) {
      # Pseudocódigo: gerar samples aleatórios
      message("[+] Samples gerados e salvos.")
    },

    samples.plot = function() {
      if (is.null(self$samples_df)) {
        stop("[✖] Nenhuma amostra carregada. Rode generate_samples() ou verifique samples.csv.")
      }
      pairs(self$samples_df)
    },

    files_summary = function() {
      files <- list.files(self$sims_and_mets_folderpath, pattern = ".apsimx$", full.names = TRUE)
      cat("[i] Total de arquivos .apsimx:", length(files), "\n")
    },

    generate_apsimx = function(sensit_base_sim_filepath, runs_only_some_n = NA, dry_run = FALSE) {
        # Stop if base sim does not exist
        if (!file.exists(sensit_base_sim_filepath)){
            cli::cli_alert_danger("Base simulation {sensit_base_sim_filepath} does not exist!")
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
            multicores = self$multicores
        )

        # Print folder stats
        print_stats_of_folder(sims_folder)
    },

    summarize = function(ids_to_summarize = NA, number_of_fields_to_check = NA, runs_only_some_n = NA, overwrite = FALSE, dry_run = FALSE) {

      # Check if parameters are integer
      if (!is.na(number_of_fields_to_check) && !is.integer(number_of_fields_to_check)) {
        cli::cli_alert_danger("'number_of_fields_to_check' must be an integer (e.g. '32L')")
        stop()
      }
      if (!is.na(runs_only_some_n) && !is.integer(runs_only_some_n)) {
        cli::cli_alert_danger("'runs_only_some_n' must be an integer (e.g. '5L')")
        stop()
      }

      # Skip summarize if "summarized.csv" already exists on folder
      if (file.exists(summarize_csv_filepath)) {
        if (overwrite) {
          cli::cli_alert_warning("'summarized.csv' file already exists on {self$folder} folder! Summarize process will run because overwrite parameter was set to TRUE.")
        } else {
          cli::cli_alert_danger("'summarized.csv' file already exists on {self$folder} folder! Please, if you want to create new summarized csv or overwrite it, set 'overwrite' to TRUE, create a new sensi folder or delete existing file!")
          stop()
        }
      } else {
        cli::cli_alert_success("'summarized.csv' file doesn't exist. Generating...")
        overwrite <- FALSE
      }

      # List .apsimx files
      files_list <- list.files(
        path = self$sims_and_mets_folderpath,
        pattern = ".db",
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
      if (!is.na(runs_only_some_n)) {
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
          if (dry_run) {
            filepath <- files_list[i]
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

      # Make big df
      self$summarize_df <- dplyr::bind_rows(res)

      # Save csv
      write.csv(summarize_df, self$summarize_csv_filepath, row.names = FALSE)
      if (overwrite) {
        cli::cli_alert_success("File 'summarized.csv' was overwriten!")
      } else {
        cli::cli_alert_success("File 'summarized.csv' was created!")
      }
    },

    run = function(runs_only_some_n = NA, simulations_names = NA, ids_to_run = NA, dry_run = FALSE) {
      run_apsimxs(
        sims_folder = self$sims_and_mets_folderpath,
        runs_only_some_n = runs_only_some_n,
        simulations_names = simulations_names,
        ids_to_run = ids_to_run,
        multicores = self$multicores,
        dry_run = dry_run
      )
    }
  )
)

RApsimxSensitivity <- function(folder) {
  RApsimxSensitivityClass$new(folder)
}
