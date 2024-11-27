
just_run_apsimx <- function(
  apsimx_filepath,
  force = TRUE,
  summarize = TRUE,
  cleanup = TRUE,
  read_output = FALSE,
  simulations_names = NA,
  report_table = "DailyReport",
  from_config_file = NA,
  xlsx_or_met_folder = NA
) {

  # Check inputs
  report_options <- c("DailyReport", "HarvestReport")
  if (!report_table %in% report_options) {
    stop(paste("Incorrect report table! Options are", paste(report_options, collapse = ", ")))
  }
  if (!file.exists(apsimx_filepath)){
    stop(paste("ERROR! apsimx simulation", apsimx_filepath, "does not exist!"))
  }

  # Copy all .xlsx or .met files to the same folder of simulation
  if (!is.na(xlsx_or_met_folder)) {
    files_to_copy <- list.files(xlsx_or_met_folder, pattern = "\\.(xlsx|met)$", full.names = TRUE)
    file.copy(files_to_copy, dirname(apsimx_filepath), overwrite = TRUE)
  }

  # If this csv already exists, skip
  csv_filepath <- gsub("\\.apsimx$", paste0(".", report_table, ".csv"), apsimx_filepath)
  if (file.exists(csv_filepath) && !force) {
    print(paste0("file ", csv_filepath, " exists! Returning..."))
  }
  else {
    command <- NA
    if(!is.na(from_config_file)) {
      command <- paste0(CONFIG_MODELS_COMMAND, " --apply ", from_config_file)
    }
    else {
      command <- paste(CONFIG_MODELS_COMMAND, apsimx_filepath)
    }
    command <- paste0(
      command,
      " --csv",
      " --single-threaded=FALSE",
      " --cpu-count=", CONFIG_MULTICORES
    )
    if (is.character(simulations_names)) {
      command <- paste0(command, " --simulation-names='", paste0(simulations_names, collapse = "|"), "'")
    }
    # command <- paste0(
    #     "docker run -i --rm --cpus='1.0' -v ",
    #     ":/ApsimX apsiminitiative/apsimng /ApsimX/",
    #     filename_sim,
    #     " --csv"
    # )

    systemtime <- tryCatch(
      {
        systemtime <- system.time(system(command, intern = TRUE))
        systemtime
      },
      warning = function(w) {
        print(paste("WARNING when running", command))
        return(NULL)
      },
      error = function(e) {
        print(paste("ERROR when running", command))
        return(NULL)
      }
    )

    if (is.null(systemtime)) {
      return(NULL)
    }
  }

  # Remove unused files
  if (cleanup) {
    pattern <- "temp|db|bak"
    if (!is.na(xlsx_or_met_folder))   pattern <- "temp|db|bak|xlsx"
    to_be_deleted <- list.files(dirname(apsimx_filepath), pattern = pattern, full.names = TRUE)
    suppressWarnings(file.remove(to_be_deleted))
  }

  if (read_output) {
    csv <- NA
    if (summarize) {
      # warning("Please, verify if values are consistent to use the correct correction method depending if Windows (new_report_run2 or different_version methods) or Linux (use normal method)")
      csv <- summarize_simcsv(
        csv_filepath = csv_filepath,
        daily_or_harvest = report_table,
        check_32_fields_use_id = FALSE,
        return_with_NA_for_daily = TRUE
      )
    } else {
      csv <- open_daily_simcsv(csv_filepath)
    }
    return(csv)
  } else {
    return(NA)
  }
}

generate_apsimx <- function(df_row, folder, sensit_base_sim_filepath) {
  # Check if base sim exists
  if (!file.exists(sensit_base_sim_filepath)){
    stop("ERROR! Base sim does not exist!")
  }

  # Get combination id
  id <- as.numeric(df_row["id"])

  # Copy base simulation to tmp folder
  filepath_sim <- file.path(folder, paste0("simulation", id, ".apsimx"))

  file.copy(
    sensit_base_sim_filepath,
    filepath_sim,
    overwrite = TRUE
  )

  # Replace parameters
  replace_values2(
    apsimx_path = filepath_sim,
    VERBOSE = FALSE,
    phen_VegAndRepTherTimRes_bothX3 = as.numeric(df_row["phen_VegAndRepTherTimRes_bothX3"]),
    phen_VegAndRepPhoMod_bothX1     = as.numeric(df_row["phen_VegAndRepPhoMod_bothX1"]),
    phen_VegTherTimeResp_X3         = as.numeric(df_row["phen_VegTherTimeResp_X3"]),
    phen_RepTherTimeResp_X3         = as.numeric(df_row["phen_RepTherTimeResp_X3"]),
    phen_VegPhoMod_X1               = as.numeric(df_row["phen_VegPhoMod_X1"]),
    phen_RepPhoMod_X1               = as.numeric(df_row["phen_RepPhoMod_X1"]),
    phen_VegetativeTarget           = as.numeric(df_row["phen_VegetativeTarget"]),
    phen_EarlyFloweringTarget       = as.numeric(df_row["phen_EarlyFloweringTarget"]),
    phen_EarlyPodDevTarget          = as.numeric(df_row["phen_EarlyPodDevTarget"]),
    phen_FractGrainFill             = as.numeric(df_row["phen_FractGrainFill"]),
    phen_EntGrainFill               = as.numeric(df_row["phen_EntGrainFill"]),
    phen_MidGrainFill               = as.numeric(df_row["phen_MidGrainFill"]),
    phen_Maturing                   = as.numeric(df_row["phen_Maturing"]),
    phen_Ripening                   = as.numeric(df_row["phen_Ripening"]),
    phen_shootlag                   = as.numeric(df_row["phen_shootlag"]),
    phen_shootrate                  = as.numeric(df_row["phen_shootrate"]),
    leaf_RUE                        = as.numeric(df_row["leaf_RUE"]),
    leaf_AreaLargLeaf               = as.numeric(df_row["leaf_AreaLargLeaf"]),
    leaf_Phyllochron                = as.numeric(df_row["leaf_Phyllochron"]),
    leaf_ExtinctionCoef_Y1          = as.numeric(df_row["leaf_ExtinctionCoef_Y1"]),
    grain_HarvIndex                 = as.numeric(df_row["grain_HarvIndex"]),
    nodule_VegGrowthRate            = as.numeric(df_row["nodule_VegGrowthRate"]),
    nodule_RepGrowthRate            = as.numeric(df_row["nodule_RepGrowthRate"]),
    nodule_MaxFixRate               = as.numeric(df_row["nodule_MaxFixRate"]),
    root_LateFrontVel               = as.numeric(df_row["root_LateFrontVel"]),
    soil_KL                         = as.numeric(df_row["soil_KL"]),
    root_EarlyFrontVel              = as.numeric(df_row["root_EarlyFrontVel"])
  )

  return(filepath_sim)
}

generate_apsimx_and_run <- function(df_row, folder, force, sensit_base_sim_filepath) {
  apsimx_filepath <- generate_apsimx(
    df_row,
    folder,
    sensit_base_sim_filepath
  )
  just_run_apsimx(
    apsimx_filepath = apsimx_filepath,
    force = force, 
    xlsx_or_met_folder = DADOS_MET_FOLDER
  )
}

generate_apsimx_from_df <- function(samples_df, folder, sensit_base_sim_filepath, N = 5, runs_only_some, parallel){
  # Stop if base sim does not exist
  if (!file.exists(sensit_base_sim_filepath)){
    stop("ERROR! Base sim does not exist!")
  }
  # Stop if tmp folder does not exist
  if (!file.exists(folder)){
    stop("ERROR! tmp folder does not exist!")
  }
  # If necessary, filter df to run just N sims
  if (runs_only_some && nrow(samples_df)>N)   samples_df <- samples_df[1:N,]

  if (parallel) {
    cl <- parallel::makeCluster(CONFIG_MULTICORES)
    future::plan(future::cluster, workers = cl)
    results <- future.apply::future_apply(
      X = samples_df,
      MARGIN = 1,
      FUN = generate_apsimx,
      folder = folder,
      sensit_base_sim_filepath = sensit_base_sim_filepath
    )
    parallel::stopCluster(cl)
  }
  else {
    results <- apply(
      X = samples_df,
      MARGIN = 1,
      FUN = generate_apsimx,
      folder = folder,
      sensit_base_sim_filepath = sensit_base_sim_filepath
    )
  }
}

run_apsimx_from_folder <- function(folder, runs_only_some, cleanup = TRUE, N = 5, simulations_names = NA, force_rerun = TRUE, ids_to_run = NA, parallel) {
  # List files
  apsimx_filepaths <- list.files(
    path = folder,
    pattern = ".apsimx",
    full.names = TRUE
  )

  # If necessary, filter df to run just N sims
  if (runs_only_some && length(apsimx_filepaths) > N)   apsimx_filepaths <- apsimx_filepaths[1:N]

  # If ids_to_run is defined, summarize just for these ids
  if (is.numeric(ids_to_run)) {
    files_list <- files_list[grepl(
      paste0("simulation", ids_to_run, ".apsimx"),
      files_list
    )]
    print(length(files_list))
    print(files_list)
  }

  if (parallel) {
    cl <- parallel::makeCluster(CONFIG_MULTICORES)
    future::plan(future::cluster, workers = cl)
    results <- future.apply::future_lapply(
      X = apsimx_filepaths,
      FUN = just_run_apsimx,
      force = force_rerun,
      cleanup = cleanup,
      simulations_names = simulations_names
    )
    parallel::stopCluster(cl)
  } else {
    results <- lapply(
      X = apsimx_filepaths,
      FUN = just_run_apsimx,
      force = force_rerun,
      cleanup = cleanup,
      simulations_names = simulations_names
    )
  }
}