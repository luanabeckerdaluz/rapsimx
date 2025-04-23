get_id_from_filepath <- function(filepath) {
  basename <- basename(filepath)
  # id <- as.numeric(str_extract_all(basename, "\\d+"))
  id <- as.numeric(regmatches(basename, gregexpr("\\d+", basename)))
  return(id)
}

list_db_tables <- function(db_filepath) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  print(RSQLite::dbListTables(conn))
  RSQLite::dbDisconnect(conn)
}

read_db_table <- function(
  db_filepath,
  table
  ) {

  # Connects to db
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  # Ensure that db connection will be closed later
  on.exit(RSQLite::dbDisconnect(conn), add = TRUE)

  df <- tryCatch({
    hr <- RSQLite::dbReadTable(conn, table)
    return(hr)
  }, warning = function(w) {
    print(paste0("WARNING: when reading db file ", basename(db_filepath), ": ", w$message))
    return(NULL)
  }, error = function(e) {
    print(paste0("ERROR when reading db file ", basename(db_filepath), ": ", e$message))
    return(NULL)
  })
  return(df)
}

summarize_sim_db <- function(
  db_filepath,
  number_of_fields_to_check = NA
  ) {

  # Handle input errors
  if (!file.exists(db_filepath)) {
    stop("ERROR! db filepath does sim does not exist!")
  }

  if (!is.na(number_of_fields_to_check) && !is.integer(number_of_fields_to_check)) {
    custom_stop("'number_of_fields_to_check' must be an integer (e.g. '32L')")
  }

  # list_db_tables(db_filepath)

  # CheckpointID   SimulationID   Zone   Clock.Today...
  df_db_harvest_report <- read_db_table(db_filepath, "HarvestReport")
  if (is.null(df_db_harvest_report)) {
    return(NULL)
  }
  # print(head(df_db_harvest_report))

  # ID   Name   FolderName
  df_db_simulations <- read_db_table(db_filepath, "_Simulations") %>%
    select(-FolderName) %>%
    rename(SimulationID = ID)
  # print(head(df_db_simulations))

  # print(head(df_db_harvest_report))
  # print(head(df_db_harvest_report))
  # print(head(read_db_table(db_filepath, "_Simulations")))

  summarized_df <- df_db_harvest_report %>%
    merge(df_db_simulations, by = "SimulationID") %>%
    dplyr::rename(
      field = Name,
      yield = Yield,
      biomass = Biomass,
      # emergence = Soybean.Phenology.EmergenceDAS,
      # flowering = Soybean.Phenology.StartFloweringDAS,
      # start_pod_development = Soybean.Phenology.StartPodDevelopmentDAS,
      # start_grain_filling = Soybean.Phenology.StartGrainFillingDAS,
      # end_grain_filling = Soybean.Phenology.EndGrainFillDAS,
      maturity = Soybean.Phenology.MaturityDAS
    ) %>%
    dplyr::select(field, yield, biomass, maturity)
    # dplyr::select(field, yield, biomass, emergence, flowering, start_pod_development,
    #   start_grain_filling, end_grain_filling, maturity)

  rownames(summarized_df) <- NULL

  sim_id <- get_id_from_filepath(db_filepath)
  if (!is.na(sim_id)) {
    summarized_df <- summarized_df %>%
      dplyr::mutate(id = sim_id) %>%
      dplyr::select(id, everything())
  }

  if (!is.na(number_of_fields_to_check) && sim_id && nrow(summarized_df) != number_of_fields_to_check) {
    custom_warning(paste0("Report id ", sim_id, " is missing fields! (", nrow(summarized_df), " / ", number_of_fields_to_check, ")"))
  }

  return(summarized_df)
}

summarize_harvest_dbs <- function(
  sensi_folder,
  ids_to_summarize = NA,
  number_of_fields_to_check = NA,
  runs_only_some_n = NA,
  parallel = TRUE,
  overwrite = FALSE,
  dry_run = FALSE
  ) {

  # Print parallel status
  if (parallel) {
    custom_cat(paste0("Running in parallel with ", CONFIG_MULTICORES, " cores"))
  } else {
    custom_cat("Not using parallel")
  }

  # Check if parameters are integer
  if (!is.na(number_of_fields_to_check) && !is.integer(number_of_fields_to_check)) {
    custom_stop("'number_of_fields_to_check' must be an integer (e.g. '32L')")
  }
  if (!is.na(runs_only_some_n) && !is.integer(runs_only_some_n)) {
    custom_stop("'runs_only_some_n' must be an integer (e.g. '5L')")
  }

  # Skip summarize if "summarized.csv" already exists on folder
  summarized_csv_filepath <- file.path(sensi_folder, "summarized.csv")
  if (file.exists(summarized_csv_filepath)) {
    if (overwrite) {
      custom_warning(paste0("'summarized.csv' file already exists on ", sensi_folder, " folder! Summarize process will run because overwrite parameter was set to TRUE."))
    } else {
      custom_stop(paste0("'summarized.csv' file already exists on ", sensi_folder, " folder! Please, if you want to create new summarized csv or overwrite it, set 'overwrite' to TRUE, create a new sensi folder or delete existing file!"))
    }
  } else {
    custom_cat("'summarized.csv' file doesn't exist. Generating...")
    overwrite <- FALSE
  }

  # Simulation folder
  sims_folder <- file.path(sensi_folder, "simulations")

  # List files
  files_list <- list.files(
    path = sims_folder,
    pattern = ".db",
    full.names = TRUE
  )

  # If ids_to_summarize is defined, summarize just for these ids
  if (is.numeric(ids_to_summarize)) {
    files_list <- files_list[grepl(
      paste0("simulation", ids_to_summarize, ".db", collapse = "|"),
      files_list
    )]
    # print(length(files_list))
    # print(files_list)
  }

  # If necessary, summarize just N sims
  if (!is.na(runs_only_some_n)) {
    if (!is.integer(runs_only_some_n)) {
      custom_stop("'runs_only_some_n' must be an integer number (e.g. '5L')")
    }
    if (runs_only_some_n > length(files_list)) {
      custom_warning(paste0("WARNING: runs_only_some_n parameter [", runs_only_some_n, "] must be lower or equal than db files count [", length(files_list), "]. Updating its value to ", length(files_list)))
      runs_only_some_n <- length(files_list)
    }
    if (length(files_list) > runs_only_some_n) {
      files_list <- files_list[1:runs_only_some_n]
    }
  }

  if (dry_run) {
    for (file in files_list) {
      custom_cat_nobreaks(paste0("It will read file ", file))
    }
    return(NULL)
  }

  # Create progress bar
  pb_summarize <- txtProgressBar(min = 0, max = length(files_list), style = 3)

  apply_parameters <- list(
    X = seq_along(files_list),
    FUN = function(i) {
      df <- summarize_sim_db(
        db_filepath = files_list[i],
        number_of_fields_to_check = number_of_fields_to_check
      )
      setTxtProgressBar(pb_summarize, i)
      return(df)
    }
  )

  # Summarize dbs
  if (parallel) {
    cl <- parallel::makeCluster(CONFIG_MULTICORES)
    future::plan(future::cluster, workers = cl)
    results <- do.call(future.apply::future_lapply, apply_parameters)
    parallel::stopCluster(cl)
  }
  else {
    results <- do.call(lapply, apply_parameters)
  }

  # Close progress bar
  close(pb_summarize)

  # Make big df
  df_all_summarized <- dplyr::bind_rows(results)

  # Save csv
  write.csv(df_all_summarized, summarized_csv_filepath, row.names = FALSE)
  if (overwrite) {
    custom_cat(paste0("File 'summarized.csv' was overwriten!"))
  } else {
    custom_cat(paste0("File 'summarized.csv' was created!"))
  }

  return(df_all_summarized)
}