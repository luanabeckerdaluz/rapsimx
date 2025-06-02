get_id_from_filepath <- function(filepath) {
  basename <- basename(filepath)
  # id <- as.numeric(str_extract_all(basename, "\\d+"))
  id <- as.numeric(regmatches(basename, gregexpr("\\d+", basename)))
  return(id)
}

.list_db_tables <- function(db_filepath) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  print(RSQLite::dbListTables(conn))
  RSQLite::dbDisconnect(conn)
}

.read_db_table <- function(db_filepath, table) {
  # Connects to db
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  # Ensure that db connection will be closed later
  on.exit(RSQLite::dbDisconnect(conn), add = TRUE)

  df <- tryCatch({
    hr <- RSQLite::dbReadTable(conn, table)
    return(hr)
  }, warning = function(w) {
    cli::cli_alert_warning("WARNING: when reading db file {basename(db_filepath)}: {w$message}")
    return(NULL)
  }, error = function(e) {
    cli::cli_alert_danger("ERROR when reading db file {basename(db_filepath)}: {e$message}")
    return(NULL)
  })

  return(df)
}

sensi_summarize_harvest_db <- function(db_filepath, number_of_fields_to_check = NULL) {
  # Handle input errors
  if (!file.exists(db_filepath)) {
    stop("ERROR! db filepath does sim does not exist!")
  }

  if (!is.null(number_of_fields_to_check) && !is.integer(number_of_fields_to_check)) {
    cli::cli_alert_danger("'number_of_fields_to_check' must be an integer (e.g. '32L')")
    stop()
  }

  # .list_db_tables(db_filepath)

  # CheckpointID   SimulationID   Zone   Clock.Today...
  df_db_harvest_report <- .read_db_table(db_filepath, "HarvestReport")
  if (is.null(df_db_harvest_report)) {
    return(NULL)
  }
  # print(head(df_db_harvest_report))

  # ID   Name   FolderName
  df_db_simulations <- .read_db_table(db_filepath, "_Simulations") |>
    select(-FolderName) |>
    rename(SimulationID = ID)
  # print(head(df_db_simulations))

  # print(head(df_db_harvest_report))
  # print(head(df_db_harvest_report))
  # print(head(.read_db_table(db_filepath, "_Simulations")))

  summarized_df <- df_db_harvest_report |>
    merge(df_db_simulations, by = "SimulationID") # |>
    # dplyr::rename(
    #   field = Name,
    #   yield = Yield,
    #   biomass = Biomass,
    #   # emergence = Soybean.Phenology.EmergenceDAS,
    #   # flowering = Soybean.Phenology.StartFloweringDAS,
    #   # start_pod_development = Soybean.Phenology.StartPodDevelopmentDAS,
    #   # start_grain_filling = Soybean.Phenology.StartGrainFillingDAS,
    #   # end_grain_filling = Soybean.Phenology.EndGrainFillDAS,
    #   maturity = Soybean.Phenology.MaturityDAS
    # ) |>
    # dplyr::select(field, yield, biomass, maturity)
    # # dplyr::select(field, yield, biomass, emergence, flowering, start_pod_development,
    # #   start_grain_filling, end_grain_filling, maturity)

  rownames(summarized_df) <- NULL

  sim_id <- get_id_from_filepath(db_filepath)
  if (!is.null(sim_id)) {
    summarized_df <- summarized_df |>
      dplyr::mutate(id = sim_id) |>
      dplyr::select(id, everything())
  }

  if (!is.null(number_of_fields_to_check) && sim_id && nrow(summarized_df) != number_of_fields_to_check) {
    cli::cli_alert_warning("Report id {sim_id} is missing fields! ({nrow(summarized_df)} / {number_of_fields_to_check})")
  }

  return(summarized_df)
}
