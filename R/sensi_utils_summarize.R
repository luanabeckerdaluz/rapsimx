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

read_db_table <- function(db_filepath, table) {
  # table_name = [CheckpointID   SimulationID   Zone   Clock.Today...]

  # Handle input errors
  if (!file.exists(db_filepath)) {
    cli::cli_alert_danger("ERROR! db {db_filepath} does not exist!")
    stop()
  }

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

  # # 
  # # TODO: Filter variables
  # # 
  # vars <- paste(sprintf("%s.[%s], ",tableName, variables), collapse="")
  # vars <- paste(vars,"_Simulations.Name as SimulationName")
  # sql <- paste0('SELECT ', vars, ' FROM ', tableName, ', _Simulations WHERE _Simulations.ID = ', tableName, '.SimulationID')
  # data <- DBI::dbGetQuery(con, sql)
  # DBI::dbDisconnect(con)
  # simulationNames <- unique(data$SimulationName)
  # # Selecting simulations
  # if (!is.null(sim_names)) {
  #   sim_idx <- simulationNames %in% sim_names
  #   simulationNames <- simulationNames[sim_idx]
  # }
  # # Creating empty list
  # sim_nb <- length(simulationNames)
  # tables <- vector("list", sim_nb)
  # # Filling it with results
  # for (i in 1:sim_nb) {
  #   sim <- simulationNames[i]
  #   tables[[i]] <- data[which(data$SimulationName == sim), ] %>% select(one_of(variables))
  #   if ("Clock.Today" %in% names(tables[[i]])) {
  #     tables[[i]] <- mutate(tables[[i]],Date=as.Date(Clock.Today)) %>%
  #       select(-Clock.Today)
  #   } else if ("Date" %in% names(tables[[i]])) {
  #     tables[[i]] <- mutate(tables[[i]],Date=as.Date(Date))
  #   } else if ("Predicted.Clock.Today" %in% names(tables[[i]])) {
  #     tables[[i]] <- mutate(tables[[i]],Date=as.Date(Predicted.Clock.Today)) %>%
  #       select(-Predicted.Clock.Today)
  #   }
  # }
  # names(tables) <- simulationNames

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

  # rapsimx::.list_db_tables(db_filepath)

  # CheckpointID   SimulationID   Zone   Clock.Today...
  df_db_harvest_report <- rapsimx::read_db_table(db_filepath, "HarvestReport")
  if (is.null(df_db_harvest_report)) {
    return(NULL)
  }
  # print(head(df_db_harvest_report))

  # ID   Name   FolderName
  df_db_simulations <- rapsimx::read_db_table(db_filepath, "_Simulations") |>
    dplyr::select(-FolderName) |>
    dplyr::rename(SimulationID = "ID")
  # print(head(df_db_simulations))

  # print(head(df_db_harvest_report))
  # print(head(df_db_harvest_report))
  # print(head(rapsimx::read_db_table(db_filepath, "_Simulations")))

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

  sim_id <- rapsimx::get_id_from_filepath(db_filepath)
  if (!is.null(sim_id)) {
    summarized_df <- summarized_df |>
      dplyr::mutate(id = sim_id) |>
      dplyr::select("id", dplyr::everything())
  }

  if (!is.null(number_of_fields_to_check) && sim_id && nrow(summarized_df) != number_of_fields_to_check) {
    cli::cli_alert_warning("Report id {sim_id} is missing fields! ({nrow(summarized_df)} / {number_of_fields_to_check})")
  }

  return(summarized_df)
}
