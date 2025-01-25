correct_csv_different_apsim_version <- function(filepath) {
  ncol_csv <- ncol(read.csv(filepath, header = FALSE, skip = 1, nrows = 1))
  csv <- read.csv(filepath, header = FALSE, skip = 1, colClasses = rep("character", ncol_csv)) %>%
    rename(
      SimulationName = V1,
      SimulationID = V2,
      CheckpointID = V5,
      CheckpointName = V6,
      Clock.Today = V7,
      DAS = V8,
      Phase = V9,
      Zone = V20
    ) %>%
    tidyr::unite("Biomass", c("V3","V4"), sep = ".", remove = T) %>%
    tidyr::unite("Soybean.AboveGround.N", c("V10","V11"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.LAI", c("V12","V13"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.Phenology.Stage", c("V14","V15"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.Total.Wt", c("V16","V17"), sep = ".", remove = T) %>%
    tidyr::unite("Yield", c("V18","V19"), sep = ".", remove = T) %>%
    mutate(
      SimulationID = as.numeric(SimulationID),
      CheckpointID = as.numeric(CheckpointID),
      Soybean.AboveGround.N = as.numeric(Soybean.AboveGround.N),
      DAS = as.numeric(DAS),
      # Clock.Today = as.Date(Clock.Today),
      Biomass = as.numeric(Biomass),
      Soybean.LAI = as.numeric(Soybean.LAI),
      Soybean.Phenology.Stage = as.numeric(Soybean.Phenology.Stage),
      Soybean.Total.Wt = as.numeric(Soybean.Total.Wt),
      Yield = as.numeric(Yield),
    )

  return(csv)
}

correct_csv_new_report <- function(filepath) {
  csv <- read.csv(filepath, header = FALSE, skip = 1) %>%
    rename(
      SimulationName = V1,
      SimulationID = V2,
      CheckpointID = V5,
      CheckpointName = V6,
      Clock.Today = V7,
      DAS = V8,
      Phase = V9,
      Zone = V48
    ) %>%
    tidyr::unite("Biomass", c("V3","V4"), sep = ".", remove = T) %>%
    tidyr::unite("Soybean.AboveGround.N", c("V38","V39"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.LAI", c("V40","V41"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.Phenology.Stage", c("V42","V43"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.Total.Wt", c("V44","V45"), sep = ".", remove = T) %>%
    tidyr::unite("Yield", c("V46","V47"), sep = ".", remove = T) %>%
    mutate(
      # Clock.Today = as.Date(Clock.Today),
      Biomass = as.numeric(Biomass),
      Soybean.LAI = as.numeric(Soybean.LAI),
      Soybean.Phenology.Stage = as.numeric(Soybean.Phenology.Stage),
      Soybean.Total.Wt = as.numeric(Soybean.Total.Wt),
      Yield = as.numeric(Yield),
    )

  return(csv)
}

correct_csv_new_report_run2 <- function(filepath) {
  csv <- read.csv(filepath, header = FALSE, skip = 1) %>%
    dplyr::rename(
      SimulationName = V1,
      SimulationID = V2,
      CheckpointID = V5,
      CheckpointName = V6,
      Clock.Today = V7,
      DAS = V8,
      Phase = V9,
      Zone = V16
    ) %>%
    tidyr::unite("Biomass", c("V3","V4"), sep = ".", remove = T) %>%
    tidyr::unite("Soybean.LAI", c("V10","V11"), sep = ".", remove = T) %>% 
    tidyr::unite("Soybean.Phenology.Stage", c("V12","V13"), sep = ".", remove = T) %>% 
    tidyr::unite("Yield", c("V14","V15"), sep = ".", remove = T) %>%
    dplyr::mutate(
      # Clock.Today = as.Date(Clock.Today),
      Biomass = as.numeric(Biomass),
      Soybean.LAI = as.numeric(Soybean.LAI),
      Soybean.Phenology.Stage = as.numeric(Soybean.Phenology.Stage),
      Yield = as.numeric(Yield),
    )

  return(csv)
}

correct_csv_normal <- function(filepath) {
  csv <- read.csv(filepath, header = TRUE)
  return(csv)
}


# library("RSQLite")

# read_apsimx_db <- function(db_filename) {
#   con <- DBI::dbConnect(RSQLite::SQLite(), db_filename)
#   tableName <- "DailyReport"
#   sql <- paste0('SELECT * FROM ', tableName)
#   data <- DBI::dbGetQuery(con, sql)
#   DBI::dbDisconnect(con)
#   return(data)
# }

open_daily_simcsv <- function(csv_filepath, read_db = FALSE) {
  if (read_db) {
    # db_filepath <- gsub(".DailyReport.csv", ".db", csv_filepath)
    # a <- read_apsimx_db(db_filepath)
    # print(head(a))
    # return(a)
    return(NA)
  }
  else {
    # normal: When using linux, use this
    # new_report: DEPRECATED
    # new_report_run2: Best for Windows
    # different_version: Old report (e.g. Read oldapply sims)
    csv_trycatch <- tryCatch({
      csv_trycatch <- correct_csv_new_report_run2(csv_filepath)
    }, warning = function(w) {
      csv_trycatch <- correct_csv_normal(csv_filepath)
    }, error = function(e) {
      csv_trycatch <- correct_csv_normal(csv_filepath)
    })
    return(csv_trycatch)
  }
}

summarize_simdf <- function(df, sim_id = NA, check_32_fields_use_id, return_with_NA_for_daily) {
  summarized_df <- df %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::reframe(
      yield = first(Yield[which(Phase == "HarvestRipe")]),
      # yield = max(Yield),
      biomass = first(Biomass[which(Phase == "HarvestRipe")]),
      # biomass = max(Biomass),
      emergence = first(DAS[which(Phase == "Emergence")]),
      flowering = first(DAS[which(Phase == "StartFlowering")]),
      pod_development = first(DAS[which(Phase == "StartPodDevelopment")]),
      start_grain_filling = first(DAS[which(Phase == "StartGrainFilling")]),
      end_grain_filling = first(DAS[which(Phase == "EndGrainFill")]),
      maturity = first(DAS[which(Phase == "Maturity")]),
      harvest = first(DAS[which(Phase == "HarvestRipe")])
    ) %>%
    dplyr::rename(field = SimulationName)

  if (!is.na(sim_id)){
    summarized_df <- summarized_df %>%
      dplyr::mutate(id = sim_id) %>%
      dplyr::select(id, everything())
  }

  rows_with_NA <- summarized_df[rowSums(is.na(summarized_df)) > 0, ]
  if (check_32_fields_use_id && sim_id && nrow(rows_with_NA) > 0) {
    print(paste("ERROR id=", sim_id, " missing fields", paste0(rows_with_NA$field, collapse = ", ")))
  }

  if (!return_with_NA_for_daily) {
    # Return just rows without NA
    rows_not_with_NA <- summarized_df[rowSums(is.na(summarized_df)) == 0, ]
    summarized_df <- rows_not_with_NA
  }

  return(summarized_df)
}

organize_harvest_csv <- function(csv_filepath, sim_id = NA, check_32_fields_use_id) {
  summarized_df <- read.csv(csv_filepath) %>%
    dplyr::rename(
      field = SimulationName,
      yield = Yield,
      biomass = Biomass,
      emergence = Soybean.Phenology.EmergenceDAS,
      flowering = Soybean.Phenology.StartFloweringDAS,
      start_pod_development = Soybean.Phenology.StartPodDevelopmentDAS,
      start_grain_filling = Soybean.Phenology.StartGrainFillingDAS,
      end_grain_filling = Soybean.Phenology.EndGrainFillDAS,
      maturity = Soybean.Phenology.MaturityDAS
    ) %>%
    dplyr::select(field, yield, biomass, emergence, flowering, start_pod_development,
      start_grain_filling, end_grain_filling, maturity)

  rownames(summarized_df) <- NULL
  if (!is.na(sim_id)) {
    summarized_df <- summarized_df %>%
      dplyr::mutate(id = sim_id) %>%
      dplyr::select(id, everything())
  }

  if (check_32_fields_use_id && sim_id && nrow(summarized_df) != 32) {
    print(paste0("Report id ", sim_id, " is missing fields! (", nrow(summarized_df), " / ", 32, ")"))
  }
}

summarize_simcsv <- function(csv_filepath, daily_or_harvest = "HarvestReport", return_with_NA_for_daily = TRUE, check_32_fields_use_id = FALSE) {
  # daily_or_harvest = 'DailyReport' or 'HarvestReport'
  # check_32_fields_use_id = FALSE or TRUE
  
  # Handle input errors
  if (!file.exists(csv_filepath)) {
    stop("ERROR! csv filepath does sim does not exist!")
  }
  report_options <- c("DailyReport", "HarvestReport")
  if (!daily_or_harvest %in% report_options) {
    stop(paste("Incorrect report table! Options are", paste(report_options, collapse = ", ")))
  }

  sim_id <- get_id_from_filepath(csv_filepath)

  # If 'DailyReport', summarize df. If 'HarvestReport', get HarvestReport.
  summarized_df <- NA
  if (daily_or_harvest == 'DailyReport') {
    sim_csv <- open_daily_simcsv(csv_filepath)
    summarized_df <- summarize_simdf(sim_csv, sim_id, check_32_fields_use_id, return_with_NA_for_daily)
  }
  else if (daily_or_harvest == 'HarvestReport') {
    summarized_df <- organize_harvest_csv(csv_filepath, sim_id, check_32_fields_use_id)
  }
  else{
    stop("daily_or_harvest parameter is invalid!")
  }

  return(summarized_df)
}

#' @title summarize_csvs_from_filelist
#' @name summarize_csvs_from_filelist
#' @description Function to summarize all CSVs based on a list of CSV filepaths
#' @param folder (string) Name of the folder containing all csv files
#' @param ids_to_summarize (array-numeric) Ids of the CSV files to summarize
#' @param check_32_fields_use_id (bool) If true, checks if summarized df has 32 unique fields.
#' @param daily_or_harvest (string) If 'DailyReport', summarizes daily CSVs. If 'HarvestReport', just read CSV.
#' @return a data frame with the columns id, field, yield, biomass, emergence, flowering, pod_development, start_grain_filling, end_grain_filling, maturity and harvest, depending of the \sQuote{files_list} argument.
#' @export
summarize_csvs_from_filelist <- function(
  folder,
  ids_to_summarize = NA,
  return_with_NA_for_daily = TRUE,
  check_32_fields_use_id = FALSE,
  daily_or_harvest = "DailyReport",
  runs_only_some) {

  # Check for inputs errors
  report_options <- c("DailyReport", "HarvestReport")
  if (!daily_or_harvest %in% report_options) {
    stop(paste("Incorrect report table! Options are", paste(report_options, collapse = ", ")))
  }

  # List files
  files_list <- list.files(
    path = folder,
    pattern = paste0(daily_or_harvest, ".csv"),
    full.names = TRUE
  )

  # If ids_to_summarize is defined, summarize just for these ids
  if (is.numeric(ids_to_summarize)) {
    files_list <- files_list[grepl(
      paste0("simulation", ids_to_summarize, ".", daily_or_harvest, collapse = "|"),
      files_list
    )]
    print(length(files_list))
    print(files_list)
  }

  # If necessary, summarize just N sims
  N <- 15
  if (runs_only_some && length(files_list) > N)   files_list <- files_list[1:N]

  # Summarize csvs
  cl <- parallel::makeCluster(CONFIG_MULTICORES)
  future::plan(future::cluster, workers = cl)
  res <- future.apply::future_lapply(
    X = files_list,
    FUN = summarize_simcsv,
    daily_or_harvest = daily_or_harvest,
    return_with_NA_for_daily = return_with_NA_for_daily,
    check_32_fields_use_id = check_32_fields_use_id
  )
  parallel::stopCluster(cl)

  # Make big df
  df_all_summarized <- dplyr::bind_rows(res)

  return(df_all_summarized)
}




list_db_tables <- function(db_filepath) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  print(RSQLite::dbListTables(conn))
  RSQLite::dbDisconnect(conn)
}

read_db_table <- function(db_filepath, table) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  hr <- RSQLite::dbReadTable(conn, table)
  RSQLite::dbDisconnect(conn)
  return(hr)
}

summarize_sim_db <- function(db_filepath, number_of_fields = NA) {
  # Handle input errors
  if (!file.exists(db_filepath)) {
    stop("ERROR! db filepath does sim does not exist!")
  }

  # list_db_tables(db_filepath)
  
  # CheckpointID   SimulationID   Zone   Clock.Today...
  df_db_harvest_report <- read_db_table(db_filepath, "HarvestReport")
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

  if (!is.na(number_of_fields) && sim_id && nrow(summarized_df) != number_of_fields) {
    print(paste0("Report id ", sim_id, " is missing fields! (", nrow(summarized_df), " / ", number_of_fields, ")"))
  }

  return(summarized_df)
}

summarize_harvest_dbs_from_filelist <- function(
  folder,
  ids_to_summarize = NA,
  number_of_fields = NA,
  runs_only_some = FALSE,
  N = 5) {

  # List files
  files_list <- list.files(
    path = folder,
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
  if (runs_only_some && length(files_list) > N)   files_list <- files_list[1:N]

  # Summarize dbs
  cl <- parallel::makeCluster(CONFIG_MULTICORES)
  future::plan(future::cluster, workers = cl)
  res <- future.apply::future_lapply(
    X = files_list,
    FUN = summarize_sim_db,
    number_of_fields = number_of_fields
  )
  parallel::stopCluster(cl)

  # Make big df
  df_all_summarized <- dplyr::bind_rows(res)

  return(df_all_summarized)
}