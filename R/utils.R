get_id_from_filepath <- function(filepath) {
  basename <- basename(filepath)
  # id <- as.numeric(str_extract_all(basename, "\\d+"))
  id <- as.numeric(regmatches(basename, gregexpr("\\d+", basename)))
  return(id)
}

create_tmp_dir <- function(folder_path, copy_met_data = TRUE) {
  # If tmp folder doesn't exist, create and copy met data
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    if (copy_met_data) {
      # Copy met data
      met_filepaths <- list.files(DADOS_MET_FOLDER, full.names = TRUE)
      file.copy(
        met_filepaths,
        folder_path,
        recursive = TRUE,
        overwrite = TRUE
      )
    }
  }
  else {
    warning(paste0(folder_path, " folder already exists! New met files were not copied! \nPlease make sure the folder name is correct in order not to overwrite important runs!"))
  }
}

create_tmp_dir_from_base_folder <- function(path_from_base_folder, copy_met_data = TRUE) {
  folder_filepath <- file.path(CONST_TMP_FOLDER, path_from_base_folder)
  create_tmp_dir(
    folder_path = folder_filepath,
    copy_met_data = copy_met_data
  )
  return(folder_filepath)
}

sim_list_to_df <- function(sim_list, sim_id){
  dfs <- lapply(names(sim_list), function(list_item_name){
    list_item <- sim_list[[list_item_name]] %>%
      mutate(SimulationName = list_item_name)
    return(list_item)
  })
  merged <- bind_rows(dfs)
  summarized <- summarize_simdf2_eval(merged, sim_id)
  return(summarized)
}

print_stats_of_folder <- function(folder_path){
  # Check number of csvs Report in folder
  print(paste("Number of csvs:", length(list.files(folder_path, pattern = "HarvestReport"))))
  # Check number of apsimx in folder
  apsimx_filepaths <- list.files(folder_path, pattern = ".apsimx", full.names = TRUE)
  print(paste("Number of apsimxs:", length(apsimx_filepaths)))
  print("Some files:")
  print(paste0("    ", apsimx_filepaths[1:5]))
}

list_apsimx_filepaths <- function(folder_path) {
  apsimx_filepaths <- list.files(folder_path, pattern = ".apsimx", full.names = TRUE)
  return(apsimx_filepaths)
}



generate_samples_csv <- function(problem, method, N_SAMPLES) {
  # method = "LHS", "FAST", "SOBOL"

  set.seed(1111)

  if (method == "LHS") {
    convert_row_to_var_bounds <- function(row) {
      for (i in 1:length(row)) {
        lb <- variable_bounds[[i]][1]
        ub <- variable_bounds[[i]][2]
        range <- ub - lb
        row[i] <- lb + row[i] * range
      }
      return(row)
    }
    N_VARS <- as.integer(problem$num_vars)
    # df_samples_norm <- import("scipy.stats.qmc")$LatinHypercube(d = N_VARS)$random(n = N_SAMPLES)
    df_samples_norm <- randomLHS(N_SAMPLES, N_VARS)
    # Convert df to var bounds
    for (i in 1:nrow(df_samples_norm)) {
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

  samples_df <- as.data.frame(samples) %>%
    `colnames<-`(problem$names) %>%
    dplyr::mutate(id = as.integer(rownames(.))) %>%
    dplyr::select(id, everything())
  dim(samples_df)
  head(samples_df)

  filename <- "samples.csv"
  write.csv(samples_df, filename, row.names = FALSE)
  print(paste0("File ", filename, " saved!"))

  return(samples_df)
}


plot_samples_distribution <- function(samples_csv_filepath = "samples.csv") {
  samples_df <- read.csv(samples_csv_filepath) %>%
    select(id, everything())
  dim(samples_df)
  head(samples_df)

  options(repr.plot.width = 14, repr.plot.height = 14)
  samples_df %>%
    tidyr::pivot_longer(cols = -id, names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = 1:nrow(.), y = value)) +
      facet_wrap(variable ~ ., scale = "free_y") +
      geom_point(size = 1)
}