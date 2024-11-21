get_id_from_filepath <- function(filepath) {
  basename <- basename(filepath)
  # id <- as.numeric(str_extract_all(basename, "\\d+"))
  id <- as.numeric(regmatches(basename, gregexpr("\\d+", basename)))
  return(id)
}

create_tmp_dir <- function(folder_path, copy_met_data = TRUE) {
  # If tmp folder doesn't exist, create and copy met data
  if (!file.exists(folder_path)) {
    dir.create(folder_path)
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