init <- function(
  multicores = parallel::detectCores() - 2,
  models_command = NA
  ) {

  # Source modules
  source(file.path("../R", "modify_parameters.R"))
  source(file.path("../R", "utils_messages.R"))
  source(file.path("../R", "utils_run_apsimx.R"))
  source(file.path("../R", "utils_sensi_salib.R"))
  source(file.path("../R", "utils_sensi_generate.R"))
  source(file.path("../R", "utils_sensi_summarize.R"))
  source(file.path("../R", "utils_sensi.R"))
  source(file.path("../R", "utils.R"))

  # Config multicores
  if (multicores >= parallel::detectCores()) {
    custom_stop(paste("ERROR: Multicores parameter (", multicores, ") is greater or equal than detected multicores (", parallel::detectCores(), ")"))
  }
  CONFIG_MULTICORES <<- multicores

  # Set apsimx Models command
  if (!is.na(models_command)) {
    CONFIG_MODELS_COMMAND <<- models_command
  } else {
    # Set apsimx Models command path based on system info
    if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "Darwin") {
      CONFIG_MODELS_COMMAND <<- "/usr/local/bin/Models"
    } else if (Sys.info()["sysname"] == "Windows") {
      CONFIG_MODELS_COMMAND <<- "C:\\APSIM2024.5.7504.0\\bin\\Models.exe"
    }
  }

  # Show summary
  # custom_cat_nobreaks(paste0("Base folder = ", CONST_BASE_FOLDER))
  # custom_cat_nobreaks(paste0("Met folder = ", CONST_MET_FOLDER))
  # custom_cat_nobreaks(paste0("Base simulations folder = ", CONST_BASE_SIMULATIONS_FOLDER))
  custom_cat_nobreaks(paste0("ApsimX Models folder = ", CONFIG_MODELS_COMMAND))
  custom_cat_nobreaks(paste0("Multicores = ", CONFIG_MULTICORES))
}