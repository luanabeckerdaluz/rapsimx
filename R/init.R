# -------------------------------------------------------

CONFIG_MULTICORES <- parallel::detectCores() - 2

# -------------------------------------------------------

init <- function(where){
    if (where == "local"){
        base_folder <<- "/home/luanabeckerdaluz/git/luanabeckerdaluz/apsimx-sensitivity"
        CONFIG_MODELS_COMMAND <<- "/usr/local/bin/Models"
        print("Config for local!")
    } else if (where == "container"){
        base_folder <<- "/home/rstudio/apsimx-sensitivity"
        CONFIG_MODELS_COMMAND <<- "/usr/local/bin/Models"
        print("Config for container!")
    } else if (where == "windows"){
        CONFIG_MODELS_COMMAND <<- "C:\\APSIM2024.5.7504.0\\bin\\Models.exe"
        # CONFIG_MODELS_COMMAND <<- "C:\\APSIM2024.2.7381.0\\bin\\Models.exe"
        base_folder <<- "C:\\apsimtestes\\"
        print(paste("Using apsim:", CONFIG_MODELS_COMMAND))
        print("Config for windows!")
    } else {
        print("Invalid where parameter! Valid options = [local, container, windows]")
    }

    # Paths
    MET_FILES_PATH          <<- file.path(base_folder, "NEW_7949_met")
    SOILS_FILES_PATH        <<- file.path(base_folder, "NEW_7949_soils")
    DADOS_MET_FOLDER        <<- file.path(base_folder, "dados-met")
    SRC_FOLDER              <<- file.path(base_folder, "R")
    TABLES_FOLDER           <<- file.path(base_folder, "field_data")
    BASE_SIMULATIONS_FOLDER <<- file.path(base_folder, "base_simulations")
    CONST_TMP_FOLDER        <<- file.path(base_folder, "tmp")
    FIELDS_METSOILS_FOLDER  <<- file.path(base_folder, "site_soils_and_met")
    BIG_FILES_FOLDER        <<- file.path(base_folder, "big_files")

    # Source modules
    source(file.path(SRC_FOLDER, "compute_salib.R"))
    source(file.path(SRC_FOLDER, "modify_parameters.R"))
    source(file.path(SRC_FOLDER, "utils_run_apsimx.R"))
    source(file.path(SRC_FOLDER, "utils_summarize.R"))
    source(file.path(SRC_FOLDER, "utils.R"))
}
