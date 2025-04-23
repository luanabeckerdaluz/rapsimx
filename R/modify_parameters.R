##################
## RAPSIMNG
##################

replace_FixedValue <- function(model, node, value){
  potential <- rapsimng::search_path(model, path = node)
  new_node <- potential$node
  new_node["FixedValue"] <- value
  new <- rapsimng::replace_model(model, potential$path, new_node)
  return(new)
}
replace_X <- function(model, node, index, value){
  potential <- rapsimng::search_path(model, path = node)
  new_node <- potential$node
  new_node[["X"]][index] <- value
  new <- rapsimng::replace_model(model, potential$path, new_node)
  return(new)
}
replace_Y <- function(model, node, index, value){
  potential <- rapsimng::search_path(model, path = node)
  new_node <- potential$node
  new_node[["Y"]][index] <- value
  new <- rapsimng::replace_model(model, potential$path, new_node)
  return(new)
}
replace_KL <- function(model, node, value){
  potential <- rapsimng::search_path(model, path = node)
  new_node <- potential$node
  new_node[["KL"]][1] <- value
  new_node[["KL"]][2] <- value
  new_node[["KL"]][3] <- value
  new <- rapsimng::replace_model(model, potential$path, new_node)
  return(new)
}

# replace_KL_jsonstring2 <- function(apsimx_filepath, kl_array){
#   json_string <- gsub("\\s+", "", paste(readLines(apsimx_filepath), collapse = "\n"))
#   updated_json <- gsub("KL":\\[[0-9.,]+\\]", paste0("KL":[", paste(kl_array, collapse = ","), "]"), json_string)
#   modified_json_pretty <- jsonlite::prettify(updated_json, indent = 2)
#   writeLines(modified_json_pretty, apsimx_filepath)
# }

replace_values <- function(
  apsimx_path,
  VERBOSE = FALSE,
  list_params_values
) {
  # Read apsimx model that will be modified
  new_model <- rapsimng::read_apsimx(apsimx_path)

  # For each names in list_params_values, replace the value in the model
  res <- lapply(
    X = names(list_params_values),
    FUN = function(key) {
      value <- list_params_values[[key]]
      # print(paste("Chave:", key, "Valor:", value))
      switch(
        key,
        "phen_VegAndRepTherTimRes_bothX3" = {
          if (VERBOSE) print("Updating [Phenology].VegetativeThermalTime.Response and [Phenology].ReproductiveThermalTime.Response")
          new_model <<- replace_X(new_model, "[Phenology].VegetativeThermalTime.Response", 3, value)
          new_model <<- replace_X(new_model, "[Phenology].ReproductiveThermalTime.Response", 3, value)
        },
        "phen_VegAndRepPhoMod_bothX1" = {
          if (VERBOSE) print("Updating [Phenology].VegetativePhotoperiodModifier.XYPairs and [Phenology].ReproductivePhotoperiodModifier.XYPairs")
          # 14.43, 21.19
          new_model <<- replace_X(new_model, "[Phenology].VegetativePhotoperiodModifier.XYPairs", 1, value)
          # 14.43, 21.19
          new_model <<- replace_X(new_model, "[Phenology].ReproductivePhotoperiodModifier.XYPairs", 1, value)
        },
        "phen_VegTherTimeResp_X3" = {
          if (VERBOSE) print("Updating [Phenology].VegetativeThermalTime.Response")
          # 10.0, 20.0, 30.0, 40.0
          new_model <<- replace_X(new_model, "[Phenology].VegetativeThermalTime.Response", 3, value)
        },
        "phen_RepTherTimeResp_X3" = {
          if (VERBOSE) print("Updating [Phenology].ReproductiveThermalTime.Response")
          # 10.0, 15.0, 30.0, 40.0
          new_model <<- replace_X(new_model, "[Phenology].ReproductiveThermalTime.Response", 3, value)
        },
        "phen_VegPhoMod_X1" = {
          if (VERBOSE) print("Updating [Phenology].VegetativePhotoperiodModifier.XYPairs")
          # 14.43, 21.19
          new_model <<- replace_X(new_model, "[Phenology].VegetativePhotoperiodModifier.XYPairs", 1, value)
        },
        "phen_RepPhoMod_X1" = {
          if (VERBOSE) print("Updating [Phenology].ReproductivePhotoperiodModifier.XYPairs")
          # 14.43, 21.19
          new_model <<- replace_X(new_model, "[Phenology].ReproductivePhotoperiodModifier.XYPairs", 1, value)
        },
        "phen_VegetativeTarget" = {
          if (VERBOSE) print("Updating [Phenology].Vegetative.Target")
          # 200.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].Vegetative.Target", value)
        },
        "phen_EarlyFloweringTarget" = {
          if (VERBOSE) print("Updating [Phenology].EarlyFlowering.Target")
          # 200.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].EarlyFlowering.Target", value)
        },
        "phen_EarlyPodDevTarget" = {
          if (VERBOSE) print("Updating [Phenology].EarlyPodDevelopment.Target")
          # 140.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].EarlyPodDevelopment.Target", value)
        },
        "phen_FractGrainFill" = {
          if (VERBOSE) print("Updating [Phenology].EarlyGrainFilling.Target.FractionofGrainfilling")
          # 0.05
          new_model <<- replace_FixedValue(new_model, "[Phenology].EarlyGrainFilling.Target.FractionofGrainfilling", value)
        },
        "phen_EntGrainFill" = {
          if (VERBOSE) print("Updating [Phenology].LateGrainFilling.Target.EntireGrainfillPeriod")
          # 500.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].LateGrainFilling.Target.EntireGrainfillPeriod", value)
        },
        "phen_MidGrainFill" = {
          if (VERBOSE) print("Updating [Phenology].MidGrainFilling.Target.FractionofMidToLateGrainfilling")
          # 0.5
          new_model <<- replace_FixedValue(new_model, "[Phenology].MidGrainFilling.Target.FractionofMidToLateGrainfilling", value)
        },
        "phen_Maturing" = {
          if (VERBOSE) print("Updating [Phenology].Maturing.Target")
          # 45.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].Maturing.Target", value)
        },
        "phen_Ripening" = {
          if (VERBOSE) print("Updating [Phenology].Ripening.Target")
          # 45.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].Ripening.Target", value)
        },
        "phen_shootlag" = {
          if (VERBOSE) print("Updating [Phenology].Emerging.Target.ShootLag")
          # 10
          new_model <<- replace_FixedValue(new_model, "[Phenology].Emerging.Target.ShootLag", value)
        },
        "phen_shootrate" = {
          if (VERBOSE) print("Updating [Phenology].Emerging.Target.DepthxRate.ShootRate")
          # 1.0
          new_model <<- replace_FixedValue(new_model, "[Phenology].Emerging.Target.DepthxRate.ShootRate", value)
        },
        "leaf_RUE" = {
          if (VERBOSE) print("Updating [Leaf].Photosynthesis.RUE")
          # 1.2
          new_model <<- replace_FixedValue(new_model, "[Leaf].Photosynthesis.RUE", value)
        },
        "leaf_AreaLargLeaf" = {
          if (VERBOSE) print("Updating [Leaf].AreaLargestLeaf")
          # 0.006
          new_model <<- replace_FixedValue(new_model, "[Leaf].AreaLargestLeaf", value)
        },
        "leaf_Phyllochron" = {
          if (VERBOSE) print("Updating [Leaf].Phyllochron")
          # 45.0
          new_model <<- replace_FixedValue(new_model, "[Leaf].Phyllochron", value)
        },
        "leaf_ExtinctionCoef_Y1" = {
          if (VERBOSE) print("Updating [Leaf].ExtinctionCoefficient.XYPairs")
          # 0.6, 0.4
          new_model <<- replace_Y(new_model, "[Leaf].ExtinctionCoefficient.XYPairs", 1, value)
        },
        "grain_HarvIndex" = {
          if (VERBOSE) print("Updating [Grain].PotentialHarvestIndex")
          # 0.5
          new_model <<- replace_FixedValue(new_model, "[Grain].PotentialHarvestIndex", value)
        },
        "root_EarlyFrontVel" = {
          if (VERBOSE) print("Updating [Root].RootFrontVelocity.PotentialRootFrontVelocity.early.Function")
          # 30.0
          new_model <<- replace_FixedValue(new_model, "[Root].RootFrontVelocity.PotentialRootFrontVelocity.early.Function", value)
        },
        "root_LateFrontVel" = {
          if (VERBOSE) print("Updating [Root].RootFrontVelocity.PotentialRootFrontVelocity.late.Function")
          # 5.0
          new_model <<- replace_FixedValue(new_model, "[Root].RootFrontVelocity.PotentialRootFrontVelocity.late.Function", value)
        },
        "nodule_VegGrowthRate" = {
          if (VERBOSE) print("Updating [Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.VegetativeGrowth.Rate")
          # 0.006
          new_model <<- replace_FixedValue(new_model, "[Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.VegetativeGrowth.Rate", value)
        },
        "nodule_RepGrowthRate" = {
          if (VERBOSE) print("Updating [Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.ReproductiveGrowth.Rate")
          # 0.002
          new_model <<- replace_FixedValue(new_model, "[Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.ReproductiveGrowth.Rate", value)
        },
        "nodule_MaxFixRate" = {
          if (VERBOSE) print("Updating [Nodule].FixationRate.DailyPotentialFixationRate.MaximumFixationRate")
          # 0.6
          new_model <<- replace_FixedValue(new_model, "[Nodule].FixationRate.DailyPotentialFixationRate.MaximumFixationRate", value)
        }
      )
    }
  )

  # Save apsimx
  # jsonlite::write_json(new_model, apsimx_path, pretty = TRUE, auto_unbox = TRUE, null = "null", digits=20)
  # write_apsimx(new_model, apsimx_path)
  to_json <- rjson::toJSON(new_model)
  pretty <- jsonlite::prettify(to_json, indent = 2)
  writeLines(pretty, apsimx_path)
}


replace_soil_and_met <- function(apsimx_filepath, met_and_soil_folder, simulations_names, change_soil, change_met) {
  ret <- lapply(
    X = simulations_names,
    FUN = function(field_name) {
      site <- substr(field_name, start = 1, stop = 2)

      if (change_soil) {
        soil_filepath <- file.path(met_and_soil_folder, paste0(site, ".rds"))
        print(paste("Setting", field_name, "soil data with", soil_filepath))
        soil <- readRDS(file = soil_filepath)
        apsimx::edit_apsimx_replace_soil_profile(
          file = basename(apsimx_filepath),
          src.dir = dirname(apsimx_filepath),
          wrt.dir = dirname(apsimx_filepath),
          root = list(field_name),
          soil.profile = soil,
          overwrite = TRUE
        )
      }

      if (change_met) {
        met_filepath <- file.path(met_and_soil_folder, paste0(site, ".met"))
        print(paste("Setting", field_name, "met data with", met_filepath))
        apsimx::edit_apsimx(
          file = basename(apsimx_filepath),
          src.dir = dirname(apsimx_filepath),
          wrt.dir = dirname(apsimx_filepath),
          node = "Weather",
          value = met_filepath,
          root = list(field_name),
          verbose = FALSE,
          overwrite = TRUE
        )
      }
    }
  )
}


write_config_file <- function(
  config_filepath,
  sim_filename,
  VERBOSE = FALSE,
  phen_VegAndRepTherTimRes_bothX3 = NA,
  phen_VegAndRepPhoMod_bothX1 = NA,
  phen_VegTherTimeResp_X3 = NA,
  phen_RepTherTimeResp_X3 = NA,
  phen_VegPhoMod_X1 = NA,
  phen_RepPhoMod_X1 = NA,
  phen_VegetativeTarget = NA,
  phen_EarlyFloweringTarget = NA,
  phen_EarlyPodDevTarget = NA,
  phen_FractGrainFill = NA,
  phen_EntGrainFill = NA,
  phen_MidGrainFill = NA,
  phen_Maturing = NA,
  phen_Ripening = NA,
  phen_shootlag = NA,
  phen_shootrate = NA,
  leaf_RUE = NA,
  leaf_AreaLargLeaf = NA,
  leaf_Phyllochron = NA,
  leaf_ExtinctionCoef_Y1 = NA,
  grain_HarvIndex = NA,
  root_EarlyFrontVel = NA,
  root_LateFrontVel = NA,
  nodule_VegGrowthRate = NA,
  nodule_RepGrowthRate = NA,
  nodule_MaxFixRate = NA,
  soil_KL = NA,
  soil_size = NA
){

  content <- c (
    paste0("load ", basename(sim_filename)),
    paste0("")
  )
  
  if (!is.na(phen_VegAndRepTherTimRes_bothX3)) {
    if (VERBOSE) print("Updating [Phenology].VegetativeThermalTime.Response and [Phenology].ReproductiveThermalTime.Response")
    content <- c(
      content,
      paste0("[Phenology].VegetativeThermalTime.Response.X = 10.0, 20.0, ", phen_VegAndRepTherTimRes_bothX3, ", 40.0"),
      paste0("[Phenology].ReproductiveThermalTime.Response.X = 10.0, 15.0, ", phen_VegAndRepTherTimRes_bothX3, ", 40.0")
    )
  }
  if (!is.na(phen_VegAndRepPhoMod_bothX1)) {
    if (VERBOSE) print("Updating [Phenology].VegetativePhotoperiodModifier.XYPairs.X and [Phenology].ReproductivePhotoperiodModifier.XYPairs.X")
    content <- c(
      content,
      paste0("[Phenology].VegetativePhotoperiodModifier.XYPairs.X = ", phen_VegAndRepPhoMod_bothX1, ", 21.19"),
      paste0("[Phenology].ReproductivePhotoperiodModifier.XYPairs.X = ", phen_VegAndRepPhoMod_bothX1, ", 21.19")
    )
  }
  if (!is.na(phen_VegTherTimeResp_X3)) {
    if (VERBOSE) print("Updating [Phenology].VegetativeThermalTime.Response.X")
    content <- c(
      content,
      paste0("[Phenology].VegetativeThermalTime.Response.X = 10, 20, ", phen_VegTherTimeResp_X3, ", 40")
    )
  }
  if (!is.na(phen_RepTherTimeResp_X3)) {
    if (VERBOSE) print("Updating [Phenology].ReproductiveThermalTime.Response.X")
    content <- c(
      content,
      paste0("[Phenology].ReproductiveThermalTime.Response.X = 10, 15, ", phen_RepTherTimeResp_X3, ", 40")
    )
  }
  if (!is.na(phen_VegPhoMod_X1)) {
    if (VERBOSE) print("Updating [Phenology].VegetativePhotoperiodModifier.XYPairs.X")
    content <- c(
      content,
      paste0("[Phenology].VegetativePhotoperiodModifier.XYPairs.X = ", phen_VegPhoMod_X1, ", 21.19")
    )
  }
  if (!is.na(phen_RepPhoMod_X1)) {
    if (VERBOSE) print("Updating [Phenology].ReproductivePhotoperiodModifier.XYPairs.X")
    content <- c(
      content,
      paste0("[Phenology].ReproductivePhotoperiodModifier.XYPairs.X = ", phen_RepPhoMod_X1, ", 21.19")
    )
  }
  if (!is.na(phen_VegetativeTarget)) {
    if (VERBOSE) print("Updating [Phenology].Vegetative.Target.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].Vegetative.Target.FixedValue = ", phen_VegetativeTarget)
    )
  }
  if (!is.na(phen_EarlyFloweringTarget)) {
    if (VERBOSE) print("Updating [Phenology].EarlyFlowering.Target.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].EarlyFlowering.Target.FixedValue = ", phen_EarlyFloweringTarget)
    )
  }
  if (!is.na(phen_EarlyPodDevTarget)) {
    if (VERBOSE) print("Updating [Phenology].EarlyPodDevelopment.Target.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].EarlyPodDevelopment.Target.FixedValue = ", phen_EarlyPodDevTarget)
    )
  }
  if (!is.na(phen_FractGrainFill)) {
    if (VERBOSE) print("Updating [Phenology].EarlyGrainFilling.Target.FractionofGrainfilling.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].EarlyGrainFilling.Target.FractionofGrainfilling.FixedValue = ", phen_FractGrainFill)
    )
  }
  if (!is.na(phen_EntGrainFill)) {
    if (VERBOSE) print("Updating [Phenology].LateGrainFilling.Target.EntireGrainfillPeriod.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].LateGrainFilling.Target.EntireGrainfillPeriod.FixedValue = ", phen_EntGrainFill)
    )
  }
  if (!is.na(phen_MidGrainFill)) {
    if (VERBOSE) print("Updating [Phenology].MidGrainFilling.Target.FractionofMidToLateGrainfilling.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].MidGrainFilling.Target.FractionofMidToLateGrainfilling.FixedValue = ", phen_MidGrainFill)
    )
  }
  if (!is.na(phen_Maturing)) {
    if (VERBOSE) print("Updating [Phenology].Maturing.Target.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].Maturing.Target.FixedValue = ", phen_Maturing)
    )
  }
  if (!is.na(phen_Ripening)) {
    if (VERBOSE) print("Updating [Phenology].Ripening.Target.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].Ripening.Target.FixedValue = ", phen_Ripening)
    )
  }
  if (!is.na(phen_shootlag)) {
    if (VERBOSE) print("Updating [Phenology].Emerging.Target.ShootLag.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].Emerging.Target.ShootLag.FixedValue = ", phen_shootlag)
    )
  }
  if (!is.na(phen_shootrate)) {
    if (VERBOSE) print("Updating [Phenology].Emerging.Target.DepthxRate.ShootRate.FixedValue")
    content <- c(
      content,
      paste0("[Phenology].Emerging.Target.DepthxRate.ShootRate.FixedValue = ", phen_shootrate)
    )
  }
  if (!is.na(leaf_RUE)) {
    if (VERBOSE) print("Updating [Leaf].Photosynthesis.RUE.FixedValue")
    content <- c(
      content,
      paste0("[Leaf].Photosynthesis.RUE.FixedValue = ", leaf_RUE)
    )
  }
  if (!is.na(leaf_AreaLargLeaf)) {
    if (VERBOSE) print("Updating [Leaf].AreaLargestLeaf.FixedValue")
    content <- c(
      content,
      paste0("[Leaf].AreaLargestLeaf.FixedValue = ", leaf_AreaLargLeaf)
    )
  }
  if (!is.na(leaf_Phyllochron)) {
    if (VERBOSE) print("Updating [Leaf].Phyllochron.FixedValue")
    content <- c(
      content,
      paste0("[Leaf].Phyllochron.FixedValue = ", leaf_Phyllochron)
    )
  }
  if (!is.na(leaf_ExtinctionCoef_Y1)) {
    if (VERBOSE) print("Updating [Leaf].ExtinctionCoefficient.XYPairs.Y")
    content <- c(
      content,
      paste0("[Leaf].ExtinctionCoefficient.XYPairs.Y = ", leaf_ExtinctionCoef_Y1, ", 0.4")
    )
  }
  if (!is.na(grain_HarvIndex)) {
    if (VERBOSE) print("Updating [Grain].PotentialHarvestIndex.FixedValue")
    content <- c(
      content,
      paste0("[Grain].PotentialHarvestIndex.FixedValue = ", grain_HarvIndex)
    )
  }
  if (!is.na(root_EarlyFrontVel)) {
    if (VERBOSE) print("Updating [Root].RootFrontVelocity.PotentialRootFrontVelocity.early.Function.FixedValue")
    content <- c(
      content,
      paste0("[Root].RootFrontVelocity.PotentialRootFrontVelocity.early.Function.FixedValue = ", root_EarlyFrontVel)
    )
  }
  if (!is.na(root_LateFrontVel)) {
    if (VERBOSE) print("Updating [Root].RootFrontVelocity.PotentialRootFrontVelocity.late.Function.FixedValue")
    content <- c(
      content,
      paste0("[Root].RootFrontVelocity.PotentialRootFrontVelocity.late.Function.FixedValue = ", root_LateFrontVel)
    )
  }
  if (!is.na(nodule_VegGrowthRate)) {
    if (VERBOSE) print("Updating [Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.VegetativeGrowth.Rate.FixedValue")
    content <- c(
      content,
      paste0("[Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.VegetativeGrowth.Rate.FixedValue = ", nodule_VegGrowthRate)
    )
  }
  if (!is.na(nodule_RepGrowthRate)) {
    if (VERBOSE) print("Updating [Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.ReproductiveGrowth.Rate.FixedValue")
    content <- c(
      content,
      paste0("[Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.ReproductiveGrowth.Rate.FixedValue = ", nodule_RepGrowthRate)
    )
  }
  if (!is.na(nodule_MaxFixRate)) {
    if (VERBOSE) print("Updating [Nodule].FixationRate.DailyPotentialFixationRate.MaximumFixationRate.FixedValue")
    content <- c(
      content,
      paste0("[Nodule].FixationRate.DailyPotentialFixationRate.MaximumFixationRate.FixedValue = ", nodule_MaxFixRate)
    )
  }
  if (!is.na(soil_KL)) {
    if (VERBOSE) print("Updating [Soil].Physical.SoybeanSoil.KL")
    if (is.na(soil_size)){
      stop("Error: Please specify soil_size")
    } else if (soil_size == 6){
      s <- paste0("[Soil].Physical.SoybeanSoil.KL = ", soil_KL, ", ", soil_KL, ", ", soil_KL, ", 0.04, 0.04, 0.02") 
      content <- c(content, s)
    } else if (soil_size == 7){
      s <- paste0("[Soil].Physical.SoybeanSoil.KL = ", soil_KL, ", ", soil_KL, ", ", soil_KL, ", 0.04, 0.04, 0.02, 0.02")
      content <- c(content, s)
    } else {
      stop("ERROR SOIL SIZE")
    }
  }

  content <- c(
    content,
    paste0(""),
    paste0("save ", basename(sim_filename)),
    paste0(""),
    paste0("run"),
    paste0("")
  )
  writeLines(content, config_filepath)
}


##################
## APSIMX
##################

edit_parameter <- function(sim_filename, sim_folder, node, value){
  apsimx::edit_apsimx_replacement(
    file = sim_filename,
    src.dir = sim_folder,
    wrt.dir = sim_folder,
    node.string = node,
    root = list("Models.Core.Folder", NA),
    parm = "FixedValue",
    value = value,
    overwrite = TRUE,
    verbose = FALSE
  )
}





############################################
############################################
# ERRO
# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node = "Soybean",
#   node.child = "Phenology",
#   node.subchild = "Vegetative",
#   parm = "Target",
#   verbose = FALSE
# )
# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Soil.Physical.SoybeanSoil",
#   # parm = "FixedValue",
#   verbose = FALSE
# )
############################################
############################################


# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Phenology.EarlyFlowering.Target",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Phenology.EarlyGrainFilling.Target.FractionofGrainfilling",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Phenology.LateGrainFilling.Target.EntireGrainfillPeriod",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Phenology.Maturing.Target",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Leaf.Photosynthesis.RUE",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Leaf.AreaLargestLeaf",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Leaf.Phyllochron",
#   parm = "FixedValue",
#   verbose = FALSE
# )

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Grain.PotentialHarvestIndex",
#   parm = "FixedValue",
#   verbose = FALSE
# )

#???????????????????????????????????????????????????
#???????????????????????????????????????????????????
# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Root.RootFrontVelocity.PotentialRootFrontVelocity.early.Function",
#   parm = "FixedValue",
#   verbose = FALSE
# )
# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Root.RootFrontVelocity.PotentialRootFrontVelocity.late.Function",
#   parm = "FixedValue",
#   verbose = FALSE
# )
# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Nodule.FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.VegetativeGrowth.Rate",
#   parm = "FixedValue",
#   verbose = FALSE
# )
# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Nodule.FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.ReproductiveGrowth.Rate",
#   parm = "FixedValue",
#   verbose = FALSE
# )
#???????????????????????????????????????????????????
#???????????????????????????????????????????????????

# inspect_apsimx_replacement(
#   file = basename(NEW_SIM),
#   src.dir = dirname(NEW_SIM),
#   node.string = "Soybean.Nodule.FixationRate.DailyPotentialFixationRate.MaximumFixationRate",
#   parm = "FixedValue",
#   verbose = FALSE
# )