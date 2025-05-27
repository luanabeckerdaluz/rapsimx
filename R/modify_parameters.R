#################################
# Replace values using RAPSIMNG
#################################

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
        },
        {
          cli::cli_alert_danger("{key} not available on replace_values function!")
          stop()
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