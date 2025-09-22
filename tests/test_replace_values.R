library(testthat)
library(rapsimng)

original_path <- "test_simulation.apsimx"
test_path <- file.path(tempdir(), "test.apsimx")

file.copy(original_path, test_path, overwrite = TRUE) || stop("Falha ao copiar o arquivo APSIMX.")

on.exit({ if (file.exists(test_path)) file.remove(test_path) })

test_that("invalid parameter", {
  expect_error(
    replace_values(
      apsimx_path = test_path,
      list_params_values = list(
        "this_doesnt_exist" = 999
      )
    )
  )
})

test_that("phen_VegAndRepTherTimRes_bothX3 was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_VegAndRepTherTimRes_bothX3" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].VegetativeThermalTime.Response")$node$X[[3]]
  expect_equal(read_value, new_value)
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].ReproductiveThermalTime.Response")$node$X[[3]]
  expect_equal(read_value, new_value)
})

test_that("phen_VegAndRepPhoMod_bothX1 was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_VegAndRepPhoMod_bothX1" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].VegetativePhotoperiodModifier.XYPairs")$node$X[[1]]
  expect_equal(read_value, new_value)
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].ReproductivePhotoperiodModifier.XYPairs")$node$X[[1]]
})

test_that("phen_VegTherTimeResp_X3 was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_VegTherTimeResp_X3" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].VegetativeThermalTime.Response")$node$X[[3]]
  expect_equal(read_value, new_value)
})

test_that("phen_RepTherTimeResp_X3 was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_RepTherTimeResp_X3" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].ReproductiveThermalTime.Response")$node$X[[3]]
  expect_equal(read_value, new_value)
})

test_that("phen_VegPhoMod_X1 was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_VegPhoMod_X1" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].VegetativePhotoperiodModifier.XYPairs")$node$X[[1]]
  expect_equal(read_value, new_value)
})

test_that("phen_RepPhoMod_X1 was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_RepPhoMod_X1" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].ReproductivePhotoperiodModifier.XYPairs")$node$X[[1]]
  expect_equal(read_value, new_value)
})

test_that("phen_VegetativeTarget was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_VegetativeTarget" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].Vegetative.Target")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_EarlyFloweringTarget was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_EarlyFloweringTarget" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].EarlyFlowering.Target")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_EarlyPodDevTarget was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_EarlyPodDevTarget" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].EarlyPodDevelopment.Target")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_FractGrainFill was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_FractGrainFill" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].EarlyGrainFilling.Target.FractionofGrainfilling")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_EntGrainFill was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_EntGrainFill" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].LateGrainFilling.Target.EntireGrainfillPeriod")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_MidGrainFill was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_MidGrainFill" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].MidGrainFilling.Target.FractionofMidToLateGrainfilling")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_Maturing was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_Maturing" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].Maturing.Target")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_Ripening was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_Ripening" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].Ripening.Target")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_shootlag was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_shootlag" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].Emerging.Target.ShootLag")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("phen_shootrate was modified", {
  new_value <- 40
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "phen_shootrate" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Phenology].Emerging.Target.DepthxRate.ShootRate")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("leaf_RUE was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "leaf_RUE" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Leaf].Photosynthesis.RUE")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("leaf_AreaLargLeaf was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "leaf_AreaLargLeaf" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Leaf].AreaLargestLeaf")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("leaf_Phyllochron was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "leaf_Phyllochron" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Leaf].Phyllochron")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("leaf_ExtinctionCoef_Y1 was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "leaf_ExtinctionCoef_Y1" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Leaf].ExtinctionCoefficient.XYPairs")$node$Y[[1]]
  expect_equal(read_value, new_value)
})

test_that("grain_HarvIndex was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "grain_HarvIndex" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Grain].PotentialHarvestIndex")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("root_EarlyFrontVel was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "root_EarlyFrontVel" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Root].RootFrontVelocity.PotentialRootFrontVelocity.early.Function")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("root_LateFrontVel was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "root_LateFrontVel" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Root].RootFrontVelocity.PotentialRootFrontVelocity.late.Function")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("nodule_VegGrowthRate was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "nodule_VegGrowthRate" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.VegetativeGrowth.Rate")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("nodule_RepGrowthRate was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "nodule_RepGrowthRate" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Nodule].FixationRate.DailyPotentialFixationRate.PotentialFixationRate.SpecificFixationRate.ReproductiveGrowth.Rate")$node$FixedValue
  expect_equal(read_value, new_value)
})

test_that("nodule_MaxFixRate was modified", {
  new_value <- 2.0
  replace_values(
    apsimx_path = test_path,
    list_params_values = list(
      "nodule_MaxFixRate" = new_value
    )
  )
  read_value <- rapsimng::search_path(rapsimng::read_apsimx(test_path), path = "[Nodule].FixationRate.DailyPotentialFixationRate.MaximumFixationRate")$node$FixedValue
  expect_equal(read_value, new_value)
})
