variable_names <- c(
  # "phen_VegAndRepTherTimRes_bothX3",
  # "phen_VegAndRepPhoMod_bothX1",
  "phen_VegTherTimeResp_X3",
  "phen_RepTherTimeResp_X3",
  "phen_VegPhoMod_X1",
  "phen_RepPhoMod_X1",
  "phen_VegetativeTarget",
  "phen_EarlyFloweringTarget",
  "phen_EarlyPodDevTarget",
  "phen_FractGrainFill",
  "phen_EntGrainFill",
  "phen_MidGrainFill",
  "phen_Maturing",
  "phen_Ripening",
  # "phen_shootlag",
  # "phen_shootrate",
  "leaf_RUE",
  "leaf_AreaLargLeaf",
  "leaf_Phyllochron",
  "leaf_ExtinctionCoef_Y1",
  "grain_HarvIndex"
  # "root_EarlyFrontVel",
  # "root_LateFrontVel",
  # "nodule_VegGrowthRate",
  # "nodule_RepGrowthRate",
  # "nodule_MaxFixRate"
  # "soil_KL"
)

variable_bounds <- list(
  # c(24, 36),      #phen_VegAndRepTherTimRes_bothX3
  # c(11.5, 17.3),  #phen_VegAndRepPhoMod_bothX1
  c(24, 36),        #phen_VegTherTimeResp_X3
  c(24, 36),        #phen_RepTherTimeResp_X3
  c(12, 17),    #phen_VegPhoMod_X1
  c(12, 17),    #phen_RepPhoMod_X1
  c(150, 250),      #phen_VegetativeTarget
  c(150, 250),      #phen_EarlyFloweringTarget
  c(100, 200),       #phen_EarlyPodDevTarget
  c(0.03, 0.07),  #phen_FractGrainFill
  c(350, 650),      #phen_EntGrainFill
  c(0.3, 0.7),    #phen_MidGrainFill
  c(30, 60),    #phen_Maturing
  c(30, 60),    #phen_Ripening
  # c(5, 15),         #phen_shootlag
  # c(0.5, 1.5),      #phen_shootrate
  c(1, 1.4),      #leaf_RUE
  c(0.004, 0.008),  #leaf_AreaLargLeaf
  c(30, 60),    #leaf_Phyllochron
  c(0.4, 0.8),      #leaf_ExtinctionCoef_Y1
  c(0.3, 0.7)     #grain_HarvIndex
  # c(21.0, 39.0),    #root_EarlyFrontVel
  # c(3.5, 6.5),      #root_LateFrontVel
  # c(0.004, 0.008),  #nodule_VegGrowthRate
  # c(0.0015, 0.0025),#nodule_RepGrowthRate
  # c(0.4, 0.8)       #nodule_MaxFixRate
  # c(0.06, 0.1)      #soil_KL
)

problem <- list(
  num_vars = length(variable_names),
  names = variable_names,
  bounds = variable_bounds
)