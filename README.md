# R Apsimx Sensitivity

## What we learned

### Cultivar parameters overlap Replacements

The cultivar parameters overlap the parameters defined in Replacements, which means the sensitivity analysis won't update the parameter. In order to Replacements to take effect, open you simulation on Apsim interface and then:
1) Go to **Simulations** >> **Replacements** >> **Soybean** >> **Cultivars** and create a new generic cultivar.
2) Inside **Command**, it is not possible to leave it empty. So choose one parameter that has no influence on crop simulation and set (e.g. `[Phenology].Emerging.Target.DepthxRate.ShootRate.FixedValue = 1`).
3) Then, inside **Simulations**, open each one of these and go to **paddock** >> **Sow on a fixed date** and set this new cultivar.

### Set Harvest Report

1) For each one of you simulations inside **Simulations**, go to **paddock** and make sure that you have **HarvestReport** created with some variables as below:

```json
"[Clock].Today",
"[Soybean].AboveGround.Wt*10 as Biomass",
"[Soybean].Grain.Total.Wt*10 as Yield",
"[Soybean].Phenology.FloweringDAS",
"[Soybean].Phenology.MaturityDAS"
```
