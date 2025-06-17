# R Apsimx Sensitivity

## Create conda env

```bash
conda create -c conda-forge -n rapsimx python=3.11 ipykernel numpy pandas salib r-base=4.4 r-devtools r-tidyverse r-irkernel
# rapsimx dependencies:
conda activate rapsimx
conda install -c conda-forge r-reticulate r-xml2 r-rjson r-future r-reticulate r-future.apply
Rscript -e "install.packages(c('apsimx','rapsimng','future.apply'), repos = 'https://cloud.r-project.org')"
python3 -m ipykernel install --name rapsimx --prefix=$CONDA_PREFIX --display=rapsimx
Rscript -e "options(warn=2); IRkernel::installspec( user = FALSE, prefix = '$CONDA_PREFIX', displayname = 'rapsimx')"
```

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
...
```
