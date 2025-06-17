# R Apsimx Sensitivity

## Create conda env

```bash
conda create -c conda-forge -n rapsimx python=3.11 ipykernel salib r-base=4.4 r-devtools r-tidyverse r-irkernel
# rapsimx dependencies:
conda activate rapsimx
conda install -c conda-forge r-reticulate r-xml2 r-rjson r-future r-future.apply r-lhs
Rscript -e "install.packages(c('apsimx','rapsimng'), repos = 'https://cloud.r-project.org')"
python3 -m ipykernel install --name rapsimx --prefix=$CONDA_PREFIX --display=rapsimx
Rscript -e "options(warn=2); IRkernel::installspec( user = FALSE, prefix = '$CONDA_PREFIX', displayname = 'rapsimx')"
```

## Install APSIMx

```bash
curl --output apsim.deb https://builds.apsim.info/api/nextgen/download/7770/Linux
mkdir -p /home/jovyan/meuapsimx/
dpkg-deb -x apsim.deb /home/jovyan/meuapsimx/
# Install dotnet
wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh
chmod +x ./dotnet-install.sh
./dotnet-install.sh --version latest
# Test if dotnet is installed
$HOME/.dotnet/dotnet --info
# Test apsimx Models
$HOME/.dotnet/dotnet $HOME/meuapsimx/usr/local/lib/apsim/2025.6.7770.0/bin/Models.dll -V
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
