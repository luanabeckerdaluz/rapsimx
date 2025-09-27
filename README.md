# R Apsimx Sensitivity

## APSIMx and Conda Configuration

### Create conda env with R and Python

```bash
conda create -n rgeo -c conda-forge python=3.11 ipykernel salib r-base r-essentials r-devtools r-remotes r-tidyverse r-xslt r-ggrepel r-lhs r-gridextra r-doparallel r-xml r-rsqlite r-plogr r-pak r-nloptr r-tictoc zlib r-lme4 r-future.apply r-future r-xml2 r-here r-png r-reticulate r-rcpptoml r-rjson -y
# rapsimx dependencies:
conda activate rapsimx
## python3 -m ipykernel install --name rapsimx --prefix=$CONDA_PREFIX --display=rapsimx
Rscript -e "options(warn=2); IRkernel::installspec(name = 'rgeo', displayname = 'R APSIMx')"
Rscript -e "options(warn = 2, timeout = 300, repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); pak::pkg_install(c('hol430/ApsimOnR', 'SticsRPacks/CroPlotR@*release', 'SticsRPacks/SticsRFiles@*release', 'SticsRPacks/SticsOnR@*release', 'SticsRPacks/CroptimizR@*release', 'apsimx', 'rapsimng', 'BayesianTools'))"
```

**Obs: In case of error when detecting packages already installed, use:**

```bash
export PKG_CONFIG_PATH=$CONDA_PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH
```

### Install dotnet

```bash
wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh
chmod +x ./dotnet-install.sh
DOTNET_INSTALLATION_PATH=$HOME/dotnet
# ./dotnet-install.sh --runtime dotnet --version 8.0.0 --install-dir $DOTNET_INSTALLATION_PATH
./dotnet-install.sh --channel 8.0 --install-dir $DOTNET_INSTALLATION_PATH
./dotnet --info
export PATH=$HOME/dotnet:$PATH
```

### Install APSIMx on Ubuntu

```bash
APSIMX_INSTALLATION_DIR=$HOME/apsim_$VERSION
mkdir -p $APSIMX_INSTALLATION_DIR
APSIMX_VERSION=7850
wget -O apsim_$APSIMX_VERSION.deb https://builds.apsim.info/api/nextgen/download/$APSIMX_VERSION/Linux
dpkg-deb -x apsim_$APSIMX_VERSION.deb $APSIMX_INSTALLATION_DIR
```

### Test APSIMx with dotnet:

```bash
$DOTNET_INSTALLATION_PATH/dotnet $APSIMX_INSTALLATION_DIR/usr/local/lib/apsim/...$APSIMX_VERSION.../bin/Models.dll -V
```

### Configuring Models and Apsim executables

```bash
cd $APSIMX_INSTALLATION_DIR/usr/local/bin
cp Models Models.OLD
cp apsim apsim.OLD
echo 'exec dotnet $APSIMX_INSTALLATION_DIR/usr/local/lib/apsim/...$APSIMX_VERSION.../bin/Models.dll "$@"' > Models
echo 'exec dotnet $APSIMX_INSTALLATION_DIR/usr/local/lib/apsim/...$APSIMX_VERSION.../bin/ApsimNG.dll "$@"' > apsim
```

---

## About APSIMx Models

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
