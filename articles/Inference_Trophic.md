# Inference_Trophic

``` r

library(spacemodR)
# Additional package for further analysis
library(rstan)
#> Loading required package: StanHeaders
#> 
#> rstan version 2.32.7 (Stan version 2.32.2)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
#> For within-chain threading using `reduce_sum()` or `map_rect()` Stan functions,
#> change `threads_per_chain` option:
#> rstan_options(threads_per_chain = 1)
library(ggplot2)
```

``` r

read_sf_safe <- function(file_path) {
  paths <- c("", "../", "../../") |> paste0(file_path)
  valid_path <- paths[file.exists(paths)][1]
  if (!is.na(valid_path)) {
    return(sf::read_sf(valid_path))
  }
  warning("File not found at: ", paste(paths, collapse = " OR "))
  return(NULL)
}
load_safe <- function(file_path) {
  paths <- c("", "../", "../../") |> paste0(file_path)
  valid_path <- paths[file.exists(paths)][1]
  if (!is.na(valid_path)) {
    return(readRDS(valid_path))
  }
  warning("File not found at: ", paste(paths, collapse = " OR "))
  return(NULL)
}
```

``` r

PATH_MAMMAL = "raw_data/sf_metaleurop_2006_2010.gpkg"
PATH_BAPPET = 'raw_data/BAPPET.csv'
PATH_WORM = 'raw_data/Cd_Sample1998_earthworm.csv'
```

``` r

# PLANTS DATASETS
dp = pd.read_csv(PATH_BAPPET)
dp = dp[(dp.etm == 'Cd') & (dp.media == 'Sol (mg/kg)')]
dp["cd_soil_log10"] = np.log10(dp['media_mean_float'])
dp["cd_plant_log10"] = np.log10(dp['plant_mean_float'])
plt.figure(figsize=c(10, 4))
sns.scatterplot(data=dp, x="cd_soil_log10", y="cd_plant_log10")
```

``` r
# WORM DATASETS
dw = pd.read_csv(PATH_WORM)
dw["cd_soil_log10"] = np.log10(dw['soil concentration (mg/kg dry wt.)'])
dw["cd_worm_log10"] = np.log10(dw['earthworm concentration (mg/kg dry wt.)'])
plt.figure(figsize=(10, 4))
sns.scatterplot(data=dw, x="cd_soil_log10", y="cd_worm_log10")
```

``` r
dm = pd.read_csv(PATH_MAMMAL)
dm['log10_cd_S'] = np.log10(dm['cd_S'])
dm['log10_cd_WB_FW'] = np.log10(dm['cd_WB_FW'])
# ADD GROUP HERBIVOE vs INSECTIVORE
lookup_table = pd.DataFrame({
    'group': ['shrew', 'mouse', 'vole'],
    'food': ['insectivore', 'herbivore', 'herbivore'],
    'food_cat': ['2', '1', '1'],
    'food_cat_num': [2, 1, 1]})
dm = dm.merge(lookup_table, on='group', how='left')
```

``` r
dh = dm[dm.food == "herbivore"]
di = dm[dm.food == "insectivore"]
dp = dp[(dp['cd_soil_log10'].isna() == False) & (dp['cd_plant_log10'].isna() == False)]
dw = dw[(dw['cd_soil_log10'].isna() == False) & (dw['cd_worm_log10'].isna() == False)]
dh = dh[(dh['log10_cd_S'].isna() == False) & (dh['log10_cd_WB_FW'].isna() == False)]
di = di[(di['log10_cd_S'].isna() == False) & (di['log10_cd_WB_FW'].isna() == False)]

xp = dp["cd_soil_log10"].to_numpy()
yp = dp["cd_plant_log10"].to_numpy()
is_finit = np.isfinite(xp) & np.isfinite(yp)
xp = xp[is_finit]
yp = yp[is_finit]

stan_data = {"Np": len(xp),
             "Nw": dw.shape[0], "Nh": dh.shape[0], "Ni": di.shape[0],
             "xp": xp, "yp": yp,
             "xw": dw["cd_soil_log10"].to_numpy(), "yw": dw["cd_worm_log10"].to_numpy(),
             "xh": dh["log10_cd_S"].to_numpy(), "yh": dh["log10_cd_WB_FW"].to_numpy(),
             "xi": di["log10_cd_S"].to_numpy(), "yi": di["log10_cd_WB_FW"].to_numpy(),
             "M": 100,
             "x_sim": np.linspace(xp.min(),xp.max(),100)
}

stan_fit = stan_model.sample(num_chains=2, num_warmup=500, num_samples=1000)
```
