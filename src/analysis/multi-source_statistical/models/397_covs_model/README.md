# Beyond crisis nowcasting: A Bayesian framework for integrating multiple social media sources into population estimates

This repository contains code and documentation to reproduce the modelling workflow for the paper:

**Beyond crisis nowcasting: A Bayesian framework for integrating multiple social media sources into population estimates**  
Target journal: **Proceedings of the National Academy of Sciences of the United States of America (PNAS)**

The case study is Ukraine (oblast level). The workflow fits a Bayesian model that integrates Facebook and Instagram audience data into weekly age and sex specific population estimates.

## Data notes and access constraints
Some inputs used by the scripts are restricted and must not be committed to a public repository. In particular, the COD-PS 2023 and 2024.

## Software requirements
- R (recent version)
- CmdStan and the R interface `cmdstanr`
- R packages used by the modelling and evaluation scripts include: `cmdstanr`, `posterior`, `bayesplot`, `ggplot2`, `tidyverse`, `tidyr`, `readxl`, `patchwork`, `purrr`, and others used by helper scripts. 

## Configuration via .env
The modelling scripts load a `.env` file, where key directories such as `repo_dir`, `in_dir`, and `out_dir` are defined. 

Create a `.env` file at the repository root. Use `example.env` as a template.

## Workflow
**Step 1. Build population proxy inputs**
Run these scripts (in order) from the folder `src/data_prep/pop_data/1_pop_data/`:

- `10_create_master_index.R`
- `20_prepare_sma_data.R`
- `30_border_crossing.R`

These scripts create outputs for: `out_dir/population_proxy/`

**Step 2. Fit the model 397_covs_model**
This workflow is currently model specific. Therefore, the Stan model file named `397_covs_model.stan` alongside all model specific files must be in the same folder. They are:

- `397_covs_model_config.R`: Prepares the data list `md` used as input to Stan, including index construction, anchor populations, and derived ratios used to impute child counts from women of reproductive ages.
- `init_generator_fun_397.R`: Creates initial values for sampling with checks on expected dimensions and seeded initialisation by chain identifier. 
- `1_mcmc_397.R`: Runs Bayesian sampling via `cmdstanr`, with threading enabled and multiple chains. 
- `2_eval_397.R`: Produces evaluation outputs including trace plots, posterior predictive checks, time series plots, and population pyramid plots.

If you run `1_mcmc_397.R` directly, it will source the model configuration script `397_covs_model_config.R`.

## Outputs
The evaluation script expects these files to exist:
- `out_dir/modelling/397_covs_model/mcmc/md_397_covs_model.rds`: input data created by `397_covs_model_config.R`.
- `out_dir/modelling/397_covs_model/mcmc/fit_397_covs_model.rds`: fitted model create by `1_mcmc_397.R`.

Bear in mind that the resulted MCMC file (`fit_397_covs_model.rds`) is around 6.3MB. The model takes nearly 6.2 hours to run with settings specified in `1_mcmc_397.R`.

