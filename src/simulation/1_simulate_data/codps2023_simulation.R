# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# libraries
library(dplyr)

# check working directory
getwd()

# output directory
outdir <- file.path('wd', 'in', 'cod-ps_2023')
dir.create(outdir, showWarnings=F, recursive=T)

# load data
codps <- read.csv(file.path(outdir, 'DO_NOT_SHARE_UKR_ADM2_POP_2023.csv'))

result <- codps |> 
  select(year, ISO3, ADM1_NAME, ADM1_PCODE, ADM2_NAME, ADM2_PCODE, T_TL) |>
  mutate(
    T_TL_sim = ifelse(is.na(T_TL), NA, rlnorm(n(), log(T_TL), 0.5/2))
  ) |>
  mutate(
    T_TL_sim = ifelse(is.na(T_TL_sim), NA, round(T_TL_sim * (sum(T_TL, na.rm=T) / sum(T_TL_sim, na.rm=T))))
  ) |> 
  select(year, ISO3, ADM1_NAME, ADM1_PCODE, ADM2_NAME, ADM2_PCODE, T_TL_sim) |>
  rename(T_TL = T_TL_sim)

# save to disk
write.csv(result, file.path(outdir, 'UKR_ADM2_POP_2023_sim.csv'), row.names=F)
