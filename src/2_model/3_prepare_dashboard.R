# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
library(cmdstanr)
library(matrixStats)

# Script parameter
model_name <- "211_age_sex_model"

# Load data
master_index <- read_csv(file.path(
  out_dir,
  paste0(tolower(country), "_master_index", output_label, ".csv")
))
fit <- readRDS(file.path(
  out_dir,
  "model",
  "age_sex_model",
  paste0("fit_", model_name, ".rds")
))

# Create dashboard indexing
master_index <- master_index |>
  separate(
    col = a_name,
    into = c("age_min0", "age_max0"),
    sep = "_",
    convert = TRUE
  ) |>
  mutate(
    country = rep("UKR", length(ADM1_PCODE)),
    admin_level = rep(1, length(ADM1_PCODE)) |> as.integer(),
    age_min = ifelse(
      str_detect(age_min0, "Plus"),
      str_sub(age_min0, 1, 2),
      round(as.numeric(age_min0), 0)
    ) |>
      as.integer(),
    age_max = ifelse(
      str_detect(age_min0, "Plus"),
      999,
      round(as.numeric(age_max0), 0)
    ) |>
      as.integer()
  ) |>
  rename(
    "pcode" = "ADM1_PCODE",
    "day" = "t_name",
    "sex" = "s"
  )

# Convert fit object to dashboard input csv
convert_pop_1Darray_dashboard <- function(
  fit_object,
  master_index = master_index
) {
  pop_stocks <- fit_object$draws(variables = "N", format = "df")

  pop_dashboard <- pop_stocks |>
    select(-starts_with(".")) |>
    t() |>
    as.data.frame()

  pop_dashboard <- pop_dashboard |>
    rownames_to_column(var = "parameter") |>
    mutate(parameter = str_sub(parameter, 3, -2) |> as.integer()) |>
    left_join(
      master_index |>
        select(
          country,
          admin_level,
          pcode,
          day,
          age_min,
          age_max,
          sex,
          parameter
        ),
      by = "parameter"
    ) |>
    mutate(
      pop = rowMeans(pick(starts_with("V")), na.rm = TRUE) |> as.integer(),
      pop_lower = rowQuantiles(
        as.matrix(pick(starts_with("V"))),
        probs = 0.025
      ) |>
        as.integer(),
      pop_upper = rowQuantiles(
        as.matrix(pick(starts_with("V"))),
        probs = 0.975
      ) |>
        as.integer(),
      across(starts_with("V"), ~ as.integer(round(.))),
      pop_posterior = paste0(
        "[",
        apply(pick(starts_with("V")), 1, function(x) paste(x, collapse = ",")),
        "]"
      )
    ) |>
    select(-starts_with("V", ignore.case = FALSE))

  write.csv(
    pop_dashboard,
    file = file.path(
      env$repo_dir,
      "src",
      "dashboard",
      "api",
      "app",
      "data",
      "db-data",
      "pop.csv"
    ),
    row.names = FALSE
  )
  return(pop_dashboard)
}

pop <- convert_pop_1Darray_dashboard(fit)

# adapt geographies

library(sf)
geo <- st_read('./src/dashboard/api/app/data/db-data/GEODATA_full.gpkg')
pop <- read_csv('./src/dashboard/api/app/data/db-data/pop.csv') |>
  filter(admin_level == 3 & day == min(day)) |>
  distinct(pcode)

geo_3 <- geo |>
  filter(admin_level == 3) |>
  left_join(
    pop |>
      mutate(
        gct = T
      )
  ) |>
  mutate(
    pcode_1 = str_sub(pcode, 1, 4),
    pcode_2 = str_sub(pcode, 1, 6)
  )

geo_ <- bind_rows(
  geo_3 |> select(pcode, geom, gct) |> mutate(admin_level = 3),
  geo_3 |>
    group_by(
      pcode_1,
      gct
    ) |>
    summarise() |>
    select(pcode = pcode_1, gct, geom) |>
    mutate(admin_level = 1),
  geo_3 |>
    group_by(
      pcode_2,
      gct
    ) |>
    summarise(n = n()) |>
    select(pcode = pcode_2, gct, geom) |>
    mutate(admin_level = 2)
)

geo_t <- geo_ |>
  full_join(
    geo |>
      st_drop_geometry()
  )

geo_t <- st_buffer(geo_t, 0.0)

geo_t <- geo_t |>
  mutate(
    country = 'UKR',
    pcode = ifelse(is.na(gct), paste0('Missing ', pcode), pcode),
    name_en = ifelse(is.na(gct), paste0('Missing ', name_en), name_en)
  ) |>
  select(-gct)

st_write(
  geo_t,
  './src/dashboard/api/app/data/db-data/GEODATA.gpkg',
  append = FALSE,
  layer = 'UKR'
)

st_write(
  geo_t |> filter(admin_level == 1),
  './src/dashboard/www/public_html/data/admin_UKR_level_1.geojson'
)

st_write(
  geo_t |> filter(admin_level == 2),
  './src/dashboard/www/public_html/data/admin_UKR_level_2.geojson',
  append = F
)

st_write(
  geo_t |> filter(admin_level == 3),
  './src/dashboard/www/public_html/data/admin_UKR_level_3.geojson',
  append = FALSE
)

st_write(
  geo |> filter(admin_level == 1),
  './src/dashboard/www/public_html/data/admin_UKR_level_1_baseline.geojson',
)
tm_shape(geo_t |> filter(admin_level == 1)) +
  tm_polygons("name_en")
