source(file.path(here::here(), "src", "helpers", "R_helpers", "generic.R"))

hromada_kyiv <- st_read(file.path(in_dir, "COD-AB", "ukr_admbnda_sspe_20230201_SHP", "ukr_admbnda_adm1_sspe_20230201.shp")) |>
  filter(ADM1_EN == "Kyiv")
hromada_df <- read_csv(file.path(in_dir, "KSE-Loc-Data-Hub", "full_dataset.csv"))
hromada_geo <- st_read(file.path(in_dir, "KSE-Loc-Data-Hub", "KSE-Loc-Data-Hub", "data", "derived", "shapefiles", "admin", "terhromad_fin.geojson"))

# Add Kyiv
hromada_geo <- st_make_valid(hromada_geo)

hromada_geo <- bind_rows(
  hromada_geo,
  hromada_kyiv |>
    mutate(COD_3 = "Kyiv") |>
    select(COD_3)
)

# Standardise
hromada_geo <- hromada_geo |>
  filter(TYPE != "Державні території" | is.na(TYPE)) |>
  rename(
    hromada_code = COD_3
  ) |>
  full_join(
    hromada_df |>
      select(hromada_code, hromada_name, oblast_name_en, raion_code, raion_name, oblast_name)
  ) |>
  mutate(
    oblast_name_en = ifelse(ADMIN_1 == "Автономна Республіка Крим", "Autonomous Republic of Crimea", oblast_name_en),
    across(
      c(hromada_name, raion_name, raion_code, oblast_name), ~ ifelse(ADMIN_1 == "Автономна Республіка Крим", "Автономна Республіка Крим", .x)
    ),
    across(
      c(hromada_name, raion_name, raion_code, oblast_name), ~ ifelse(hromada_code == "Kyiv", "Київ", .x)
    ),
    oblast_name_en = ifelse(hromada_code == "Kyiv", "Kyiv", oblast_name_en),
    raion_name = case_when(
      oblast_name == "Луганська" ~ "Luhanska",
      oblast_name == "Донецька" ~ "Donetska",
      TRUE ~ raion_name
    ),
    oblast_name_en = case_when(
      oblast_name == "Луганська" ~ "Luhanska",
      oblast_name == "Донецька" ~ "Donetska",
      oblast_name_en == "Vinnytsia" ~ "Vinnytska",
      oblast_name_en == "Driproptrovska" ~ "Dnipropetrovska",
      oblast_name_en == "Zhytomir" ~ "Zhytomyrska",
      oblast_name_en == "Ivano-Frankivsk" ~ "Ivano-Frankivska",
      oblast_name_en == "Kherson" ~ "Khersonska",
      oblast_name_en == "Kirovograd" ~ "Kirovohradska",
      oblast_name_en == "Kyiv-oblast" ~ "Kyivska",
      oblast_name_en == "Luhanska" ~ "Luhanska",
      oblast_name_en == "Lviv" ~ "Lvivska",
      oblast_name_en == "Mykolayiv" ~ "Mykolaivska",
      oblast_name_en == "Odesa" ~ "Odeska",
      oblast_name_en == "Poltava" ~ "Poltavska",
      oblast_name_en == "Rivenska" ~ "Rivnenska",
      oblast_name_en == "Vonyn" ~ "Volynska",
      oblast_name_en == "Kharkiv" ~ "Kharkivska",
      oblast_name_en == "Khmelnitsk" ~ "Khmelnytska",
      oblast_name_en == "Cherkassy" ~ "Cherkaska",
      oblast_name_en == "Cherniveska" ~ "Chernivetska",
      oblast_name_en == "Chernigiv" ~ "Chernihivska",
      TRUE ~ oblast_name_en
    )
  ) |>
  select(-starts_with("ADMIN"), -TYPE, -KOATUU_old)

# Standardise with COD-AB
#   full_join(
#     raion_geo |>
#       mutate(
#         ADM2_UA = ifelse(ADM2_UA == "Кам'янець-Подільський", "Кам’янець-Подільський", ADM2_UA),
#         ADM2_UA = ifelse(ADM2_UA == "Володимирський", "Володимир-Волинський", ADM2_UA),
#         ADM2_UA = ifelse(ADM2_UA == "Звягельський", "Новоград-Волинський", ADM2_UA)
#       ) |>
#       select(ADM2_EN, ADM2_UA, ADM1_EN, ADM1_UA, ADM1_PCODE, ADM2_PCODE) |>
#       st_drop_geometry() |>
#       rename(
#         raion_name = ADM2_UA,
#         oblast_name = ADM1_UA,
#       )
#   )

# Write output

write_csv(hromada_geo |> st_drop_geometry(), file.path(out_dir, "ua_master_hromada.csv"))
st_write(hromada_geo, file.path(out_dir, "ua_master_hromada.gpkg"), append = FALSE)


# Visualise
tm_shape(hromada_geo) + tm_polygons()
