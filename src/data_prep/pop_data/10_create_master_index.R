
rm(list = ls())
gc()

source(file.path(here::here(), "src", "helpers", "R_helpers", "generic.R"))
source(file.path(here::here(), "src", "helpers", "R_helpers", "data_querying.R"))

agesex <- c("F_20_29", "F_30_39", "F_40_49", "F_50_59", "F_60Plus", "F_15_19",
            "M_20_29", "M_30_39", "M_40_49", "M_50_59", "M_60Plus", "M_15_19")
  
date_start <- "2022-02-25"
date_end   <- "2024-05-14"
country    <- "UA"

meta_keys <- read_csv(file.path(env$repo_dir, "data", "meta", paste0(tolower(country), "_meta_keys.csv")))
pcodes    <- read_csv(file = file.path(env$repo_dir, "data", "COD-PS", "2022", "population_baseline22.csv"))

output_label <- ""

geo_index <- meta_keys |>
  rename(i_key = geo_key) |>
  distinct(i_key) |>
  left_join(
    pcodes |>
      mutate(macroregion = case_when(
           name=="Autonomous Republic of Crimea" ~ "Autonomous",
           name=="Cherkasy Oblast"                  ~ "Center",
           name=="Chernihiv Oblast"                  ~ "North",
           name=="Chernivtsi Oblast"                 ~ "West",
           name=="Dnipropetrovsk Oblast"             ~ "East",
           name=="Donetsk Oblast"                    ~ "East",
           name=="Ivano-Frankivsk Oblast"            ~ "West",
           name=="Kharkiv Oblast"                    ~ "East",
           name=="Kherson Oblast"                    ~ "South",
           name=="Khmelnytskyi Oblast"               ~ "West",
           name=="Kyiv Oblast"                       ~ "North",
           name=="Kirovohrad Oblast"                 ~ "Center",
           name=="Kyiv"                              ~ "City",
           name=="Luhansk Oblast"                    ~ "East",
           name=="Lviv Oblast"                       ~ "West",
           name=="Mykolaiv Oblast"                   ~ "South",
           name=="Odessa Oblast"                     ~ "South",
           name=="Poltava Oblast"                    ~ "Center",
           name=="Rivne Oblast"                      ~ "West",
           name=="Sevastopol"                        ~ "Autonomous",
           name=="Sumy Oblast"                       ~ "North",
           name=="Ternopil Oblast"                   ~ "West",
           name=="Vinnytsia Oblast"                  ~ "Center",
           name=="Volyn Oblast"                      ~ "West",
           name=="Zakarpattia Oblast"                ~ "West",
           name=="Zaporizhia Oblast"                 ~ "East",
           name=="Zhytomyr Oblast"                   ~ "North"),
          e_name = case_when(
            macroregion=="Autonomous" ~ "Autonomous & East",
            macroregion=="East" ~ "Autonomous & East",
            macroregion=="Center" ~ "Center", 
            macroregion=="City" ~ "City",
            macroregion=="North" ~ "North", 
            macroregion=="South" ~ "South",
            macroregion=="West" ~ "West"),
          e = case_when(e_name=="Autonomous & East" ~ 6,
                        e_name=="Center" ~ 2, 
                        e_name=="City" ~ 1,
                        e_name=="North" ~ 5, 
                        e_name=="South" ~ 4,
                        e_name=="West" ~ 3) %>% as.integer()) |> 
      select(fb_key, ADM1_PCODE, ADM1_EN, macroregion, e_name, e) |>
      rename(i_key = fb_key, i_name = ADM1_EN)
  ) |>
  arrange(i_key) |>
  mutate(
    i = 1:n(),
    i_key = as.integer(i_key)
  )

time_index_expanded <- tibble(
  collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)
) |>
  mutate(
    t_name = floor_date(as.Date(collection_date), "week", week_start = 1),
    t_key  = str_replace_all(as.character(t_name), "-", "") |> as.integer(),
    t      = t_name |> as.character() |> as_factor() |> as.integer()
  )

time_index <- time_index_expanded |>
  distinct(t_name, t_key, t) |>
  arrange(t)

agesex_index <- lapply(
  agesex, function(agesex_) {
    agesex_col_to_query_args(agesex_) |>
      as_tibble() |>
      mutate(agesex = agesex_)
  }
) |>
  bind_rows() |>
  mutate(
    a_name = str_sub(agesex, 3),
    a      = as.integer(factor(a_name,
                               levels = c("20_29","30_39","40_49","50_59","60Plus","15_19"))),
    a_key  = paste0(age_min, str_pad(age_max, 3, pad = "0")) |> as.integer(),
    s_name = str_sub(agesex, 1, 1),
    s      = ifelse(s_name == "F", 2, 1),
    s_key  = s
  ) |>
  select(-age_min, -age_max, -gender)


master_index <- expand_grid(
  t = unique(time_index$t),
  i = geo_index$i,
  a = unique(agesex_index$a),
  s = unique(agesex_index$s)
) |>
  left_join(time_index, by = "t") |>
  left_join(geo_index, by = "i") |>
  left_join(agesex_index, by = c("a", "s")) |>
  mutate(parameter = row_number()) |>
  arrange(t, i, a, s) |>
  mutate(tias = row_number())


write_csv(master_index, file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
write_csv(time_index_expanded, file.path(out_dir, paste0(tolower(country), "_time_index", output_label, ".csv")))

