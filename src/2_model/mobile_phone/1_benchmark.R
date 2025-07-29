source(file.path(here::here(), "R_helpers/generic.R"))
source(here::here(".env"), local = env)
dir.create(file.path(here::here(), "wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "wd"))

repo_dir <- env$repo_dir
data_dir <- file.path(env$repo_dir, "data")
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex_domestic.csv")) %>%
  filter(destination_oblast != "Abroad") |>
  mutate(
    origin_raion = ifelse(origin_raion == "Київ", "Kyiv", origin_raion),
    destination_raion = ifelse(destination_raion == "Київ", "Kyiv", destination_raion))

stocks_hromada_agesex <- flows_hromada_agesex |>
  as_tibble() |>
  group_by(t, a_name, s_name, hromada = destination_hromada, oblast = destination_oblast, raion = destination_raion) |>
  summarise(
    pop_estimated = sum(monthlyFlow_hat_calibrated),
    .groups = "drop" ) %>% ungroup() %>%
  rename(ADM3_Minrehion_CODE=hromada)
  
        
key <- c("ADM3_Minrehion_CODE") 

clean_key <- function(x) {x %>%
    str_trim() %>%                      
    str_squish() %>%                    
    str_replace_all("[[:punct:]]", "") %>%  
    stringi::stri_trans_general("Latin-ASCII")  }

stocks_hromada_agesex <- stocks_hromada_agesex %>%
    mutate(!!key := clean_key(!!sym(key)))


geo_linkADM3 <- readxl::read_excel(file.path(data_dir, "COD-PS", "2022", "ukr_gazetteer_v01.xlsx"), sheet = "ukr_admgz_adm3") %>%
  dplyr::select(ADM1_PCODE,	ADM2_PCODE,	ADM3_PCODE,	Minrehion_CODE) %>%
  rename(ADM3_Minrehion_CODE=Minrehion_CODE)

geo_linkADM2 <- readxl::read_excel(file.path(data_dir, "COD-PS", "2022", "ukr_gazetteer_v01.xlsx"), sheet = "ukr_admgz_adm3") %>%
  dplyr::select(ADM1_PCODE,	ADM2_PCODE,	ADM3_PCODE,	Minrehion_CODE) %>%
  rename(ADM2_Minrehion_CODE=Minrehion_CODE)


geo <- readxl::read_excel(file.path(data_dir, "COD-PS", "ukr_adminboundaries_tabulardata.xlsx"), sheet = "ADM3") %>%
  dplyr::select(ADM1_EN, ADM1_PCODE, ADM2_PCODE, ADM2_EN, ADM3_PCODE, ADM3_EN, AREA_SQKM) %>%
  rename(ADM3_AREA_SQKM=AREA_SQKM) %>%
  left_join(geo_linkADM3) %>%
  left_join(geo_linkADM2) %>%
  group_by(ADM2_PCODE, ADM2_EN) %>% 
  mutate(ADM2_AREA_SQKM = sum(ADM3_AREA_SQKM, na.rm = TRUE),
         HRM_SHARE_RAION = ADM3_AREA_SQKM / ADM2_AREA_SQKM) %>% ungroup() %>%
  group_by(ADM1_PCODE, ADM1_EN, .add = TRUE) %>% 
  mutate(ADM1_AREA_SQKM = sum(ADM3_AREA_SQKM, na.rm = TRUE),
         RAION_SHARE_OBLAST = ADM2_AREA_SQKM/ADM1_AREA_SQKM,
         HRM_SHARE_OBLAST = ADM3_AREA_SQKM/ADM1_AREA_SQKM) %>% ungroup() %>%
  mutate(!!key := clean_key(!!sym(key)),
         ADM3_Minrehion_CODE = ifelse(ADM1_EN=="Kyiv", "Kyiv", ADM3_Minrehion_CODE),
         ADM2_Minrehion_CODE = ifelse(ADM1_EN=="Kyiv", "Kyiv", ADM2_Minrehion_CODE))
  
date_start <- "2021-12-01"
date_end   <- "2025-02-01"
                   
time_index_expanded <- tibble(
    collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)) |>
    mutate(t_name = floor_date(as.Date(collection_date), "week", week_start = 1),
           t_key  = stringr::str_replace_all(as.character(t_name), "-", "") |> as.integer(),
           t = t_name |> as.character() |> haven::as_factor() |> as.integer())
                  
time_index <- time_index_expanded |>
                    distinct(t_name, t_key, t) |>
                    arrange(t)


codps22 <- read.csv(file.path(data_dir, "COD-PS", "2022", "population_baseline22.csv")) %>%
  transmute(ADM1_PCODE = ADM1_PCODE,
         #F_18_24 = 0.4*F_15_19 + F_20_24,    
         #M_18_24 = M_15_19 + M_20_24,
         F_25_34 = F_25_29 + F_30_34,    
         M_25_34 = M_25_29 + M_30_34,
         F_35_44 = F_35_39 + F_40_44,    
         M_35_44 = M_35_39 + M_40_44,
         F_45_54 = F_45_49 + F_50_54,    
         M_45_54 = M_45_49 + M_50_54,
         F_55_64 = F_55_59 + F_60_64,    
         M_55_64 = M_55_59 + M_60_64) %>%
  pivot_longer(
    cols      = 2:9,
    names_to  = "demog",
    values_to = "pop0") %>%
 separate(demog, into = c("s_name", "age_min", "age_max"), sep = "_") %>%
 mutate(a_name = paste0(age_min, "-",age_max)) %>%
 dplyr::select(ADM1_PCODE, s_name, a_name, pop0)


oblast_hrom_total <- geo %>%            
  distinct(ADM1_PCODE, ADM1_EN, ADM3_Minrehion_CODE) %>% 
  group_by(ADM1_PCODE, ADM1_EN) %>% 
  summarise(total_hromadas = n(), .groups = "drop")

oblast_hrom_covered <- comparison %>%    
  distinct(ADM1_PCODE, ADM3_Minrehion_CODE) %>% 
  group_by(ADM1_PCODE) %>% 
  summarise(covered_hromadas = n(), .groups = "drop")

coverage_check <- oblast_hrom_total %>% 
  left_join(oblast_hrom_covered, by = "ADM1_PCODE") %>% 
  mutate(
    covered_hromadas = replace_na(covered_hromadas, 0),
    full_coverage    = covered_hromadas == total_hromadas,
    coverage_prop    = covered_hromadas / total_hromadas )

oblasts_complete <- coverage_check %>% 
  filter(full_coverage) %>% 
  select(ADM1_PCODE, ADM1_EN, total_hromadas)   




codps23 <- read.csv(file.path(data_dir, "COD-PS", "2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv")) %>%
  rename(ADM1_EN=ADM1_NAME, ADM2_EN=ADM2_NAME) %>%
  transmute(ADM1_PCODE=ADM1_PCODE, ADM1_EN = ADM1_EN,
         ADM2_PCODE = ADM2_PCODE, ADM2_EN = ADM2_EN,
         #F_18_24 = 0.4*F_15_19 + F_20_24,    
         #M_18_24 = M_15_19 + M_20_24,
         F_25_34 = F_25_29 + F_30_34,    
         M_25_34 = M_25_29 + M_30_34,
         F_35_44 = F_35_39 + F_40_44,    
         M_35_44 = M_35_39 + M_40_44,
         F_45_54 = F_45_49 + F_50_54,    
         M_45_54 = M_45_49 + M_50_54,
         F_55_64 = F_55_59 + F_60_64,    
         M_55_64 = M_55_59 + M_60_64) %>%
  pivot_longer(
    cols      = 5:12,
    names_to  = "demog",
    values_to = "pop1") %>%
 separate(demog, into = c("s_name", "age_min", "age_max"), sep = "_") %>%
 mutate(a_name = paste0(age_min, "-",age_max),
        ADM2_PCODE = ifelse(ADM1_PCODE=="UA01"|ADM1_PCODE=="UA85", ADM1_PCODE, ADM2_PCODE),
        ADM2_EN = ifelse(ADM1_PCODE=="UA01"|ADM1_PCODE=="UA85", ADM1_EN, ADM2_EN)) %>%
 dplyr::select(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, s_name, a_name, pop1)


raion_hrom_total <- geo %>%                         
  distinct(ADM2_PCODE, ADM2_EN, ADM3_Minrehion_CODE) %>% 
  group_by(ADM2_PCODE, ADM2_EN) %>% 
  summarise(total_hromadas = n(), .groups = "drop")

raion_hrom_covered <- comparison %>%            
  distinct(ADM2_PCODE, ADM3_Minrehion_CODE) %>% 
  group_by(ADM2_PCODE) %>% 
  summarise(covered_hromadas = n(), .groups = "drop")

raion_coverage <- raion_hrom_total %>% 
  left_join(raion_hrom_covered, by = "ADM2_PCODE") %>% 
  mutate(
    covered_hromadas = replace_na(covered_hromadas, 0),
    full_coverage    = covered_hromadas == total_hromadas,
    coverage_prop    = covered_hromadas / total_hromadas )

raions_complete <- raion_coverage %>% 
  filter(full_coverage) %>% 
  arrange(ADM2_EN) %>% 
  select(ADM2_PCODE, ADM2_EN, total_hromadas)    



codps24 <- readxl::read_excel(file.path(data_dir, "COD-PS", "2024", "[restricted release] UKR_ADM2_POP_2024_Sept_27.xlsx"), sheet = 2) %>%
  rename(ADM1_EN=ADM1_NAME, ADM2_EN=ADM2_NAME) %>%
  transmute(ADM1_PCODE=ADM1_PCODE, ADM1_EN = ADM1_EN,
            ADM2_PCODE = ADM2_PCODE, ADM2_EN = ADM2_EN,
            #F_18_24 = 0.4*F_15_19 + F_20_24,    
            #M_18_24 = M_15_19 + M_20_24,
            F_25_34 = F_25_29 + F_30_34,    
            M_25_34 = M_25_29 + M_30_34,
            F_35_44 = F_35_39 + F_40_44,    
            M_35_44 = M_35_39 + M_40_44,
            F_45_54 = F_45_49 + F_50_54,    
            M_45_54 = M_45_49 + M_50_54,
            F_55_64 = F_55_59 + F_60_64,    
            M_55_64 = M_55_59 + M_60_64) %>%
pivot_longer(
cols      = 5:12,
names_to  = "demog",
values_to = "pop2") %>%
separate(demog, into = c("s_name", "age_min", "age_max"), sep = "_") %>%
mutate(a_name = paste0(age_min, "-",age_max),
       ADM2_PCODE = ifelse(is.na(ADM2_PCODE), ADM1_PCODE, ADM2_PCODE),
       ADM2_EN = ifelse(is.na(ADM2_EN), ADM1_EN, ADM2_EN)) %>%
dplyr::select(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, s_name, a_name, pop2)


ref_dates <- tibble(
  year_benchmark = c(2022, 2023, 2024),
  ref_date = as.Date(c("2022-01-01", "2023-07-01", "2024-09-01")))


comparison22 <- stocks_hromada_agesex %>% 
    select(-raion, -oblast) %>% 
    left_join(geo) %>% 
    filter(t == as.Date("2022-01-01"),     
           ADM1_PCODE %in% oblasts_complete$ADM1_PCODE,
          a_name!="18_24") %>% 
    group_by(ADM1_PCODE, ADM1_EN, a_name, s_name) %>% 
    summarise(pop_estimated = sum(pop_estimated), .groups = "drop") %>%
    left_join(codps22) %>%
    filter(a_name != "18_24" & a_name != "18-24")




ggplot(comparison22,
       aes(x = pop0/1000, y = pop_estimated/1000, color = a_name, shape = s_name)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 3.5, stroke = 1.8, alpha = 0.65) +
  #facet_wrap(~ ADM1_EN, scales = "free", nrow=2) +
  labs(title = "Vodafone estimates vs. COD‑PS 2022",
       subtitle = "Reference date: 1 Jan 2022",
       x = "COD-PS (in thousands)" ,
       y = "Vodafone‑estimated population (in thousands)",
       colour = "Age group",
      shape = "Sex") +
  #scale_color_viridis_d() +
  scale_shape_manual(values = c("F" = 3, "M" = 2)) +
  theme_minimal(base_size = 20)



comparison23 <- stocks_hromada_agesex %>% 
    select(-raion, -oblast) %>% 
    left_join(geo) %>% 
    filter(t == as.Date("2023-07-01"),     
           ADM2_PCODE %in% raions_complete$ADM2_PCODE,
          a_name!="18_24") %>% 
    group_by(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, a_name, s_name) %>% 
    summarise(pop_estimated = sum(pop_estimated), .groups = "drop") %>%
    left_join(codps23) %>%
    filter(a_name != "18_24" & a_name != "18-24") 


base_plot <- ggplot(comparison23,
    aes(x = pop1/1000,
        y = pop_estimated/1000,
        colour = a_name,
        shape  = s_name)) +
geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
geom_point(size = 3.5, stroke = 1.8, alpha = 0.65) +
labs(title    = "Vodafone estimates vs. COD‑PS 2023",
subtitle = "Reference date: 1 Jul 2023",
x        = "COD‑PS (in thousands)",
y        = "Vodafone estimate (in thousands)",
colour   = "Age group",
shape    = "Sex") +
scale_shape_manual(values = c(F = 3, M = 2)) +
scale_color_viridis_d() +
theme_minimal(base_size = 20)

n_col <- 3    
n_row <- 2

n_pages <- ggforce::n_pages(
base_plot + facet_wrap_paginate(~ ADM1_EN, ncol = n_col, nrow = n_row, page = 1)
)

paged_plot <- base_plot +
facet_wrap_paginate(~ ADM1_EN, ncol = n_col, nrow = n_row, page = 4)
print(paged_plot)   




comparison24 <- stocks_hromada_agesex %>% 
    select(-raion, -oblast) %>% 
    left_join(geo) %>% 
    filter(t == as.Date("2024-09-01"),     
           ADM2_PCODE %in% raions_complete$ADM2_PCODE,
          a_name!="18_24") %>% 
    group_by(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, a_name, s_name) %>% 
    summarise(pop_estimated = sum(pop_estimated), .groups = "drop") %>%
    left_join(codps24) %>%
    filter(a_name != "18_24" & a_name != "18-24") 


base_plot <- ggplot(comparison24,
    aes(x = pop2/1000,
        y = pop_estimated/1000,
        colour = a_name,
        shape  = s_name)) +
geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
geom_point(size = 3.5, stroke = 1.8, alpha = 0.65) +
labs(title    = "Vodafone estimates vs. COD‑PS 2024",
subtitle = "Reference date: 1 Sep 2024",
x        = "COD‑PS (in thousands)",
y        = "Vodafone estimate (in thousands)",
colour   = "Age group",
shape    = "Sex") +
scale_shape_manual(values = c(F = 3, M = 2)) +
scale_color_viridis_d() +
theme_minimal(base_size = 20)
base_plot

n_col <- 3    
n_row <- 2

n_pages <- ggforce::n_pages(
base_plot + facet_wrap_paginate(~ ADM1_EN, ncol = n_col, nrow = n_row, page = 1)
)

paged_plot <- base_plot +
facet_wrap_paginate(~ ADM1_EN, ncol = n_col, nrow = n_row, page = 4)
print(paged_plot)   


#######################################
# SMD estimates vs Vodafone estimates
#######################################
#2023_WHO_Ukraine_Population\deliverables\20240305 Deterministic Estimates
parent_dir <- file.path(out_dir,
  "20240305 Deterministic Estimates/extract",
  "Ukraine_population_estimates_2203",
  "Ukraine_population_estimates_2023",
  "oblast_daily_population/model")

all_subdirs   <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
date_pattern  <- "^\\d{4}-\\d{2}-\\d{2}$"

target_dates  <- seq(as.Date("2021-12-01"), as.Date("2025-02-01"), by = "day")
target_dates  <- target_dates[weekdays(target_dates) == "Sunday"]

date_subdirs  <- all_subdirs[str_detect(basename(all_subdirs), date_pattern)]
filtered_subdirs <- date_subdirs[basename(date_subdirs) %in% target_dates]

target_filename <- "population_current.csv"

population_data_list <- vector("list", length(filtered_subdirs))
names(population_data_list) <- basename(filtered_subdirs)

for (s in seq_along(filtered_subdirs)) {
fp <- file.path(filtered_subdirs[s], target_filename)
if (file.exists(fp)) {
population_data_list[[s]] <- read_csv(fp,
                    show_col_types = FALSE) %>%
mutate(date = as.Date(basename(filtered_subdirs[s])))
}
}

combined_population_data_2023 <- bind_rows(population_data_list) %>%
rename(t_date = date) %>%                  
transmute(
t_date,
ADM1_PCODE,
F_25_34 = f_25 + f_30,    M_25_34 = m_25 + m_30,
F_35_44 = f_35 + f_40,    M_35_44 = m_35 + m_40,
F_45_54 = f_45 + f_50,    M_45_54 = m_45 + m_50,
F_55_64 = f_55 + f_60,    M_55_64 = m_55 + m_60
) %>%
filter(ADM1_PCODE %in% oblasts_complete$ADM1_PCODE) %>%
pivot_longer(cols = -c(t_date, ADM1_PCODE),
names_to  = "demog",
values_to = "pop1") %>%                  
separate(demog, into = c("s_name", "age_min", "age_max"), sep = "_") %>%
mutate(
a_name = paste0(age_min, "-", age_max),
ADM1_PCODE = str_remove_all(ADM1_PCODE, "\\s+")
) %>%
select(t_date, ADM1_PCODE, s_name, a_name, pop1)





