env <- new.env()
source(here::here('.env'), local=env)
dir.create(file.path(env$out_dir,...))

#dir = c("~/ndph/DemSci/projects/2023_WHO_Ukraine_Population/tmp/")
#dir.create(dir, showWarnings = F, recursive = T)


# outward border crossings
out_refugee_file <- file.path(dir, "SRF_Data/crossing_borders/refugees_out_daily.csv")

download.file(url = 'https://data.unhcr.org/population/get/timeseries?export=csv&widget_id=324084&sv_id=54&population_group=5460&frequency=day&fromDate=1900-01-01',
              destfile = out_refugee_file)

dat_refugees_out <- read.csv(out_refugee_file, 
                             stringsAsFactors = F,
                             skip=1,
                             skipNul=T,
                             sep=';')
names(dat_refugees_out) <- c('date', 'ref_out')



# inward border crossings
in_refugee_file <- file.path(dir, "SRF_Data/crossing_borders/refugees_in_daily.csv")

download.file(url = 'https://data.unhcr.org/population/get/timeseries?export=csv&widget_id=324085&sv_id=54&population_group=5472&frequency=day&fromDate=1900-01-01',
              destfile = in_refugee_file)

dat_refugees_in <- read.csv(in_refugee_file, 
                            stringsAsFactors = F,
                            skip=1,
                            skipNul=T,
                            sep=';')
names(dat_refugees_in) <- c('date', 'ref_in')

# net border crossings
dat_refugees <- dat_refugees_out
dat_refugees$individuals <- dat_refugees_out$individuals - dat_refugees_in$individuals


#install.packages("imputeTS")
library("imputeTS")

# daily time step
refugees_totd <- expand.grid(
  date = seq(as.Date("2022-02-27"), 
             as.Date("2024-05-14"), by = "day") %>% as.character()) %>%
  left_join(dat_refugees_in, by=c("date")) %>%
  left_join(dat_refugees_out, by=c("date")) %>%
  mutate(ref_inc = imputeTS::na_interpolation(ref_in, option = "linear"),
         ref_outc = imputeTS::na_interpolation(ref_out, option = "linear"),
         net = ref_in-ref_out,
         netc = ref_inc-ref_outc,
         date = as.Date(date)) 

# weekly time step
refugees_totw <- expand.grid(
  date = seq(as.Date("2022-02-27"), 
             as.Date("2024-05-14"), by = "day") %>%
    .[weekdays(.) == "Sunday"] %>% as.character()) %>%
  left_join(dat_refugees_in, by = c("date")) %>%
  left_join(dat_refugees_out, by = c("date")) %>%
  mutate(ref_inc = imputeTS::na_interpolation(ref_in, option = "linear"),
         ref_outc = imputeTS::na_interpolation(ref_out, option = "linear"),
         net = ref_in-ref_out,
         netc = ref_inc-ref_outc,
         date = as.Date(date)) 

