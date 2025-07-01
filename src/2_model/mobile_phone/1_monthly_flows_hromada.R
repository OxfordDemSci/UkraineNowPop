rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(ggplot2)

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
repo_dir <- env$repo_dir
data_dir <- file.path(env$repo_dir, "data")
src_dir <- file.path(repo_dir, "src", "2_model/mobile_phone")

in_dir <- env$in_dir
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = F, recursive = T)

# CODPS data
codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
if (file.exists(file.path(in_dir, "cod-ps_2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv"))) {
  codps_N1 <- read.csv(file.path(in_dir, "cod-ps_2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv"))
} else if(file.exists(file.path(in_dir, "cod-ps_2023", "UKR_ADM2_POP_2023_sim.csv"))){
  codps_N1 <- read.csv(file.path(in_dir, "cod-ps_2023", "UKR_ADM2_POP_2023_sim.csv"))
} else{
  codps_N1 <- read.csv(file.path(in_dir, "cod-ps", "ukr_admpop_adm1_2022.csv"))
}

# Vodafone data

# Monthly flows
# flows_m <- read.csv("Monthly Flows.csv",sep=';')
flows_m <- read.csv(file.path(data_dir, "Monthly Flows.csv"),sep=';')

# Monthly stocks
# stocks <- read.csv("Stocks.csv",sep=';')
stocks <- read.csv(file.path(data_dir, "Stocks.csv"),sep=';')

# hromada-level baseline population
# pop <- read.csv("full_dataset.csv")
pop <- read.csv(file.path(data_dir, "full_dataset.csv"))

# get oblast-level population pyramid 
# for initial age-sex-hromada specific penetration rates
# first, data cleaning

pop$oblast_name_en[pop$oblast_name == "Донецька"] <- 'Donetsk'
pop$oblast_name_en[pop$oblast_name == "Луганська"] <- 'Luhansk'

pop$ADM1_EN <- NA
pop$ADM1_EN[pop$oblast_name_en == "Vinnytsia"] <- 'Vinnytska'
pop$ADM1_EN[pop$oblast_name_en == "Vonyn"] <- 'Volynska'
pop$ADM1_EN[pop$oblast_name_en == "Driproptrovska"] <- 'Dnipropetrovska'
pop$ADM1_EN[pop$oblast_name_en == "Donetks"] <- 'Donetska'
pop$ADM1_EN[pop$oblast_name_en == "Donetsk"] <- 'Donetska'
pop$ADM1_EN[pop$oblast_name_en == "Luhansk"] <- 'Luhanska'
pop$ADM1_EN[pop$oblast_name_en == "Zhytomir"] <- 'Zhytomyrska'
pop$ADM1_EN[pop$oblast_name_en == "Zakarpatska"] <- 'Zakarpatska'
pop$ADM1_EN[pop$oblast_name_en == "Zaporizka"] <- 'Zaporizka'
pop$ADM1_EN[pop$oblast_name_en == "Ivano-Frankivsk"] <- 'Ivano-Frankivska'
pop$ADM1_EN[pop$oblast_name_en == "Kyiv-oblast"] <- 'Kyivska'
pop$ADM1_EN[pop$oblast_name_en == "Kirovograd"] <- 'Kirovohradska'
pop$ADM1_EN[pop$oblast_name_en == "Lviv"] <- 'Lvivska'
pop$ADM1_EN[pop$oblast_name_en == "Mykolayiv"] <- 'Mykolaivska'
pop$ADM1_EN[pop$oblast_name_en == "Odesa"] <- 'Odeska'
pop$ADM1_EN[pop$oblast_name_en == "Poltava"] <- 'Poltavska'
pop$ADM1_EN[pop$oblast_name_en == "Rivenska"] <- 'Rivnenska'
pop$ADM1_EN[pop$oblast_name_en == "Sumska"] <- 'Sumska'
pop$ADM1_EN[pop$oblast_name_en == "Ternopilska"] <- 'Ternopilska'
pop$ADM1_EN[pop$oblast_name_en == "Kharkiv"] <- 'Kharkivska'
pop$ADM1_EN[pop$oblast_name_en == "Kherson"] <- 'Khersonska'
pop$ADM1_EN[pop$oblast_name_en == "Khmelnitsk"] <- 'Khmelnytska'
pop$ADM1_EN[pop$oblast_name_en == "Cherkassy"] <- 'Cherkaska'
pop$ADM1_EN[pop$oblast_name_en == "Cherniveska"] <- 'Chernivetska'
pop$ADM1_EN[pop$oblast_name_en == "Chernigiv"] <- 'Chernihivska'

pop <- left_join(pop, codps, by = 'ADM1_EN')

ages.vodafone <- c("0-17","18-24","25-34","35-44","45-54","55-64","65+")

pop$F_00_17 <- (pop$F_00_04 + pop$F_05_09 + pop$F_10_14 + pop$F_15_19*(3/5))/pop$T_TL
pop$F_18_24 <- (pop$F_15_19*(2/5) + pop$F_20_24)/pop$T_TL
pop$F_25_34 <- (pop$F_25_29 + pop$F_30_34)/pop$T_TL
pop$F_35_44 <- (pop$F_35_39 + pop$F_40_44)/pop$T_TL
pop$F_45_54 <- (pop$F_45_49 + pop$F_50_54)/pop$T_TL
pop$F_55_64 <- (pop$F_55_59 + pop$F_60_64)/pop$T_TL
pop$F_65Plus <- (pop$F_65_69 + pop$F_70_74 + pop$F_75_79 + pop$F_80Plus)/pop$T_TL

pop$M_00_17 <- (pop$M_00_04 + pop$M_05_09 + pop$M_10_14 + pop$M_15_19*(3/5))/pop$T_TL
pop$M_18_24 <- (pop$M_15_19*(2/5) + pop$M_20_24)/pop$T_TL
pop$M_25_34 <- (pop$M_25_29 + pop$M_30_34)/pop$T_TL
pop$M_35_44 <- (pop$M_35_39 + pop$M_40_44)/pop$T_TL
pop$M_45_54 <- (pop$M_45_49 + pop$M_50_54)/pop$T_TL
pop$M_55_64 <- (pop$M_55_59 + pop$M_60_64)/pop$T_TL
pop$M_65Plus <- (pop$M_65_69 + pop$M_70_74 + pop$M_75_79 + pop$M_80Plus)/pop$T_TL

pop.kyiv <- codps %>% filter(ADM1_EN == 'Kyiv')

pop.kyiv$F_00_17 <- (pop.kyiv$F_00_04 + pop.kyiv$F_05_09 + pop.kyiv$F_10_14 + pop.kyiv$F_15_19*(3/5))/pop.kyiv$T_TL
pop.kyiv$F_18_24 <- (pop.kyiv$F_15_19*(2/5) + pop.kyiv$F_20_24)/pop.kyiv$T_TL
pop.kyiv$F_25_34 <- (pop.kyiv$F_25_29 + pop.kyiv$F_30_34)/pop.kyiv$T_TL
pop.kyiv$F_35_44 <- (pop.kyiv$F_35_39 + pop.kyiv$F_40_44)/pop.kyiv$T_TL
pop.kyiv$F_45_54 <- (pop.kyiv$F_45_49 + pop.kyiv$F_50_54)/pop.kyiv$T_TL
pop.kyiv$F_55_64 <- (pop.kyiv$F_55_59 + pop.kyiv$F_60_64)/pop.kyiv$T_TL
pop.kyiv$F_65Plus <- (pop.kyiv$F_65_69 + pop.kyiv$F_70_74 + pop.kyiv$F_75_79 + pop.kyiv$F_80Plus)/pop.kyiv$T_TL

pop.kyiv$M_00_17 <- (pop.kyiv$M_00_04 + pop.kyiv$M_05_09 + pop.kyiv$M_10_14 + pop.kyiv$M_15_19*(3/5))/pop.kyiv$T_TL
pop.kyiv$M_18_24 <- (pop.kyiv$M_15_19*(2/5) + pop.kyiv$M_20_24)/pop.kyiv$T_TL
pop.kyiv$M_25_34 <- (pop.kyiv$M_25_29 + pop.kyiv$M_30_34)/pop.kyiv$T_TL
pop.kyiv$M_35_44 <- (pop.kyiv$M_35_39 + pop.kyiv$M_40_44)/pop.kyiv$T_TL
pop.kyiv$M_45_54 <- (pop.kyiv$M_45_49 + pop.kyiv$M_50_54)/pop.kyiv$T_TL
pop.kyiv$M_55_64 <- (pop.kyiv$M_55_59 + pop.kyiv$M_60_64)/pop.kyiv$T_TL
pop.kyiv$M_65Plus <- (pop.kyiv$M_65_69 + pop.kyiv$M_70_74 + pop.kyiv$M_75_79 + pop.kyiv$M_80Plus)/pop.kyiv$T_TL
pop.kyiv <- pop.kyiv %>%
  select(
    hromada_code = name,
    F_00_17,F_18_24,F_25_34,F_35_44,F_45_54,F_55_64,F_65Plus,
    M_00_17,M_18_24,M_25_34,M_35_44,M_45_54,M_55_64,M_65Plus
  )

pop.pyramid <- pop %>%
  select(
    hromada_code,
    F_00_17,F_18_24,F_25_34,F_35_44,F_45_54,F_55_64,F_65Plus,
    M_00_17,M_18_24,M_25_34,M_35_44,M_45_54,M_55_64,M_65Plus
  ) 

pop.pyramid <- rbind(pop.pyramid,pop.kyiv)

pop.pyramid <- pop.pyramid %>%
  pivot_longer(
    # !hromada,
    cols = starts_with("F_"),
    names_to = "f_age",
    names_prefix = "F_",
    values_to = "f_population"#,
    # values_drop_na = TRUE
  ) %>%
  pivot_longer(
    cols = starts_with("M_"),
    names_to = "m_age",
    names_prefix = "M_",
    values_to = "m_population"
  )

pop.pyramid.f <- pop.pyramid %>%
  select(
    hromada.source = hromada_code,
    age = f_age,
    pop.prop = f_population
  ) %>%
  distinct()
pop.pyramid.f$sex <- 'female'

pop.pyramid.m <- pop.pyramid %>%
  select(
    hromada.source = hromada_code,
    age = m_age,
    pop.prop = m_population
  ) %>%
  distinct()
pop.pyramid.m$sex <- 'male'

pop.pyramid.final <- rbind(pop.pyramid.f,pop.pyramid.m)

pop.pyramid.final$age[pop.pyramid.final$age == '00_17'] <- '0-17'
pop.pyramid.final$age[pop.pyramid.final$age == '18_24'] <- '18-24'
pop.pyramid.final$age[pop.pyramid.final$age == '25_34'] <- '25-34'
pop.pyramid.final$age[pop.pyramid.final$age == '35_44'] <- '35-44'
pop.pyramid.final$age[pop.pyramid.final$age == '45_54'] <- '45-54'
pop.pyramid.final$age[pop.pyramid.final$age == '55_64'] <- '55-64'
pop.pyramid.final$age[pop.pyramid.final$age == '65Plus'] <- '65+'

pop.pyramid.final <- left_join(
  pop.pyramid.final,
  pop %>% select(
    hromada.source = hromada_code,
    hromada.pop = total_popultaion_2022
  ),
  by = 'hromada.source'
)
pop.pyramid.final$hromada.pop[pop.pyramid.final$hromada.source == 'Kyiv'] <- codps$T_TL[codps$ADM1_EN == 'Kyiv']

pop.pyramid.final$population <- pop.pyramid.final$pop.prop * pop.pyramid.final$hromada.pop

sum(pop.pyramid.final$population)

rm(pop.kyiv,pop.pyramid,pop.pyramid.f,pop.pyramid.m,pop.pyramid.final)
rm(codps,codps_N1)

# join flows, stocks, and baseline pop data
flows_m$time.dest <- dmy(flows_m$month)
flows_m$time.source <- flows_m$time.dest - months(1)
stocks$time.dest <- dmy(stocks$month)

flows_m <- rename(
  flows_m, 
  flow = subscribers, 
  hromada.dest = Current.home.hromada,
  hromada.source = Home.hromada.last.month
)
stocks <- rename(stocks, stock.dest = subscribers, hromada.dest = Hromada)

# drop obsolete month variable
flows_m <- flows_m %>% select(-month)

# attach oblasts
flows_m <- left_join(
  flows_m,
  pop %>% select(
    hromada.source = hromada_code,
    oblast.source = ADM1_EN
  )
)

flows_m <- left_join(
  flows_m,
  pop %>% select(
    hromada.dest = hromada_code,
    oblast.dest = ADM1_EN
  )
)

flows_m$oblast.source[flows_m$hromada.source == 'Kyiv'] <- 'Kyiv'
flows_m$oblast.dest[flows_m$hromada.dest == 'Kyiv'] <- 'Kyiv'
flows_m$oblast.source[flows_m$hromada.source == 'abroad'] <- 'abroad'
flows_m$oblast.dest[flows_m$hromada.dest == 'abroad'] <- 'abroad'
flows_m$oblast.source[flows_m$hromada.source == ''] <- ''

stocks <- left_join(
  stocks,
  pop %>% select(
    hromada.dest = hromada_code,
    oblast.dest = ADM1_EN
  )
)

stocks$oblast.dest[stocks$hromada.dest == 'Kyiv'] <- 'Kyiv'
stocks$oblast.dest[stocks$hromada.dest == 'abroad'] <- 'abroad'

# aggregate over age-sex groups to hromada level
flows_m <- flows_m %>%
  group_by(hromada.dest,hromada.source,time.dest) %>%
  mutate(flow = sum(flow)) %>%
  select(-age,-sex) %>%
  distinct()

stocks <- stocks %>%
  group_by(hromada.dest,time.dest) %>%
  mutate(stock.dest = sum(stock.dest)) %>%
  select(-age,-sex,-month) %>%
  distinct()

# estimate stocks based on flows
flows_m <- flows_m %>%
  group_by(hromada.dest,time.dest) %>%
  mutate(stock.dest.flow = sum(flow))

flows_m <- left_join(flows_m,stocks)

flows_m <- left_join(
  flows_m,
  stocks %>%
    rename(
      hromada.source = hromada.dest,
      time.source = time.dest,
      stock.source = stock.dest
    ) %>%
    select(hromada.source,time.source,stock.source),
  by = c('hromada.source','time.source')
)

# compare stocks to flows data (they match)
cor(flows_m$stock.dest,flows_m$stock.dest.flow)

# drop obsolete flow-based stock estimate and rename
flows_m <- flows_m %>%
  select(-stock.dest.flow) %>%
  rename(
    stock.hromada.dest = stock.dest,
    stock.hromada.source = stock.source
  )

# attach baseline hromada population data
flows_m <- left_join(
  flows_m,
  pop %>% select(
    hromada.source = hromada_code,
    hromada.source.baseline.pop = total_popultaion_2022
  )
)

flows_m <- left_join(
  flows_m,
  pop %>% select(
    hromada.dest = hromada_code,
    hromada.dest.baseline.pop = total_popultaion_2022
  )
)

stocks <- left_join(
  stocks,
  pop %>% select(
    hromada.dest = hromada_code,
    hromada.dest.baseline.pop = total_popultaion_2022
  )
)

### baseline population for Kyiv: 
### 2,952,301 (https://en.wikipedia.org/wiki/Kyiv) 
### or 1,795,079 (https://en.wikipedia.org/wiki/Kyiv_Oblast)
flows_m$hromada.dest.baseline.pop[flows_m$hromada.dest == 'Kyiv'] <- 2952301
flows_m$hromada.source.baseline.pop[flows_m$hromada.source == 'Kyiv'] <- 2952301
stocks$hromada.dest.baseline.pop[stocks$hromada.dest == 'Kyiv'] <- 2952301

# calculate oblast stocks and populations
stocks <- stocks %>% 
  group_by(oblast.dest,time.dest) %>%
  rename(stock.hromada.dest = stock.dest) %>%
  mutate(stock.oblast.dest = sum(stock.hromada.dest))

stocks <- left_join(
  stocks,
  pop %>% 
    group_by(ADM1_EN) %>%
    mutate(oblast.dest.baseline.pop = sum(total_popultaion_2022)) %>%
    select(oblast.dest = ADM1_EN,oblast.dest.baseline.pop) %>%
    distinct(),
  by = c('oblast.dest')
)

stocks$oblast.dest.baseline.pop[stocks$oblast.dest == 'Kyiv'] <- 2952301

flows_m <- left_join(
  flows_m,
  stocks %>% select(
    time.source = time.dest,
    hromada.source = hromada.dest,
    oblast.source = oblast.dest,
    stock.oblast.source = stock.oblast.dest,
    oblast.source.baseline.pop = oblast.dest.baseline.pop
  ) %>% distinct(),
  by = c('time.source','hromada.source','oblast.source')
)

flows_m <- left_join(
  flows_m,
  stocks %>% select(
    time.dest,
    hromada.dest,
    oblast.dest,
    stock.oblast.dest,
    oblast.dest.baseline.pop
  ) %>% distinct(),
  by = c('time.dest','hromada.dest','oblast.dest')
)

# calculate initial penetration rate
# for 'unknown' source, use penetration rate of destination
flows_m <- flows_m %>%
  mutate(
    hromada.source.alt = 
      ifelse(
        hromada.source == '',
        hromada.dest,
        hromada.source
      ),
    oblast.source.alt = 
      ifelse(
        oblast.source == '',
        oblast.dest,
        oblast.source
      )
  )

stocks.baseline <- stocks %>%
  filter(time.dest == min(stocks$time.dest)) %>%
  mutate(
    pen.source.hromada = stock.hromada.dest/hromada.dest.baseline.pop,
    pen.source.oblast = stock.oblast.dest/oblast.dest.baseline.pop
  )

flows_m <- left_join(
  flows_m,
  stocks.baseline %>%
    select(
      hromada.source.alt = hromada.dest,
      time.source = time.dest,
      oblast.source.alt = oblast.dest,
      pen.source.hromada,
      pen.source.oblast
    ),
  by = c('hromada.source.alt','time.source','oblast.source.alt')
)

# for 'abroad' source, use national penetration rate
domestic.stock <- 
  sum(stocks.baseline$stock.hromada.dest[
    stocks.baseline$hromada.dest != 'abroad'])
domestic.pop <- sum(stocks.baseline$hromada.dest.baseline.pop[
  stocks.baseline$hromada.dest != 'abroad']) 
# equals total.pop <- 31559249 # total baseline domestic pop

rm(stocks.baseline)

flows_m$pen.source.hromada[
  (flows_m$hromada.source.alt=='abroad') &
    (flows_m$time.source == min(flows_m$time.source))    
] <-
  domestic.stock / domestic.pop
flows_m$pen.source.oblast[
  (flows_m$oblast.source.alt=='abroad') &
    (flows_m$time.source == min(flows_m$time.source))
] <-
  domestic.stock / domestic.pop

flows_m <- flows_m %>%
  select(
    hromada.dest,
    hromada.source,
    hromada.source.alt,
    flow,
    time.dest,
    oblast.dest,    
    oblast.source,
    oblast.source.alt,
    stock.hromada.dest,
    stock.oblast.dest,
    pen.source.hromada,
    pen.source.oblast
  )

# calculate pop-level flow estimate
flows_m$flow.pop <- flows_m$flow / flows_m$pen.source.hromada

# time periods in study
months.obs.stock <- unique(stocks$time.dest)
months.obs <- unique(flows_m$time.dest)
n.months <- length(months.obs)

# estimate population at destination as sum of pop-level flows
pop.est.tmp <-
  flows_m %>%
  group_by(hromada.dest) %>%
  filter(time.dest == months.obs[1]) %>%
  mutate(pop.hromada.dest = sum(flow.pop)) %>%
  select(hromada.dest,time.dest,pop.hromada.dest) %>%
  distinct()

flows_m <-
  flows_m  %>%
  left_join(pop.est.tmp, by = c('hromada.dest','time.dest')) 

rm(pop.est.tmp)

pop.est.tmp <-
  flows_m %>%
  group_by(oblast.dest) %>%
  filter(time.dest == months.obs[1]) %>%
  mutate(pop.oblast.dest = sum(flow.pop)) %>%
  select(oblast.dest,time.dest,pop.oblast.dest) %>%
  distinct()

flows_m <-
  flows_m  %>%
  left_join(pop.est.tmp, by = c('oblast.dest','time.dest'))

rm(pop.est.tmp)

# update penetration rate
flows_m$pen.dest.hromada <- flows_m$stock.hromada.dest / flows_m$pop.hromada.dest
flows_m$pen.dest.oblast <- flows_m$stock.oblast.dest / flows_m$pop.oblast.dest

flows_m.final <- flows_m %>% filter(time.dest == months.obs[1])

total.pop <- sum(flows_m.final$flow.pop)
flows_m.final$total.pop <- total.pop

domestic.pop <- sum(
  flows_m.final$flow.pop[flows_m.final$hromada.dest != 'abroad']
)
flows_m.final$domestic.pop <- domestic.pop

abroad.pop <- sum(
  flows_m.final$flow.pop[flows_m.final$hromada.dest == 'abroad']
)
flows_m.final$abroad.pop <- abroad.pop

for(t in 2:n.months){
  print(t/n.months)
  
  # carry forward hromada and oblast penetration rates from last time period
  pen.rate.tmp <-
    flows_m.final %>%
    filter(time.dest == months.obs[t-1]) %>% 
    mutate(
      time.dest = months.obs[t],
    ) %>%
    select(
      hromada.source.alt = hromada.dest,
      time.dest,
      pen.source.hromada = pen.dest.hromada) %>%
    distinct()
  
  flows_m.tmp <- 
    flows_m  %>% 
    filter(time.dest == months.obs[t]) 
  
  flows_m.tmp <- flows_m.tmp %>%
    left_join(
      pen.rate.tmp, 
      by = c('hromada.source.alt','time.dest')) %>%
    mutate(
      pen.source.hromada = coalesce(pen.source.hromada.x,pen.source.hromada.y),
    ) %>%
    select(-pen.source.hromada.x, -pen.source.hromada.y) 
  
  pen.rate.tmp <-
    flows_m.final %>%
    ungroup() %>%
    filter(time.dest == months.obs[t-1]) %>% 
    mutate(
      time.dest = months.obs[t],
    ) %>%
    select(
      oblast.source.alt = oblast.dest,
      time.dest,
      pen.source.oblast = pen.dest.oblast) %>%
    distinct()
  
  flows_m.tmp <- flows_m.tmp %>%
    left_join(
      pen.rate.tmp,
      by = c('oblast.source.alt','time.dest')) %>%
    mutate(
      pen.source.oblast = coalesce(pen.source.oblast.x,pen.source.oblast.y)
    ) %>%
    select(-pen.source.oblast.x, -pen.source.oblast.y)    
  
  rm(pen.rate.tmp)
  
  # impute any missing hromada penetration rates by oblast penetration rate
  flows_m.tmp <- flows_m.tmp %>%
    mutate(
      pen.source.hromada = ifelse(
        is.na(pen.source.hromada),
        pen.source.oblast,
        pen.source.hromada
      )
    )
  
  # estimate pop-level flow
  flows_m.tmp$flow.pop <- flows_m.tmp$flow / flows_m.tmp$pen.source.hromada
  
  # recalibrate to total population
  flows_m.tmp$flow.pop <- flows_m.tmp$flow.pop * total.pop / sum(flows_m.tmp$flow.pop)
  
  # estimate population at destination as sum of pop-level flows
  pop.est.tmp <-
    flows_m.tmp %>%
    group_by(hromada.dest) %>%
    mutate(pop.hromada.dest = sum(flow.pop)) %>%
    select(hromada.dest,pop.hromada.dest) %>%
    distinct()
  
  flows_m.tmp <-
    flows_m.tmp  %>%
    left_join(pop.est.tmp, by = c('hromada.dest')) %>%
    mutate(pop.hromada.dest = coalesce(pop.hromada.dest.x,pop.hromada.dest.y)) %>%
    select(-pop.hromada.dest.x, -pop.hromada.dest.y)  
  
  rm(pop.est.tmp)
  
  pop.est.tmp <-
    flows_m.tmp %>%
    group_by(oblast.dest) %>%
    mutate(pop.oblast.dest = sum(flow.pop)) %>%
    select(oblast.dest,pop.oblast.dest) %>%
    distinct()
  
  flows_m.tmp <-
    flows_m.tmp  %>%
    left_join(pop.est.tmp, by = c('oblast.dest')) %>%
    mutate(pop.oblast.dest = coalesce(pop.oblast.dest.x,pop.oblast.dest.y)) %>%
    select(-pop.oblast.dest.x, -pop.oblast.dest.y)  
  
  rm(pop.est.tmp)  
  
  # estimate penetration rate at destination
  flows_m.tmp$pen.dest.hromada <- flows_m.tmp$stock.hromada.dest / flows_m.tmp$pop.hromada.dest
  flows_m.tmp$pen.dest.oblast <- flows_m.tmp$stock.oblast.dest / flows_m.tmp$pop.oblast.dest
  
  total.pop <- sum(flows_m.tmp$flow.pop)
  flows_m.tmp$total.pop <- total.pop
  
  domestic.pop <- sum(
    flows_m.tmp$flow.pop[flows_m.tmp$hromada.dest != 'abroad']
  )
  flows_m.tmp$domestic.pop <- domestic.pop
  
  abroad.pop <- sum(
    flows_m.tmp$flow.pop[flows_m.tmp$hromada.dest == 'abroad']
  )
  flows_m.tmp$abroad.pop <- abroad.pop
  
  print(total.pop)
  
  flows_m.final <- rbind(flows_m.final, flows_m.tmp)
  
  rm(flows_m.tmp)
}

# save(flows_m.final, file = "flows_m_final_20250701.RData")
# saveRDS(flows_m.final, file = 'flows_m_final_20250701.rds')
# write.csv(flows_m.final, "flows_m_final_20250701.csv")

# load("flows_m_final.RData")

# plot population estimates over time
library(ggplot2)

flows_m.final %>%
  ggplot(mapping=aes(x=time.dest,y=total.pop)) + 
  geom_line() +
  ggtitle('total pop')

flows_m.final %>%
  ggplot(mapping=aes(x=time.dest,y=domestic.pop)) + 
  geom_line() +
  ggtitle('domestic pop')

flows_m.final %>%
  ggplot(mapping=aes(x=time.dest,y=abroad.pop)) + 
  geom_line() +
  ggtitle('abroad pop')

flows_m.final %>%
  ggplot(mapping=aes(time.dest)) + 
  geom_line(aes(y = total.pop, colour = "total")) + 
  geom_line(aes(y = domestic.pop, colour = "domestic")) +  
  geom_line(aes(y = abroad.pop, colour = "abroad")) +    
  ggtitle('Ukraine population')

# oblast population and penetration rate over time
flows_m.final %>%
  ggplot(mapping=aes(x=time.dest,y=pop.oblast.dest,colour=oblast.dest)) + 
  geom_line() +
  ggtitle('oblast pop')

flows_m.final %>%
  ggplot(mapping=aes(x=time.dest,y=pen.dest.oblast,colour=oblast.dest)) + 
  geom_line() +
  ggtitle('oblast penetration rate')
