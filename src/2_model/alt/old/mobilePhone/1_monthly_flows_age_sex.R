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

unique(pop$oblast_name_en)
unique(codps$ADM1_EN)

# View(codps %>% filter(ADM1_EN == 'Kyiv'))
# View(codps %>% filter(ADM1_EN == 'Kyivska'))

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

# join flows, stocks, and baseline pop data
flows_m$time.dest <- dmy(flows_m$month)
flows_m$time.source <- flows_m$time.dest - months(1)
flows_m$time <- flows_m$time.dest
stocks$time.dest <- dmy(stocks$month)

flows_m <- rename(
  flows_m, 
  flow = subscribers, 
  hromada.dest = Current.home.hromada,
  hromada.source = Home.hromada.last.month
)
stocks <- rename(stocks, stock.dest = subscribers, hromada.dest = Hromada)

### baseline population for Kyiv: 
### 2,952,301 (https://en.wikipedia.org/wiki/Kyiv) 
### or 1,795,079 (https://en.wikipedia.org/wiki/Kyiv_Oblast)
# flows_m$pop.dest.baseline[flows_m$hromada.dest == 'Kyiv'] <- 2952301
# stocks$pop.dest.baseline[stocks$hromada.dest == 'Kyiv'] <- 2952301

# estimate stocks (by age and sex) based on flows
flows_m <- flows_m %>%
  group_by(hromada.dest,age,sex,time.dest) %>%
  mutate(stock.dest.flow = sum(flow))

# calculate total stock (over age and sex) in each hromada
flows_m <- flows_m %>%
  group_by(hromada.dest,time.dest) %>%
  mutate(stock.dest.flow.total = sum(flow))

stocks <- stocks %>%
  group_by(hromada.dest,time.dest) %>%
  mutate(stock.dest.total = sum(stock.dest))

flows_m <- left_join(flows_m,stocks)

# initial penetration rate
months.obs.stock <- unique(stocks$time.dest)
months.obs <- unique(flows_m$time.dest)
n.months <- length(months.obs)

stocks.baseline <- stocks %>%
  filter(time.dest == months.obs.stock[1]) %>%
  rename(hromada.source = hromada.dest) %>%
  mutate(time.dest = time.dest + months(1))

pop.pyramid.final <- left_join(
  pop.pyramid.final,
  stocks.baseline,
  by = c('hromada.source','sex','age')
)

pop.pyramid.final$pen.source <- pop.pyramid.final$stock.dest / pop.pyramid.final$population

flows_m <- left_join(
  flows_m,
  pop.pyramid.final %>%
    select(hromada.source,sex,age,time.dest,pen.source)
)

# compare stocks to flows data
cor(flows_m$stock.dest,flows_m$stock.dest.flow)
cor(flows_m$stock.dest.flow.total,flows_m$stock.dest.total)

# population at destination hromada (by age and sex)
flows_m$pop.dest <- NA

# pop-level flow (by source and destination hromada and age and sex)
flows_m$flow.pop <- NA

# estimate of penetration rate in destination hromada
flows_m$pen.dest <- NA

# estimate of total population (Ukraine + abroad)
flows_m$total.pop <- NA

# calculate pop-level flow estimate 
flows_m$flow.pop <- flows_m$flow / flows_m$pen.source

# calculate domestic pop and stock (by age and sex)
domestic.stock <- stocks %>%
  filter(time.dest == months.obs.stock[1], hromada.dest != 'abroad') %>%
  group_by(age,sex) %>%
  summarise(domestic.stock = sum(stock.dest,na.rm=TRUE)) 

domestic.pop <- flows_m %>%
  filter(time.dest == months.obs[1], hromada.dest != 'abroad') %>%
  group_by(age,sex) %>%
  summarise(domestic.pop = sum(flow.pop,na.rm=TRUE))

pen.source <- left_join(domestic.stock,domestic.pop, by = c('age','sex')) %>% 
  mutate(pen.source = domestic.stock/domestic.pop)

pen.source$pen.na <- TRUE

# impute missing penetration rates based on pop-level mean penetration rate for each age-sex group
flows_m.tmp <- flows_m %>% filter(time.dest == months.obs[1])
flows_m.tmp$pen.na <- is.na(flows_m.tmp$pen.source)

flows_m.tmp <- flows_m.tmp %>%
  left_join(
    pen.source %>% select(age,sex,pen.source,pen.na), 
    by = c('sex','age','pen.na')) %>%
  mutate(pen.source = coalesce(pen.source.x,pen.source.y)) %>%
  select(-pen.source.x, -pen.source.y, -pen.na)  

rm(pen.source)

flows_m.tmp <- flows_m.tmp %>%
  select(hromada.dest,hromada.source,age,sex,time.dest,pen.source)

flows_m <- left_join(
  flows_m,flows_m.tmp, 
  by = c('hromada.dest','hromada.source','age','sex','time.dest')) %>%
  mutate(pen.source = coalesce(pen.source.x,pen.source.y)) %>%
  select(-pen.source.x, -pen.source.y)    

# calculate pop-level flow estimate 
flows_m$flow.pop <- flows_m$flow / flows_m$pen.source

# estimate population at destination hromada (by age and sex) as sum of pop-level flows
pop.est.tmp <-
  flows_m %>%
  group_by(hromada.dest,sex,age) %>%
  filter(time == months.obs[1]) %>%
  mutate(pop.dest = sum(flow.pop)) %>%
  select(hromada.dest,sex,age,time,pop.dest) %>%
  distinct()

flows_m <-
  flows_m  %>%
  left_join(pop.est.tmp, by = c('hromada.dest','sex','age','time')) %>%
  mutate(pop.dest = coalesce(pop.dest.x,pop.dest.y)) %>%
  select(-pop.dest.x, -pop.dest.y)

rm(pop.est.tmp)

# calculate total pop as sum of all pop-level flows
flows_m <- flows_m %>%
  group_by(time.dest) %>%
  mutate(total.pop = sum(flow.pop))

# update penetration rate
flows_m$pen.dest <- flows_m$stock.dest / flows_m$pop.dest

# visualize penetration rate distribution
hist(unique(flows_m$pen.dest))

flows_m.final <- flows_m %>% filter(time.dest == months.obs[1])

for(t in 2:n.months){
  print(t/n.months)
  
  # get source penetration rate from destination in last time period
  pen.rate.tmp <-
    flows_m.final %>%
    group_by(hromada.dest,sex,age) %>%
    filter(time == months.obs[t-1]) %>% 
    mutate(
      time = months.obs[t],
      pen.dest = median(pen.dest)
    ) %>%
    select(hromada.dest,sex,age,time,pen.dest) %>%
    distinct()
  
  pen.rate.tmp <- 
    pen.rate.tmp %>%
    rename(
      hromada.source = hromada.dest,
      pen.source = pen.dest,
    )
  
  # subset to specific time period and merge penetration rates
  flows_m.tmp <- 
    flows_m  %>% 
    filter(time == months.obs[t]) 
  
  flows_m.tmp <- flows_m.tmp %>%
    left_join(pen.rate.tmp, by = c('hromada.source','sex','age','time'), relationship='many-to-one') %>%
    mutate(pen.source = coalesce(pen.source.x,pen.source.y)) %>%
    select(-pen.source.x, -pen.source.y)  
  
  rm(pen.rate.tmp)
  
  # calculate domestic pop and stock (by age and sex) 
  # for imputing missing penetration rates
  domestic.stock <- stocks %>%
    filter(time.dest == months.obs.stock[t], hromada.dest != 'abroad') %>%
    group_by(age,sex,time.dest) %>%
    summarise(domestic.stock = sum(stock.dest)) 
  
  domestic.pop <- flows_m.final %>%
    filter(time.dest == months.obs[t-1], hromada.dest != 'abroad') %>%
    group_by(age,sex,time.dest) %>%
    summarise(domestic.pop = sum(flow.pop))
  
  pen.source <- left_join(domestic.stock,domestic.pop, by = c('age','sex','time.dest')) %>% 
    mutate(pen.source = domestic.stock/domestic.pop)
  
  pen.source$pen.na <- TRUE
  
  # impute missing penetration rates based on pop-level mean penetration rate for each age-sex group
  flows_m.tmp$pen.na <- is.na(flows_m.tmp$pen.source)
  
  flows_m.tmp <- flows_m.tmp %>%
    left_join(
      pen.source %>% select(age,sex,pen.source,pen.na), 
      by = c('sex','age','pen.na')) %>%
    mutate(pen.source = coalesce(pen.source.x,pen.source.y)) %>%
    select(-pen.source.x, -pen.source.y)   
  
  rm(pen.source)
  
  # estimate pop-level flow
  flows_m.tmp$flow.pop <- flows_m.tmp$flow / flows_m.tmp$pen.source
  
  # estimate population at destination as sum of pop-level flows
  flows_m.tmp <-
    flows_m.tmp %>%
    group_by(hromada.dest,sex,age) %>%
    mutate(pop.dest = sum(flow.pop))
  
  # estimate penetration rate at destination
  flows_m.tmp$pen.dest <- flows_m.tmp$stock.dest / flows_m.tmp$pop.dest 
  
  # estimate total population (Ukraine + abroad)
  flows_m.tmp$total.pop <- sum(flows_m.tmp$flow.pop)
  
  # join to final dataset
  flows_m.final <- rbind(flows_m.final, flows_m.tmp %>% select(-pen.na))
  
  rm(flows_m.tmp)
}

# attach oblasts
flows_m.final <- left_join(
  flows_m.final,
  pop %>% select(
    hromada.source = hromada_code,
    oblast.source = ADM1_EN
  )
)

flows_m.final <- left_join(
  flows_m.final,
  pop %>% select(
    hromada.dest = hromada_code,
    oblast.dest = ADM1_EN
  )
)

flows_m.final$oblast.source[flows_m.final$hromada.source == 'Kyiv'] <- 'Kyiv'
flows_m.final$oblast.dest[flows_m.final$hromada.dest == 'Kyiv'] <- 'Kyiv'
flows_m.final$oblast.source[flows_m.final$hromada.source == 'abroad'] <- 'abroad'
flows_m.final$oblast.dest[flows_m.final$hromada.dest == 'abroad'] <- 'abroad'
flows_m.final$oblast.source[flows_m.final$hromada.source == ''] <- ''

# get total stock and pop in each oblast over time
flows_m.final <- flows_m.final %>%
  group_by(oblast.dest,time.dest) %>%
  mutate(
    oblast.dest.pop = sum(flow.pop),
    oblast.dest.stock = sum(flow),
    oblast.dest.pen = oblast.dest.stock/oblast.dest.pop
  )

# save(flows_m.final, file = "flows_m_final.RData")
# saveRDS(flows_m.final, file = 'flows_m_final.rds')
# write.csv(flows_m.final, "flows_m_final.csv")

# load("flows_m_final.RData")

# plot population estimates over time
library(ggplot2)

flows_m.final %>%
  # group_by(sex) %>%
  filter(hromada.dest == 'Kyiv') %>%  
  # filter(hromada.dest == 'UA07060190000036342') %>%
  # filter(hromada == 'Kyiv',sex == 'female') %>%
  ggplot(mapping=aes(x=time,y=pop.dest,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex)) +
  # labs(title = "Kyiv")
  ggtitle('Kyiv population')

flows_m.final %>%
  # group_by(sex) %>%
  filter(hromada.source == 'Kyiv') %>%  
  # filter(hromada.dest == 'UA07060190000036342') %>%
  # filter(hromada == 'Kyiv',sex == 'female') %>%
  ggplot(mapping=aes(x=time,y=pen.source,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex)) +
  # labs(title = "Kyiv")
  ggtitle('Kyiv penetration rate')

flows_m.final %>%
  filter(hromada.source == 'abroad') %>%  
  ggplot(mapping=aes(x=time,y=pen.source,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex)) +
  ggtitle('abroad penetration rate')

flows_m.final %>%
  filter(hromada.source == '') %>%  
  ggplot(mapping=aes(x=time,y=pen.source,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex)) +
  ggtitle('unknown penetration rate')

flows_m.final %>%
  filter(hromada.dest == 'abroad') %>%  
  ggplot(mapping=aes(x=time,y=pop.dest,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex)) +
  ggtitle('abroad population')

flows_m.final %>%
  filter(hromada.source == '') %>% 
  group_by(time,age,sex) %>%
  mutate(inflow = sum(flow)) %>%
  ggplot(mapping=aes(x=time,y=inflow,colour=age)) + 
  geom_line() +
  # facet_wrap(vars(sex,age)) #+
  facet_wrap(vars(sex)) +
  # labs(title = "Kyiv")
  ggtitle('Inflow from "unknown"')

flows_m.final %>%
  filter(hromada.source == 'abroad') %>% 
  group_by(time,age,sex) %>%
  mutate(inflow = sum(flow)) %>%
  ggplot(mapping=aes(x=time,y=inflow,colour=age)) + 
  geom_line() +
  # facet_wrap(vars(sex,age)) #+
  facet_wrap(vars(sex)) +
  # labs(title = "Kyiv")
  ggtitle('Inflow from "abroad"')

flows_m.final %>%
  ggplot(mapping=aes(x=time,y=total.pop)) + 
  geom_line() +
  ggtitle('total pop')


# oblast-specific penetration rates
unique(flows_m.final$oblast.dest)

flows_m.final %>%
  filter(oblast.dest == 'Kyiv') %>%  
  ggplot(mapping=aes(x=time,y=oblast.dest.pen)) + 
  geom_line() +
  ggtitle('Kyiv penetration rate')

flows_m.final %>%
  filter(oblast.dest == 'Kyivska') %>%  
  ggplot(mapping=aes(x=time,y=oblast.dest.pen)) + 
  geom_line() +
  ggtitle('Kyivska penetration rate')

flows_m.final %>%
  filter(oblast.dest == 'Odeska') %>%  
  ggplot(mapping=aes(x=time,y=oblast.dest.pen)) + 
  geom_line() +
  ggtitle('Odeska penetration rate')

flows_m.final %>%
  ggplot(mapping=aes(x=time,y=oblast.dest.pen,colour=oblast.dest)) + 
  geom_line() +
  ggtitle('Oblast penetration rate')

flows_m %>%
  group_by(time,age,sex) %>%
  mutate(num.hromada = length(unique(hromada))) %>%
  ggplot(mapping=aes(x=time,y=num.hromada,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex))

stocks %>%
  group_by(time,age,sex) %>%
  mutate(num.hromada = length(unique(hromada))) %>%
  ggplot(mapping=aes(x=time,y=num.hromada,colour=age)) + 
  geom_line() +
  facet_wrap(vars(sex))

stocks %>%
  group_by(time) %>%
  mutate(num.hromada = length(unique(hromada))) %>%
  ggplot(mapping=aes(x=time,y=num.hromada)) + 
  geom_line()

flows_m %>%
  group_by(time) %>%
  mutate(num.hromada = length(unique(hromada))) %>%
  ggplot(mapping=aes(x=time,y=num.hromada)) + 
  geom_line()
