#install.packages("tidyverse")
library("tidyverse")
#install.packages("haven")
library("haven")
#install.packages("naniar")
library("naniar")

setwd("/home/andrea/ndph/DemSci/projects/2023_WHO_Ukraine_Population/tmp/SRF_Data/IOM_survey/IOM_GPS Package/IOM_GPS_Datasets/Datasets")

###################
# Wave 1
###################
data_w1_2000_Oxf <- read_sav("data_w1_2000_Oxf.sav") %>%
  rename(gender=A1,
         age=A2,
         dest=B1а,  #current oblast*
         dest_m=Reg, #current macroregion*
         dest_area=B2, #area: city, road, etc.
         isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
         change_res=B3а,  #Did you leave your habitual place of residence because of the war?
         orig=B3с,  #Where was your habitual residence before the war started? - Oblast
         orig_m=Reg2,    #macroregion before war
         fut_mov_coun = B7а,   #What country are you planning to move to?
         fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
         childu5=D1_02, #children under 5 years old (yes, no)
         child5_18=D1_03,  #individuals aged 5-18
         elderly60=D1_06  #individuals aged >60
         ) %>%
  dplyr::select(-dt_id, -B8__oth97) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999") %>% as.character(),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA) %>% as.character(),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80") %>% as.character(),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region - UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80") %>% as.character(),
         
         orig_name1 = case_when(
           mover==0 & !is.na(dest_name) ~ as.character(dest_name), 
           mover==1 & !is.na(orig_name) ~ as.character(orig_name))) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  filter(!is.na(orig_name1), !is.na(dest_name), !is.na(age), !is.na(gender) ) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, gender, age, age_group,
                isdest_res_pre_war, childu5, child5_18, elderly60) 

data_w1_2000_Oxf1 <- data_w1_2000_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("1", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-03-09"),
         collection_date = as.Date("2022-03-16"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w1_2000_Oxf2 <- data_w1_2000_Oxf1 %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count)) %>% ungroup() %>%
  mutate(day = as.Date("2022-03-16"))

View(data_w1_2000_Oxf2)
ls(data_w1_2000_Oxf1)
lapply(data_w1_2000_Oxf1, class)


###################
# Wave 2
###################
data_w2_2000_Oxf <- read_sav("data_w2_2000_Oxf.sav") %>%
  rename(#gender=A1,
         age=A2,
         dest=B1а,  #current oblast*
         dest_m=Reg, #current macroregion*
         #dest_area=B2, #area: city, road, etc.
         isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
         change_res=B3а,  #Did you leave your habitual place of residence because of the war?
         orig=B3с,  #Where was your habitual residence before the war started? - Oblast
         orig_m=Reg2,    #macroregion before war
         #fut_mov_coun = B7а,   #What country are you planning to move to?
         #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
         childu1=D1_02, #children 0-1 year old (yes, no)
         childu1_count=A5_1, #count of children 0-1 year old
         childu5=D1_03, #children 1-5 years old
         childu5_count=A5_2, #count of children 1-5 years old
         child5_18=D1_04,  #individuals aged 5-18
         child5_18_count=A5_3, #count of people aged 5-18
         elderly60=D1_07,  #individuals aged >60 
         elderly60_count=A5_4, #count of individuals aged >60
         a18_60m_count=A5_5, #count of males aged 18-60
         a18_60f_count=A5_6 #count of females aged 18-60  (max 15 in all questions about counts)
         ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999") %>% as.character(),
         
        #gender = case_when(
        #   gender==1 ~ "Male", 
        #   gender==2 ~ "Female", 
        #   TRUE ~ NA) %>% as.character(),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80") %>% as.character(),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80") %>% as.character(),
         
         orig_name1 = case_when(
           mover==0 ~ as.character(dest_name), 
           mover==1 ~ as.character(orig_name))) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  filter(!is.na(orig_name1), !is.na(dest_name), !is.na(age)#, !is.na(gender) 
         ) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, age_group,
                isdest_res_pre_war, 
                childu1, childu1_count, childu5, childu5_count, child5_18, child5_18_count, elderly60, elderly60_count, a18_60m_count, a18_60f_count) 

data_w2_2000_Oxf1 <- data_w2_2000_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group#, gender
           ) %>%
  summarise(count0 = n(),
            Female = round(n()*0.51, 0), 
            Male = count0-Female, .groups = 'drop') %>% ungroup() %>%
  pivot_longer(
    cols = c("Female", "Male"),
    names_to = "gender",
    values_to = "count") %>%
  mutate(wave = rep("2", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-03-24"),
         collection_date = as.Date("2022-04-01"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w2_2000_Oxf2 <- data_w2_2000_Oxf1 %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count)) %>% ungroup() %>%
  mutate(day = as.Date("2022-04-01"))

View(data_w2_2000_Oxf2)
View(data_w2_2000_Oxf)
View(data_w2_2000_Oxf1)


###################
# Wave 3
###################
data_w3_2000_Oxf <- read_sav("data_w3_2000_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D1a_02, #count of children 0-1 year old
    childu5=D1_03, #children 1-5 years old
    childu5_count=D1a_03, #count of children 1-5 years old
    child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D1a_04, #count of people aged 5-18
    elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D1a_07 #count of individuals aged >60
    #a18_60m_count=A5_5, #count of males aged 18-60
    #a18_60f_count=A5_6 #count of females aged 18-60  (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999") %>% as.character(),
         
         gender = case_when(
            gender==1 ~ "Male", 
            gender==2 ~ "Female", 
            TRUE ~ NA) %>% as.character(),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80") %>% as.character(),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80") %>% as.character(),
         
         orig_name1 = case_when(
           mover==0 ~ as.character(dest_name), 
           mover==1 ~ as.character(orig_name))) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  filter(!is.na(orig_name1), !is.na(dest_name), !is.na(age), !is.na(gender)) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, gender, 
                age, age_group,
                isdest_res_pre_war, 
                childu1, childu1_count, childu5, childu5_count, child5_18, child5_18_count, elderly60, elderly60_count) 

data_w3_2000_Oxf1 <- data_w3_2000_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("3", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-04-11"),
         collection_date = as.Date("2022-04-17"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w3_2000_Oxf2 <- data_w3_2000_Oxf1 %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count)) %>% ungroup() %>%
  mutate(day = as.Date("2022-04-17"))

View(data_w3_2000_Oxf2)



View(data_w3_2000_Oxf %>% select(c("orig", "dest", "category", "category_upd2", "mover")))
###################
# Wave 4
###################
data_w4_2006_Oxf <- read_sav("data_w4_2006_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D1a_02, #count of children 0-1 year old
    childu5=D1_03, #children 1-5 years old
    childu5_count=D1a_03, #count of children 1-5 years old
    child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D1a_04, #count of people aged 5-18
    elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D1a_07 #count of individuals aged >60
    #a18_60m_count=A5_5, #count of males aged 18-60
    #a18_60f_count=A5_6 #count of females aged 18-60   (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                childu1, childu1_count, childu5, childu5_count, child5_18, child5_18_count, elderly60, elderly60_count) 



data_w4_2006_Oxf1 <- data_w4_2006_Oxf %>%
  group_by(orig_pcode, dest_pcode) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("4", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-04-29"),
         collection_date = as.Date("2022-05-03"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w4_2006_Oxf2 <- data_w4_2006_Oxf1 %>%
  dplyr::select(dest_pcode, count) %>%
  mutate(across(c("count"), ~ na_if(., 99))) %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-05-03"))

View(data_w4_2006_Oxf2)


###################
# Wave 5
###################
data_w5_2001_Oxf <- read_sav("data_w5_2001_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D1a_02, #count of children 0-1 year old
    childu5=D1_03, #children 1-5 years old
    childu5_count=D1a_03, #count of children 1-5 years old
    child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D1a_04, #count of people aged 5-18
    child5_18_counto=D0_3, #count of people under 18 years old
    elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D1a_07, #count of individuals aged >60
    a18plusm_count=D0_1, #count of males aged 18+
    a18plusf_count=D0_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                childu1, childu1_count, childu5, childu5_count, child5_18, child5_18_count, elderly60, elderly60_count,
                a18plusm_count, a18plusf_count) 

data_w5_2001_Oxf1 <- data_w5_2001_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("5", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-05-17"),
         collection_date = as.Date("2022-05-23"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w5_2001_Oxf2 <- data_w5_2001_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-05-23"))

View(data_w5_2001_Oxf2)

###################
# Wave 6
###################
data_w6_2000_Oxf <- read_sav("data_w6_2000_Oxf.sav")%>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D1a_02, #count of children 0-1 year old
    childu5=D1_03, #children 1-5 years old
    childu5_count=D1a_03, #count of children 1-5 years old
    child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D1a_04, #count of people aged 5-18
    child5_18_counto=D0_3, #count of people under 18 years old
    elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D1a_07, #count of individuals aged >60
    a18plusm_count=D0_1, #count of males aged 18+
    a18plusf_count=D0_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                childu1, childu1_count, childu5, childu5_count, child5_18, child5_18_count, elderly60, elderly60_count,
                a18plusm_count, a18plusf_count) 

data_w6_2000_Oxf1 <- data_w6_2000_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("6", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-06-17"),
         collection_date = as.Date("2022-06-23"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)
 

data_w6_2000_Oxf2 <- data_w6_2000_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-06-23"))

View(data_w6_2000_Oxf2)
sum(data_w6_2000_Oxf2$count)

###################
# Wave 7
###################
data_w7_2002_Oxf <- read_sav("data_w7_2002_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D0_2_5, #count of people aged 5-18
    child5_18m_count=D1a_1, #count of males aged 5-18
    child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D0_2_8, #count of individuals aged >60
    a18plusm_count=D0_2_1, #count of males aged 18+
    a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) 

data_w7_2002_Oxf1 <- data_w7_2002_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("7", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-07-17"),
         collection_date = as.Date("2022-07-23"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w7_2002_Oxf2 <- data_w7_2002_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-07-23"))

View(data_w7_2002_Oxf2)

###################
# Wave 8
###################
data_w8_2001_Oxf <- read_sav("data_w8_2001_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D0_2_8, #count of individuals aged >60
    a18plusm_count=D0_2_1, #count of males aged 18+
    a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) 

data_w8_2001_Oxf1 <- data_w8_2001_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("8", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-08-17"),
         collection_date = as.Date("2022-08-23"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w8_2001_Oxf2 <- data_w8_2001_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-08-23"))

View(data_w8_2001_Oxf2)

###################
# Wave 9
###################
data_w9_2002_Oxf <- read_sav("data_w9_2002_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D0_2_8, #count of individuals aged >60
    a18plusm_count=D0_2_1, #count of males aged 18+
    a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) 

data_w9_2002_Oxf1 <- data_w9_2002_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("9", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-09-12"),
         collection_date = as.Date("2022-09-26"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w9_2002_Oxf2 <- data_w9_2002_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-09-26"))

View(data_w9_2002_Oxf2)

###################
# Wave 10
###################
data_w10_2002_Oxf <- read_sav("data_w10_2002_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D0_2_8, #count of individuals aged >60
    a18plusm_count=D0_2_1, #count of males aged 18+
    a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) 


data_w10_2002_Oxf1 <- data_w10_2002_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("10", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-10-17"),
         collection_date = as.Date("2022-10-27"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w10_2002_Oxf2 <- data_w10_2002_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-10-27"))

View(data_w10_2002_Oxf2)



###################
# Wave 11
###################
data_w11_2002_Oxf <- read_sav("data_w11_2002_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D0_2_8, #count of individuals aged >60
    a18plusm_count=D0_2_1, #count of males aged 18+
    a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) 

data_w11_2002_Oxf1 <- data_w11_2002_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("11", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2022-11-25"),
         collection_date = as.Date("2022-12-05"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w11_2002_Oxf2 <- data_w11_2002_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2022-12-05"))

View(data_w11_2002_Oxf2)

###################
# Wave 12
###################
data_w12_2000_Oxf <- read_sav("data_w12_2000_Oxf.sav") %>%
  rename(
    gender=A1,
    age=A2,
    dest=B1а,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=B3, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=B3а,  #Did you leave your habitual place of residence because of the war?
    orig=B3с,  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    elderly60_count=D0_2_8, #count of individuals aged >60
    a18plusm_count=D0_2_1, #count of males aged 18+
    a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) 


data_w12_2000_Oxf1 <- data_w12_2000_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("12", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2023-01-16"),
         collection_date = as.Date("2023-01-23"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w12_2000_Oxf2 <- data_w12_2000_Oxf %>%
  dplyr::select(dest_pcode, a18plusm_count, a18plusf_count, childu5_count, child5_18_count) %>%
  mutate(across(c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("a18plusm_count", "a18plusf_count", "childu5_count", "child5_18_count"),
               names_to = "age_group", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2023-01-23"))

View(data_w12_2000_Oxf2)

###################
# Wave 13, 14, 15
###################
GPS_R13_R14_R15 <- read_sav("GPS_R13_R14_R15_Merged_Screener_60000.sav") %>%
  rename(
    gender=S05,
    age=S06,
    dest=P02,  #current oblast*
    #dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=P03, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=P04,  #Did you leave your habitual place of residence because of the war?
    orig=P06  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    #childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    #childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    #child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    #elderly60_count=D0_2_8, #count of individuals aged >60
    #a18plusm_count=D0_2_1, #count of males aged 18+
    #a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
  ) %>%
  mutate(mover = case_when(isdest_res_pre_war==2 & orig!=dest~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                childu1_count, #childu5, 
                childu5_count, #child5_18, 
                child5_18_count, #elderly60, 
                elderly60_count,
                a18plusm_count, a18plusf_count) #%>%
#filter(!is.na(orig_name1), !is.na(dest_name), !is.na(age), !is.na(gender) 
#) %>%
#group_by(orig_pcode, dest_pcode, age_group, gender#, mover
#) %>%
#summarise(Count = n(), .groups = 'drop') %>% ungroup() %>%
#mutate(wave = rep("4", length(orig_pcode)))

ls()

###################
# Wave 13
###################
data_w13_5297_Oxf <- read_sav("Main survey_data_w13_5297_Oxf.sav") %>%
  rename(
    gender=S05,
    age=S06,
    dest=P02,  #current oblast*
    #dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=P03, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=P04,  #Did you leave your habitual place of residence because of the war?
    orig=P06  #Where was your habitual residence before the war started? - Oblast
    #orig_m=Reg2,    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    #*childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    ##childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    #*child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    #*elderly60_count=D0_2_8, #count of individuals aged >60
    #*a18plusm_count=D0_2_1, #count of males aged 18+
    #*a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
    
    #D01_2, #number of people
    #D01f_1, D01f_2,D01f_3,D01f_4,D01f_5,D01f_6,D01f_7,D01f_8,D01f_9,D01f_10,D01f_11,D01f_12, #ages
    #D01m_1, D01m_2,D01m_3,D01m_4,D01m_5,D01m_6,D01m_7,D01m_8,D01m_9,D01m_10  #ages
  ) %>%
  #rowwise() %>%
  mutate(#a0_1f = sum(c_across(D01f_1:D01f_12)==0, na.rm=T),
         #a0_1m = sum(c_across(D01m_1:D01m_10)==0, na.rm=T),
         #a1_4f = sum(c_across(D01f_1:D01f_12) %in% c(1,2,3,4), na.rm=T),
         #a1_4m = sum(c_across(D01m_1:D01m_10) %in% c(1,2,3,4), na.rm=T),
         #a5_9f = sum(c_across(D01f_1:D01f_12) %in% c(5,6,7,8,9), na.rm=T),
         #a5_9m = sum(c_across(D01m_1:D01m_10) %in% c(5,6,7,8,9), na.rm=T),
         #a10_14f = sum(c_across(D01f_1:D01f_12) %in% c(10,11,12,13,14), na.rm=T),
         #a10_14m = sum(c_across(D01m_1:D01m_10) %in% c(10,11,12,13,14), na.rm=T),
         #a15_19f = sum(c_across(D01f_1:D01f_12) %in% c(15,16,17,18,19), na.rm=T),
         #a15_19m = sum(c_across(D01m_1:D01m_10) %in% c(15,16,17,18,19), na.rm=T),
         
         #a15_17f = sum(c_across(D01f_1:D01f_12) %in% c(15,16,17), na.rm=T),
         #a15_17m = sum(c_across(D01m_1:D01m_10) %in% c(15,16,17), na.rm=T),
         
         #a18_19f = sum(c_across(D01f_1:D01f_12) %in% c(18,19), na.rm=T),
         #a18_19m = sum(c_across(D01m_1:D01m_10) %in% c(18,19), na.rm=T),
         
         #a20_24f = sum(c_across(D01f_1:D01f_12) %in% c(20,21,22,23,24), na.rm=T),
         #a20_24m = sum(c_across(D01m_1:D01m_10) %in% c(20,21,22,23,24), na.rm=T),
         #a25_29f = sum(c_across(D01f_1:D01f_12) %in% c(25,26,27,28,29), na.rm=T),
         #a25_29m = sum(c_across(D01m_1:D01m_10) %in% c(25,26,27,28,29), na.rm=T),
         #a30_34f = sum(c_across(D01f_1:D01f_12) %in% c(30,31,32,33,34), na.rm=T),
         #a30_34m = sum(c_across(D01m_1:D01m_10) %in% c(30,31,32,33,34), na.rm=T),
         #a35_39f = sum(c_across(D01f_1:D01f_12) %in% c(35,36,37,38,39), na.rm=T),
         #a35_39m = sum(c_across(D01m_1:D01m_10) %in% c(35,36,37,38,39), na.rm=T),
         #a40_44f = sum(c_across(D01f_1:D01f_12) %in% c(40,41,42,43,44), na.rm=T),
         #a40_44m = sum(c_across(D01m_1:D01m_10) %in% c(40,41,42,43,44), na.rm=T),
         #a45_49f = sum(c_across(D01f_1:D01f_12) %in% c(45,46,47,48,49), na.rm=T),
         #a45_49m = sum(c_across(D01m_1:D01m_10) %in% c(45,46,47,48,49), na.rm=T),
         #a50_54f = sum(c_across(D01f_1:D01f_12) %in% c(50,51,52,53,54), na.rm=T),
         #a50_54m = sum(c_across(D01m_1:D01m_10) %in% c(50,51,52,53,54), na.rm=T),
         #a55_59f = sum(c_across(D01f_1:D01f_12) %in% c(55,56,57,58,59), na.rm=T),
         #a55_59m = sum(c_across(D01m_1:D01m_10) %in% c(55,56,57,58,59), na.rm=T),
         #a60_64f = sum(c_across(D01f_1:D01f_12) %in% c(60,61,62,63,64), na.rm=T),
         #a60_64m = sum(c_across(D01m_1:D01m_10) %in% c(60,61,62,63,64), na.rm=T),
         #a65_69f = sum(c_across(D01f_1:D01f_12) %in% c(65,66,67,68,69), na.rm=T),
         #a65_69m = sum(c_across(D01m_1:D01m_10) %in% c(65,66,67,68,69), na.rm=T),
         #a70_74f = sum(c_across(D01f_1:D01f_12) %in% c(70,71,72,73,74), na.rm=T),
         #a70_74m = sum(c_across(D01m_1:D01m_10) %in% c(70,71,72,73,74), na.rm=T),
         #a75_79f = sum(c_across(D01f_1:D01f_12) %in% c(75,76,77,78,79), na.rm=T),
         #a75_79m = sum(c_across(D01m_1:D01m_10) %in% c(75,76,77,78,79), na.rm=T),
         #a80plusf = sum(c_across(D01f_1:D01f_12)>=80, na.rm=T),
         #a80plusm = sum(c_across(D01m_1:D01m_10)>=80, na.rm=T),
         
         mover = case_when(isdest_res_pre_war==2 & as.integer(orig)!=as.integer(dest)~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                D01_1,  #females total
                D01_2#,  #males total
                #a1_4f, a1_4m, a5_9f, a5_9m, a10_14f, a10_14m,
                #a15_19f, a15_19m, a20_24f, a20_24m, a25_29f, a25_29m,
                #a30_34f, a30_34m, a35_39f, a35_39m, a40_44f, a40_44m,
                #a45_49f, a45_49m, a50_54f, a50_54m, a55_59f, a55_59m,
                #a60_64f, a60_64m, a65_69f, a65_69m, a70_74f, a70_74m,
                #a75_79f, a75_79m, a80plusf, a80plusm
                ) 


data_w13_5297_Oxf1 <- data_w13_5297_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("13", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2023-05-11"),
         collection_date = as.Date("2023-06-14"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w13_5297_Oxf2 <- data_w13_5297_Oxf %>%
  dplyr::select(dest_pcode, D01_1, D01_2) %>%
  mutate(across(c("D01_1", "D01_2"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("D01_1", "D01_2"),
               names_to = "sex", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2023-06-14"))

View(data_w13_5297_Oxf2)

###################
# Wave 14
###################
data_w14_5148_Oxf <- read_sav("Main survey_data_w14_5148_Oxf.sav") %>%
  rename(
    gender=S05,
    age=S06,
    dest=P02,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=P03, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=P04,  #Did you leave your habitual place of residence because of the war?
    orig=P06,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Region2    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    #*childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    ##childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    #*child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    #*elderly60_count=D0_2_8, #count of individuals aged >60
    #*a18plusm_count=D0_2_1, #count of males aged 18+
    #*a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
    
    #D01_2, #number of people
    #D01f_1, D01f_2,D01f_3,D01f_4,D01f_5,D01f_6,D01f_7,D01f_8,D01f_9,D01f_10,D01f_11, #ages
    #D01m_1, D01m_2,D01m_3,D01m_4,D01m_5,D01m_6,D01m_7,D01m_8,D01m_9,D01m_10, D01m_10,  #ages
  ) %>%
  #rowwise() %>%
  mutate(#a0_1f = sum(c_across(D01f_1:D01f_11)==0, na.rm=T),
         #a0_1m = sum(c_across(D01m_1:D01m_11)==0, na.rm=T),
         #a1_4f = sum(c_across(D01f_1:D01f_11) %in% c(1,2,3,4), na.rm=T),
         #a1_4m = sum(c_across(D01m_1:D01m_11) %in% c(1,2,3,4), na.rm=T),
         #a5_9f = sum(c_across(D01f_1:D01f_11) %in% c(5,6,7,8,9), na.rm=T),
         #a5_9m = sum(c_across(D01m_1:D01m_11) %in% c(5,6,7,8,9), na.rm=T),
         #a10_14f = sum(c_across(D01f_1:D01f_11) %in% c(10,11,12,13,14), na.rm=T),
         #a10_14m = sum(c_across(D01m_1:D01m_11) %in% c(10,11,12,13,14), na.rm=T),
         #a15_19f = sum(c_across(D01f_1:D01f_11) %in% c(15,16,17,18,19), na.rm=T),
         #a15_19m = sum(c_across(D01m_1:D01m_11) %in% c(15,16,17,18,19), na.rm=T),
         
         #a15_17f = sum(c_across(D01f_1:D01f_11) %in% c(15,16,17), na.rm=T),
         #a15_17m = sum(c_across(D01m_1:D01m_11) %in% c(15,16,17), na.rm=T),
         #a18_19f = sum(c_across(D01f_1:D01f_11) %in% c(18,19), na.rm=T),
         #a18_19m = sum(c_across(D01m_1:D01m_11) %in% c(18,19), na.rm=T),
         
         #a20_24f = sum(c_across(D01f_1:D01f_11) %in% c(20,21,22,23,24), na.rm=T),
         #a20_24m = sum(c_across(D01m_1:D01m_11) %in% c(20,21,22,23,24), na.rm=T),
         #a25_29f = sum(c_across(D01f_1:D01f_11) %in% c(25,26,27,28,29), na.rm=T),
         #a25_29m = sum(c_across(D01m_1:D01m_11) %in% c(25,26,27,28,29), na.rm=T),
         #a30_34f = sum(c_across(D01f_1:D01f_11) %in% c(30,31,32,33,34), na.rm=T),
         #a30_34m = sum(c_across(D01m_1:D01m_11) %in% c(30,31,32,33,34), na.rm=T),
         #a35_39f = sum(c_across(D01f_1:D01f_11) %in% c(35,36,37,38,39), na.rm=T),
         #a35_39m = sum(c_across(D01m_1:D01m_11) %in% c(35,36,37,38,39), na.rm=T),
         #a40_44f = sum(c_across(D01f_1:D01f_11) %in% c(40,41,42,43,44), na.rm=T),
         #a40_44m = sum(c_across(D01m_1:D01m_11) %in% c(40,41,42,43,44), na.rm=T),
         #a45_49f = sum(c_across(D01f_1:D01f_11) %in% c(45,46,47,48,49), na.rm=T),
         #a45_49m = sum(c_across(D01m_1:D01m_11) %in% c(45,46,47,48,49), na.rm=T),
         #a50_54f = sum(c_across(D01f_1:D01f_11) %in% c(50,51,52,53,54), na.rm=T),
         #a50_54m = sum(c_across(D01m_1:D01m_11) %in% c(50,51,52,53,54), na.rm=T),
         #a55_59f = sum(c_across(D01f_1:D01f_11) %in% c(55,56,57,58,59), na.rm=T),
         #a55_59m = sum(c_across(D01m_1:D01m_11) %in% c(55,56,57,58,59), na.rm=T),
         #a60_64f = sum(c_across(D01f_1:D01f_11) %in% c(60,61,62,63,64), na.rm=T),
         #a60_64m = sum(c_across(D01m_1:D01m_11) %in% c(60,61,62,63,64), na.rm=T),
         #a65_69f = sum(c_across(D01f_1:D01f_11) %in% c(65,66,67,68,69), na.rm=T),
         #a65_69m = sum(c_across(D01m_1:D01m_11) %in% c(65,66,67,68,69), na.rm=T),
         #a70_74f = sum(c_across(D01f_1:D01f_11) %in% c(70,71,72,73,74), na.rm=T),
         #a70_74m = sum(c_across(D01m_1:D01m_11) %in% c(70,71,72,73,74), na.rm=T),
         #a75_79f = sum(c_across(D01f_1:D01f_11) %in% c(75,76,77,78,79), na.rm=T),
         #a75_79m = sum(c_across(D01m_1:D01m_11) %in% c(75,76,77,78,79), na.rm=T),
         #a80plusf = sum(c_across(D01f_1:D01f_11)>=80, na.rm=T),
         #a80plusm = sum(c_across(D01m_1:D01m_11)>=80, na.rm=T),
         
         mover = case_when(isdest_res_pre_war==2 & as.integer(orig)!=as.integer(dest)~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                D01_1,  #females total
                D01_2#,  #males total
                #a1_4f, a1_4m, a5_9f, a5_9m, a10_14f, a10_14m,
                #a15_19f, a15_19m, a20_24f, a20_24m, a25_29f, a25_29m,
                #a30_34f, a30_34m, a35_39f, a35_39m, a40_44f, a40_44m,
                #a45_49f, a45_49m, a50_54f, a50_54m, a55_59f, a55_59m,
                #a60_64f, a60_64m, a65_69f, a65_69m, a70_74f, a70_74m,
                #a75_79f, a75_79m, a80plusf, a80plusm
                ) 

data_w14_5148_Oxf1 <- data_w14_5148_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("14", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2023-09-03"),
         collection_date = as.Date("2023-09-25"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)


data_w14_5148_Oxf2 <- data_w14_5148_Oxf %>%
  dplyr::select(dest_pcode, D01_1, D01_2) %>%
  mutate(across(c("D01_1", "D01_2"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("D01_1", "D01_2"),
               names_to = "sex", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2023-09-25"))

View(data_w14_5148_Oxf2)


###################
# Wave 15
###################
data_w15_5060_Oxf <- read_sav("Main survey_data_w15_5060_Oxf.sav") %>%
  rename(
    gender=S05,
    age=S06,
    dest=P02,  #current oblast*
    dest_m=Reg, #current macroregion*
    #dest_area=B2, #area: city, road, etc.
    isdest_res_pre_war=P03, #(1)Is this your primary place of residence? (habitual residence prior to this war)?
    change_res=P04,  #Did you leave your habitual place of residence because of the war?
    orig=P06,  #Where was your habitual residence before the war started? - Oblast
    orig_m=Reg2    #macroregion before war
    #fut_mov_coun = B7а,   #What country are you planning to move to?
    #fut_mov_oblast=B7b,   #What oblast/rajon are you planning to move to? -Oblast
    #childu1=D1_02, #children 0-1 year old (yes, no)
    #*childu1_count=D0_2_3, #count of children 0-1 year old
    #childu5=D1_03, #children 1-5 years old
    ##childu5_count=D0_2_4, #count of children 1-5 years old
    #child5_18=D1_04,  #individuals aged 5-18
    #*child5_18_count=D0_2_5, #count of people aged 5-18
    #child5_18m_count=D1a_1, #count of males aged 5-18
    #child5_18f_count=D1a_2, #count of females aged 5-18
    #child5_18_counto=D0_3, #count of people under 18 years old
    #elderly60=D1_07,  #individuals aged >60 
    #*elderly60_count=D0_2_8, #count of individuals aged >60
    #*a18plusm_count=D0_2_1, #count of males aged 18+
    #*a18plusf_count=D0_2_2 #count of females aged 18+    (max 20 in all questions about counts, and 21-98 write in)
    
    #D01_2, #number of people
    #D01f_1, D01f_2,D01f_3,D01f_4,D01f_5,D01f_6,D01f_7,D01f_8,D01f_9,D01f_10,D01f_10, #ages
    #D01m_1, D01m_2,D01m_3,D01m_4,D01m_5,D01m_6,D01m_7,D01m_8,D01m_9,D01m_10, D01m_10,  #ages
  ) %>%
  #rowwise() %>%
  mutate(#a0_1f = sum(c_across(D01f_1:D01f_10)==0, na.rm=T),
         #a0_1m = sum(c_across(D01m_1:D01m_8)==0, na.rm=T),
         #a1_4f = sum(c_across(D01f_1:D01f_10) %in% c(1,2,3,4), na.rm=T),
         #a1_4m = sum(c_across(D01m_1:D01m_8) %in% c(1,2,3,4), na.rm=T),
         #a5_9f = sum(c_across(D01f_1:D01f_10) %in% c(5,6,7,8,9), na.rm=T),
         #a5_9m = sum(c_across(D01m_1:D01m_8) %in% c(5,6,7,8,9), na.rm=T),
         #a10_14f = sum(c_across(D01f_1:D01f_10) %in% c(10,11,12,13,14), na.rm=T),
         #a10_14m = sum(c_across(D01m_1:D01m_8) %in% c(10,11,12,13,14), na.rm=T),
         #a15_19f = sum(c_across(D01f_1:D01f_10) %in% c(15,16,17,18,19), na.rm=T),
         #a15_19m = sum(c_across(D01m_1:D01m_8) %in% c(15,16,17,18,19), na.rm=T),
         
         #a15_17f = sum(c_across(D01f_1:D01f_10) %in% c(15,16,17), na.rm=T),
         #a15_17m = sum(c_across(D01m_1:D01m_8) %in% c(15,16,17), na.rm=T),
         #a18_19f = sum(c_across(D01f_1:D01f_10) %in% c(18,19), na.rm=T),
         #a18_19m = sum(c_across(D01m_1:D01m_8) %in% c(18,19), na.rm=T),
         
         #a20_24f = sum(c_across(D01f_1:D01f_10) %in% c(20,21,22,23,24), na.rm=T),
         #a20_24m = sum(c_across(D01m_1:D01m_8) %in% c(20,21,22,23,24), na.rm=T),
         #a25_29f = sum(c_across(D01f_1:D01f_10) %in% c(25,26,27,28,29), na.rm=T),
         #a25_29m = sum(c_across(D01m_1:D01m_8) %in% c(25,26,27,28,29), na.rm=T),
         #a30_34f = sum(c_across(D01f_1:D01f_10) %in% c(30,31,32,33,34), na.rm=T),
         #a30_34m = sum(c_across(D01m_1:D01m_8) %in% c(30,31,32,33,34), na.rm=T),
         #a35_39f = sum(c_across(D01f_1:D01f_10) %in% c(35,36,37,38,39), na.rm=T),
         #a35_39m = sum(c_across(D01m_1:D01m_8) %in% c(35,36,37,38,39), na.rm=T),
         #a40_44f = sum(c_across(D01f_1:D01f_10) %in% c(40,41,42,43,44), na.rm=T),
         #a40_44m = sum(c_across(D01m_1:D01m_8) %in% c(40,41,42,43,44), na.rm=T),
         #a45_49f = sum(c_across(D01f_1:D01f_10) %in% c(45,46,47,48,49), na.rm=T),
         #a45_49m = sum(c_across(D01m_1:D01m_8) %in% c(45,46,47,48,49), na.rm=T),
         #a50_54f = sum(c_across(D01f_1:D01f_10) %in% c(50,51,52,53,54), na.rm=T),
         #a50_54m = sum(c_across(D01m_1:D01m_8) %in% c(50,51,52,53,54), na.rm=T),
         #a55_59f = sum(c_across(D01f_1:D01f_10) %in% c(55,56,57,58,59), na.rm=T),
         #a55_59m = sum(c_across(D01m_1:D01m_8) %in% c(55,56,57,58,59), na.rm=T),
         #a60_64f = sum(c_across(D01f_1:D01f_10) %in% c(60,61,62,63,64), na.rm=T),
         #a60_64m = sum(c_across(D01m_1:D01m_8) %in% c(60,61,62,63,64), na.rm=T),
         #a65_69f = sum(c_across(D01f_1:D01f_10) %in% c(65,66,67,68,69), na.rm=T),
         #a65_69m = sum(c_across(D01m_1:D01m_8) %in% c(65,66,67,68,69), na.rm=T),
         #a70_74f = sum(c_across(D01f_1:D01f_10) %in% c(70,71,72,73,74), na.rm=T),
         #a70_74m = sum(c_across(D01m_1:D01m_8) %in% c(70,71,72,73,74), na.rm=T),
         #a75_79f = sum(c_across(D01f_1:D01f_10) %in% c(75,76,77,78,79), na.rm=T),
         #a75_79m = sum(c_across(D01m_1:D01m_8) %in% c(75,76,77,78,79), na.rm=T),
         #a80plusf = sum(c_across(D01f_1:D01f_10)>=80, na.rm=T),
         #a80plusm = sum(c_across(D01m_1:D01m_8)>=80, na.rm=T),
         
         mover = case_when(isdest_res_pre_war==2 & as.integer(orig)!=as.integer(dest)~1,  #yes 
                           isdest_res_pre_war==1~0,  #no
                           TRUE ~ NA),
         
         age_group = case_when(
           age < 20 ~ "18-19",
           age>=20 & age <= 24 ~ "20-24",
           age>=25 & age <= 29 ~ "25-29",
           age>=30 & age <= 34 ~ "30-34",
           age>=35 & age <= 39 ~ "35-39",
           age>=40 & age <= 44 ~ "40-44",
           age>=45 & age <= 49 ~ "45-49",
           age>=50 & age <= 54 ~ "50-54",
           age>=55 & age <= 59 ~ "55-59",
           age>=60 & age <= 64 ~ "60-64",
           age>=65 & age <= 69 ~ "65-69",
           age>=70 & age <= 74 ~ "70-74",
           age>=75 & age <= 79 ~ "75-79",
           age>=80 ~ "80-999"),
         
         gender = case_when(
           gender==1 ~ "Male", 
           gender==2 ~ "Female", 
           TRUE ~ NA),
         
         dest = as.character(dest),
         dest_name = case_when(
           dest=="1" ~ "Vinnitsya region	-	UA05",
           dest=="2" ~ "Volyn region	-	UA07",
           dest=="3" ~ "Dnipropetrovsk region	-	UA12",
           dest=="4" ~ "Donetsk region	-	UA14",
           dest=="5" ~ "Zhytomyr region	-	UA18",
           dest=="6" ~ "Zakarpattya region	-	UA21",
           dest=="7" ~ "Zaporizhzhya region	-	UA23",
           dest=="8" ~ "Ivano Frankivsk region	-	UA26",
           dest=="9" ~ "Kyiv region	-	UA32",
           dest=="10" ~ "Kirovograd region	-	UA35",
           dest=="11" ~ "Luhansk region	-	UA44",
           dest=="12" ~ "Lviv region	-	UA46",
           dest=="13" ~ "Mykolayiv region	-	UA48",
           dest=="14" ~ "Odessa region	-	UA51",
           dest=="15" ~ "Poltava region	-	UA53",
           dest=="16" ~ "Rivne region	-	UA56",
           dest=="17" ~ "Sumy region	-	UA59",
           dest=="18" ~ "Ternopil region	-	UA61",
           dest=="19" ~ "Kharkiv region	-	UA63",
           dest=="20" ~ "Kherson region	-	UA65",
           dest=="21" ~ "Khmelnitskiy region	-	UA68",
           dest=="22" ~ "Cherkasy region	-	UA71",
           dest=="23" ~ "Chernivtsi region	-	UA73",
           dest=="24" ~ "Chernihiv region	-	UA74",
           dest=="25" ~ "Kiev City	-	UA80"),
         
         orig = as.character(orig),
         orig_name = case_when(
           orig=="1" ~ "Vinnitsya region	-	UA05",
           orig=="2" ~ "Volyn region	-	UA07",
           orig=="3" ~ "Dnipropetrovsk region	-	UA12",
           orig=="4" ~ "Donetsk region	-	UA14",
           orig=="5" ~ "Zhytomyr region	-	UA18",
           orig=="6" ~ "Zakarpattya region	-	UA21",
           orig=="7" ~ "Zaporizhzhya region	-	UA23",
           orig=="8" ~ "Ivano Frankivsk region	-	UA26",
           orig=="9" ~ "Kyiv region	-	UA32",
           orig=="10" ~ "Kirovograd region	-	UA35",
           orig=="11" ~ "Luhansk region	-	UA44",
           orig=="12" ~ "Lviv region	-	UA46",
           orig=="13" ~ "Mykolayiv region	-	UA48",
           orig=="14" ~ "Odessa region	-	UA51",
           orig=="15" ~ "Poltava region	-	UA53",
           orig=="16" ~ "Rivne region	-	UA56",
           orig=="17" ~ "Sumy region	-	UA59",
           orig=="18" ~ "Ternopil region	-	UA61",
           orig=="19" ~ "Kharkiv region	-	UA63",
           orig=="20" ~ "Kherson region	-	UA65",
           orig=="21" ~ "Khmelnitskiy region	-	UA68",
           orig=="22" ~ "Cherkasy region	-	UA71",
           orig=="23" ~ "Chernivtsi region	-	UA73",
           orig=="24" ~ "Chernihiv region	-	UA74",
           orig=="25" ~ "Kiev City	-	UA80"),
         
         orig_name1 = case_when(
           mover==0 ~ dest_name, 
           mover==1 ~ orig_name)) %>%  
  separate(dest_name, c("dest_name", "dest_pcode"), sep="-", remove = TRUE) %>%
  separate(orig_name1, c("orig_name1", "orig_pcode"), sep="-", remove = TRUE) %>%
  dplyr::select(orig, orig_pcode, dest, dest_pcode, #gender, 
                age, isdest_res_pre_war, 
                #childu1, 
                D01_1,  #females total
                D01_2#,  #males total
                #a1_4f, a1_4m, a5_9f, a5_9m, a10_14f, a10_14m,
                #a15_19f, a15_19m, a20_24f, a20_24m, a25_29f, a25_29m,
                #a30_34f, a30_34m, a35_39f, a35_39m, a40_44f, a40_44m,
                #a45_49f, a45_49m, a50_54f, a50_54m, a55_59f, a55_59m,
                #a60_64f, a60_64m, a65_69f, a65_69m, a70_74f, a70_74m,
                #a75_79f, a75_79m, a80plusf, a80plusm
                ) 


data_w15_5060_Oxf1 <- data_w15_5060_Oxf %>%
  group_by(orig_pcode, dest_pcode, age_group, gender) %>%
  summarise(count = n(), .groups = 'drop') %>% ungroup() %>%
  mutate(wave = rep("15", length(orig_pcode)) %>% as.integer(),
         collection_date0 = as.Date("2023-11-27"),
         collection_date = as.Date("2023-12-27"),
         prop_tot = count/sum(count)) %>%
  group_by(dest_pcode) %>%
  mutate(dest_tot = sum(count)) %>% ungroup() %>%
  group_by(orig_pcode) %>%
  mutate(orig_tot = sum(count)) %>% ungroup() %>%
  mutate(prop_orig = count/orig_tot,
         prop_dest = count/dest_tot)

data_w15_5060_Oxf2 <- data_w15_5060_Oxf %>%
  dplyr::select(dest_pcode, D01_1, D01_2) %>%
  mutate(across(c("D01_1", "D01_2"), ~ na_if(., 99))) %>%
  pivot_longer(cols = c("D01_1", "D01_2"),
               names_to = "sex", 
               values_to = "count") %>%
  group_by(dest_pcode) %>%
  summarise(count = sum(count, na.rm=T)) %>% ungroup() %>%
  mutate(day = as.Date("2023-12-27"))

View(data_w15_5060_Oxf2)



IOM_survey_Obl <- rbind(data_w1_2000_Oxf2, data_w2_2000_Oxf2, data_w3_2000_Oxf2,
                        data_w4_2006_Oxf2, data_w5_2001_Oxf2, data_w6_2000_Oxf2, 
                        data_w7_2002_Oxf2, data_w8_2001_Oxf2, data_w9_2002_Oxf2,
                        data_w10_2002_Oxf2, data_w11_2002_Oxf2, data_w12_2000_Oxf2,
                        data_w13_5297_Oxf2, data_w14_5148_Oxf2, data_w15_5060_Oxf2) %>%
  mutate(dest_pcode = str_sub(dest_pcode, start = 2),
         dest_pcode = replace_na(dest_pcode, "UA01")) %>%
  rename(pcode = dest_pcode) %>%
  arrange(pcode, day) %>% 
  group_by(pcode) %>%
  complete(day = seq.Date(as.Date("2022-02-27"), min(day), by = "day")) %>%
  fill(count, .direction = "updown") %>% ungroup() %>%
  group_by(pcode) %>%
  mutate(count = count+1,
         growth_rate = if_else(row_number() == 1, NA_real_, ((count-lag(count))/lag(count)))) %>%  ungroup() %>%
  group_by(pcode) %>%
  complete(day = seq.Date(min(as.Date("2022-03-06")), max(day), by = "day")) %>%
  fill(c("count", "growth_rate"), .direction = "updown") %>% ungroup() %>%
  filter(wday(day, label = FALSE) == 1) %>%
  group_by(pcode) %>%
  mutate(growth_rate = if_else(row_number() <= 2, nth(growth_rate, 3), growth_rate)) %>%  
  ungroup() %>%
  rename(collection_date = day) %>%
  mutate(collection_date = as.character(collection_date),
         geo_name = case_when(pcode=="UA01" ~ "Autonomous Republic of Crimea",
                              pcode=="UA71" ~ "Cherkasy Oblast",
                              pcode=="UA74" ~ "Chernihiv Oblast" ,
                              pcode=="UA73" ~ "Chernivtsi Oblast",
                              pcode=="UA12" ~ "Dnipropetrovsk Oblast",
                              pcode=="UA14" ~ "Donetsk Oblast",
                              pcode=="UA26" ~ "Ivano-Frankivsk Oblast",
                              pcode=="UA63" ~ "Kharkiv Oblast",
                              pcode=="UA65" ~ "Kherson Oblast",
                              pcode=="UA68" ~ "Khmelnytskyi",
                              pcode=="UA35" ~ "Kirovohrad Oblast",
                              pcode=="UA80" ~ "Kyiv",
                              pcode=="UA32" ~ "Kiev Oblast",
                              pcode=="UA44" ~ "Luhansk Oblast",
                              pcode=="UA46" ~ "Lviv Oblast",
                              pcode=="UA48" ~ "Mykolaiv Oblast",
                              pcode=="UA51" ~ "Odessa Oblast",
                              pcode=="UA53" ~ "Poltava Oblast",
                              pcode=="UA56" ~ "Rivne Oblast",
                              pcode=="UA85" ~ "Sevastopol",
                              pcode=="UA59" ~ "Sumy Oblast",
                              pcode=="UA61" ~ "Ternopil Oblast",
                              pcode=="UA05" ~ "Vinnytsia Oblast",
                              pcode=="UA07" ~ "Volyn Oblast",
                              pcode=="UA21" ~ "Zakarpattia Oblast",
                              pcode=="UA23" ~ "Zaporizhia Oblast",
                              pcode=="UA18" ~ "Zhytomyr Oblast"), 
         
         macroregion = case_when(
           geo_name=="Autonomous Republic of Crimea"~"Autonomous",
           geo_name=="Cherkasy Oblast"~"Center",
           geo_name=="Chernihiv Oblast"~"North",
           geo_name=="Chernivtsi Oblast"~"West",
           geo_name=="Dnipropetrovsk Oblast"~"East",
           geo_name=="Donetsk Oblast"~"East",
           geo_name=="Ivano-Frankivsk Oblast"~"West",
           geo_name=="Kharkiv Oblast"~"East",
           geo_name=="Kherson Oblast"~"South",
           geo_name=="Khmelnytskyi"~"West",
           geo_name=="Kiev Oblast"~"North",
           geo_name=="Kirovohrad Oblast"~"Center",
           geo_name=="Kyiv"~"City",
           geo_name=="Luhansk Oblast"~"East",
           geo_name=="Lviv Oblast"~"West",
           geo_name=="Mykolaiv Oblast"~"South",
           geo_name=="Odessa Oblast"~"South",
           geo_name=="Poltava Oblast"~"Center",
           geo_name=="Rivne Oblast"~"West",
           geo_name=="Sevastopol"~"Autonomous",
           geo_name=="Sumy Oblast"~"North",
           geo_name=="Ternopil Oblast"~"West",
           geo_name=="Vinnytsia Oblast"~"Center",
           geo_name=="Volyn Oblast"~"West",
           geo_name=="Zakarpattia Oblast"~"West",
           geo_name=="Zaporizhia Oblast"~"East",
           geo_name=="Zhytomyr Oblast"~"North")) %>%
  filter(growth_rate<=1)
  
View(IOM_survey_Obl)

ggplot(IOM_survey_Obl, aes(x = collection_date, y = growth_rate, group = pcode, colour = pcode)) +
  geom_line() +
  facet_wrap(~macroregion*pcode) +
  labs(x = "Year",
       y = "Growth rate",
       color = "Oblast") +
  theme_minimal()


ls(data_w1_2000_Oxf2)
ls(data_w2_2000_Oxf2)
ls(data_w3_2000_Oxf2)
ls(data_w4_2006_Oxf2)
ls(data_w5_2001_Oxf2)
ls(data_w6_2000_Oxf2)
ls(data_w7_2002_Oxf2) 
ls(data_w8_2001_Oxf2)
ls(data_w9_2002_Oxf2)
ls(data_w10_2002_Oxf2)
ls(data_w11_2002_Oxf2)
ls(data_w12_2000_Oxf2)
ls(data_w13_5297_Oxf2)
ls(data_w14_5148_Oxf2)
ls(data_w15_5060_Oxf2)



mob_df <- expand.grid(
  orig_pcode = c("UA01", "UA05", "UA07", "UA12", "UA14",
            "UA18", "UA21", "UA23", "UA26", "UA32",
            "UA35", "UA44", "UA46", "UA48", "UA51",
            "UA53", "UA56", "UA59", "UA61", "UA63",
            "UA65", "UA68", "UA71", "UA73", "UA74",
            "UA80", "UA85"),  #27 groups
  
  dest_pcode = c("UA01", "UA05", "UA07", "UA12", "UA14",
                 "UA18", "UA21", "UA23", "UA26", "UA32",
                 "UA35", "UA44", "UA46", "UA48", "UA51",
                 "UA53", "UA56", "UA59", "UA61", "UA63",
                 "UA65", "UA68", "UA71", "UA73", "UA74",
                 "UA80", "UA85"),  #27 groups
  
  gender = c("Male", "Female"),   #2 groups
  
  age_group0 = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                 "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-999")) %>%
  mutate(country = rep("Ukraine", length(Date)),
       geo_name = case_when(pcode=="UA01" ~ "Autonomous Republic of Crimea",
                            pcode=="UA71" ~ "Cherkasy Oblast",
                            pcode=="UA74" ~ "Chernihiv Oblast" ,
                            pcode=="UA73" ~ "Chernivtsi Oblast",
                            pcode=="UA12" ~ "Dnipropetrovsk Oblast",
                            pcode=="UA14" ~ "Donetsk Oblast",
                            pcode=="UA26" ~ "Ivano-Frankivsk Oblast",
                            pcode=="UA63" ~ "Kharkiv Oblast",
                            pcode=="UA65" ~ "Kherson Oblast",
                            pcode=="UA68" ~ "Khmelnytskyi",
                            pcode=="UA35" ~ "Kirovohrad Oblast",
                            pcode=="UA80" ~ "Kyiv",
                            pcode=="UA32" ~ "Kiev Oblast",
                            pcode=="UA44" ~ "Luhansk Oblast",
                            pcode=="UA46" ~ "Lviv Oblast",
                            pcode=="UA48" ~ "Mykolaiv Oblast",
                            pcode=="UA51" ~ "Odessa Oblast",
                            pcode=="UA53" ~ "Poltava Oblast",
                            pcode=="UA56" ~ "Rivne Oblast",
                            pcode=="UA85" ~ "Sevastopol",
                            pcode=="UA59" ~ "Sumy Oblast",
                            pcode=="UA61" ~ "Ternopil Oblast",
                            pcode=="UA05" ~ "Vinnytsia Oblast",
                            pcode=="UA07" ~ "Volyn Oblast",
                            pcode=="UA21" ~ "Zakarpattia Oblast",
                            pcode=="UA23" ~ "Zaporizhia Oblast",
                            pcode=="UA18" ~ "Zhytomyr Oblast"))
