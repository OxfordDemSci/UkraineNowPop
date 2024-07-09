#install.packages("tidyverse")
library("tidyverse")


od_matrix_feb <- structure(c(268	,	71	,	115	,	195	,	65	,
                         130	,	150	,	162	,	295	,	169	,
                         189	,	17	,	49	,	66	,	293	,
                         241	,	253	,	33	,	142	,	288	,
                         213	,	18	,	41	,	243	,	59), 
                 .Dim = c(5, 5)) %>% as.table()

od_matrix_mar <- structure(c(171	,	179	,	8	,	121	,	36	,
                        125	,	235	,	183	,	158	,	210	,
                        88	,	117	,	14	,	129	,	212	,
                        221	,	113	,	169	,	65	,	109	,
                        81	,	99	,	139	,	11	,	98), 
                        .Dim = c(5, 5)) %>% as.table()

od_matrix_apr <- structure(c(183	,	165	,	161	,	4	,	30,
                             8	,	156	,	123	,	132	,	117,
                             148	,	46	,	171	,	91	,	176,
                             229	,	13	,	66	,	84	,	178,
                             140	,	118	,	23	,	38	,	159), 
                           .Dim = c(5, 5)) %>% as.table()

od_matrix_may <- structure(c(174	,	244	,	115	,	187	,	88	,
                             210	,	71	,	109	,	233	,	71	,
                             193	,	172	,	66	,	59	,	195	,
                             70	,	216	,	65	,	190	,	43	,
                             65	,	191	,	104	,	130	,	103), 
                           .Dim = c(5, 5)) %>% as.table()


dimnames(od_matrix_feb) <- list(
  orig = c("A", "B", "C", "D", "E"),
  dest = c("A", "B", "C", "D", "E"))

dimnames(od_matrix_mar) <- list(
  orig = c("A", "B", "C", "D", "E"),
  dest = c("A", "B", "C", "D", "E"))

dimnames(od_matrix_apr) <- list(
  orig = c("A", "B", "C", "D", "E"),
  dest = c("A", "B", "C", "D", "E"))

dimnames(od_matrix_may) <- list(
  orig = c("A", "B", "C", "D", "E"),
  dest = c("A", "B", "C", "D", "E"))


freq_od_feb <- as.data.frame(od_matrix_feb) %>%
  group_by(orig) %>%
  mutate(count_orig = sum(Freq)) %>% ungroup() %>%
  group_by(dest) %>%
  mutate(count_dest = sum(Freq)) %>% ungroup() %>%
  mutate(count_tot = sum(Freq),
         month = 1)

freq_od_mar <- as.data.frame(od_matrix_mar) %>%
  group_by(orig) %>%
  mutate(count_orig = sum(Freq)) %>% ungroup() %>%
  group_by(dest) %>%
  mutate(count_dest = sum(Freq)) %>% ungroup() %>%
  mutate(count_tot = sum(Freq),
         month = 2)

freq_od_apr <- as.data.frame(od_matrix_apr) %>%
  group_by(orig) %>%
  mutate(count_orig = sum(Freq)) %>% ungroup() %>%
  group_by(dest) %>%
  mutate(count_dest = sum(Freq)) %>% ungroup() %>%
  mutate(count_tot = sum(Freq),
         month = 3)

freq_od_may <- as.data.frame(od_matrix_may) %>%
  group_by(orig) %>%
  mutate(count_orig = sum(Freq)) %>% ungroup() %>%
  group_by(dest) %>%
  mutate(count_dest = sum(Freq)) %>% ungroup() %>%
  mutate(count_tot = sum(Freq),
         month = 4)

freq_od <- rbind(freq_od_feb, freq_od_mar, freq_od_apr, freq_od_may)
glimpse(freq_od)

margin_orig <- freq_od %>%
  select(orig, count_orig, month) %>%
  unique() %>%
  filter(month==1)

margin_dest <- freq_od %>%
  select(dest, count_dest, month) %>%
  unique() %>%
  filter(month==1)


sim_data_list <- list(
  I = 5,
  J = 5,
  row_margins = margin_orig$count_orig,
  col_margins = margin_dest$count_dest
)


fit_lognormal <- stan(file = "src/analysis/toy_examples/loglinear/loglinear_model.stan",
               data = sim_data_list,
               iter = 500, 
               thin = 1, 
               warmup = 100,
               verbose = FALSE, 
               chains = 3, cores = 3, 
               seed = 26)

print(fit_lognormal)
plot(fit_lognormal)




