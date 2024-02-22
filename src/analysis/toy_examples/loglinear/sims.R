

od_matrix <- structure(c(268	,	71	,	115	,	195	,	65	,
                         130	,	150	,	162	,	295	,	169	,
                         189	,	17	,	49	,	66	,	293	,
                         241	,	253	,	33	,	142	,	288	,
                         213	,	18	,	41	,	243	,	59), 
                 .Dim = c(5, 5)) %>% as.table()

dimnames(od_matrix) <- list(
  res_t = c("A", "B", "C", "D", "E"),
  res_t1 = c("A", "B", "C", "D", "E")
)


freq_od <- as.data.frame(od_matrix) %>%
  group_by(res_t) %>%
  mutate(orig = sum(Freq)) %>% ungroup() %>%
  group_by(res_t1) %>%
  mutate(dest = sum(Freq)) %>% ungroup() %>%
  mutate(total = sum(Freq))

#View(freq_od)


sim_data_list <- list(
  N = nrow(freq_od),
  orig = freq_od$orig,
  dest = freq_od$dest,
  tot = freq_od$total
)

#install.packages("rstan")
library("rstan")

loglinear_model <- stan_model("src/analysis/toy_examples/loglinear/loglinear_model.stan")

