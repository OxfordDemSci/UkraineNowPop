source(file.path(here::here(), "R_helpers/generic.R"))

flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex_domestic.csv"))
