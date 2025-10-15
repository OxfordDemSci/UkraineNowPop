rm(list = ls())
gc()
library(data.table)
library(stringr)
library(readr)
library(here)

source(file.path(here::here(), "R_helpers/generic.R"))

output_date <- "202510"

# create output directory ----
dir.create(
  file.path(out_dir, "model", "deterministic", "deliverables", output_date),
  recursive = TRUE,
  showWarnings = FALSE
)

# load data ----
pcodes <- fread(file.path(here::here(
  "src/dashboard/api/app/data/db-data/global_pcodes.csv"
)))
flows <- fread(file.path(
  out_dir,
  "model",
  "deterministic",
  "mobilePhone_deterministic_agesex.csv"
))

# filter & rename pcodes -----
pcodes <- pcodes[
  `Admin Level` == 1 & Location == "UKR",
  .(pcode = `P-Code`, Name)
]

# prepare flows data -----
flows[,
  origin_hromada_PCODE := fifelse(
    origin_hromada == "Kyiv",
    "UA8000000",
    origin_hromada
  )
]
flows[
  origin_hromada_PCODE != "Abroad",
  origin_hromada_PCODE := str_sub(origin_hromada_PCODE, 1, 9)
]

flows[,
  origin_raion_PCODE := fifelse(origin_raion == "Kyiv", "UA8000", origin_raion)
]
flows[
  origin_hromada_PCODE != "Abroad",
  origin_raion_PCODE := str_sub(origin_raion_PCODE, 1, 6)
]

flows[,
  destination_hromada_PCODE := fifelse(
    destination_hromada == "Kyiv",
    "UA8000000",
    destination_hromada
  )
]
flows[
  destination_hromada_PCODE != "Abroad",
  destination_hromada_PCODE := str_sub(destination_hromada_PCODE, 1, 9)
]

flows[,
  destination_raion_PCODE := fifelse(
    destination_raion == "Kyiv",
    "UA8000",
    destination_raion
  )
]
flows[
  destination_hromada_PCODE != "Abroad",
  destination_raion_PCODE := str_sub(destination_raion_PCODE, 1, 6)
]

setnames(flows, "monthlyFlow_hat_calibrated", "pop_estimated")

## remove ngct -----
flows <- flows[!grepl("ngct", destination_hromada)]

## merge oblast info -----
setnames(pcodes, "Name", "destination_oblast")
flows <- merge(
  flows,
  pcodes[, .(destination_oblast, destination_oblast_PCODE = pcode)],
  by = "destination_oblast",
  all.x = TRUE
)

setnames(pcodes, "destination_oblast", "origin_oblast")
flows <- merge(
  flows,
  pcodes[, .(origin_oblast, origin_oblast_PCODE = pcode)],
  by = "origin_oblast",
  all.x = TRUE
)

## fix special oblast codes -----
flows[
  origin_oblast %in% c("Abroad", "Unknown"),
  origin_oblast_PCODE := origin_oblast
]
flows[
  destination_oblast == "Abroad",
  destination_oblast_PCODE := "Abroad"
]

# remove unwanted column
flows[, c("pi_hat", "scaling_factor", "monthlyFlow_hat_agesex") := NULL]

# aggregate to create stocks ----
stocks <- flows[,
  .(pop_estimated = sum(pop_estimated, na.rm = TRUE)),
  by = .(
    t,
    a_name,
    s_name,
    macroregion = destination_macroregion,
    hromada = destination_hromada,
    oblast = destination_oblast,
    raion = destination_raion,
    hromada_PCODE = destination_hromada_PCODE,
    raion_PCODE = destination_raion_PCODE,
    oblast_PCODE = destination_oblast_PCODE
  )
]

# write output ----
fwrite(
  stocks,
  file.path(
    out_dir,
    "model",
    "deterministic",
    "deliverables",
    output_date,
    paste0(tolower(country), "_stocks_hromada_agesex", output_label, ".csv")
  )
)
fwrite(
  flows,
  file.path(
    out_dir,
    "model",
    "deterministic",
    "deliverables",
    output_date,
    paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
  )
)

# write output separate for each t ----

for (month in unique(flows$t) |> as.character()) {
  # month = "2025-07-01"

  output_month_path <- file.path(
    out_dir,
    "model",
    "deterministic",
    "deliverables",
    output_date,
    'estimates_by_month',
    month
  )
  dir.create(output_month_path, recursive = TRUE, showWarnings = FALSE)

  flows_sub <- flows[t == month]
  stocks_sub <- stocks[t == month]
  # reshape to wide format
  flows_sub_wide <- dcast(
    flows_sub,
    origin_hromada +
      destination_hromada +
      origin_hromada_PCODE +
      origin_raion_PCODE +
      origin_oblast_PCODE +
      destination_hromada_PCODE +
      destination_raion_PCODE +
      destination_oblast_PCODE ~
      a_name + s_name,
    value.var = "pop_estimated"
  )
  stocks_sub_wide <- dcast(
    stocks_sub,
    t +
      hromada +
      oblast +
      raion +
      hromada_PCODE +
      raion_PCODE +
      oblast_PCODE ~
      a_name + s_name,
    value.var = "pop_estimated"
  )
  # create total columns
  age_groups <- unique(flows$a_name)
  for (age in age_groups) {
    flows_sub_wide[,
      paste0(age, "_Total") := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("^", age, "_"))
    ]
    stocks_sub_wide[,
      paste0(age, "_Total") := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("^", age, "_"))
    ]
  }
  sexes <- unique(flows$s_name)
  for (sex in sexes) {
    flows_sub_wide[,
      paste0(sex, "_Total") := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("_", sex, "$"))
    ]
    stocks_sub_wide[,
      paste0(sex, "_Total") := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("_", sex, "$"))
    ]
  }
  flows_sub_wide[,
    "Total" := rowSums(.SD, na.rm = TRUE),
    .SDcols = paste0(sexes, "_Total")
  ]
  stocks_sub_wide[,
    "Total" := rowSums(.SD, na.rm = TRUE),
    .SDcols = paste0(sexes, "_Total")
  ]

  # write output

  fwrite(
    flows_sub,
    file.path(
      output_month_path,
      paste0(
        tolower(country),
        "_flows_hromada_agesex_",
        gsub("-", "", month),
        output_label,
        ".csv"
      )
    )
  )
  fwrite(
    stocks_sub,
    file.path(
      output_month_path,
      paste0(
        tolower(country),
        "_stocks_hromada_agesex_",
        gsub("-", "", month),
        output_label,
        ".csv"
      )
    )
  )
}
