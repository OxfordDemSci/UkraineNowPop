rm(list = ls())
gc()
library(data.table)
library(stringr)
library(readr)
library(here)
library(readxl)

source(file.path(here::here(), "R_helpers/generic.R"))

output_date <- "202510"

# create output directory ----
dir.create(
  file.path(out_dir, "model", "deterministic", "deliverables", output_date),
  recursive = TRUE,
  showWarnings = FALSE
)

# load data ----
pcodes <- read_excel(
  file.path(
    in_dir,
    "COD-PS",
    "2022",
    "ukr_adminboundaries_tabulardata.xlsx"
  ),
  sheet = "ADM3"
) |>
  select(
    ADM3_EN,
    ADM2_EN,
    ADM1_EN,
    ADM3_PCODE,
    ADM2_PCODE,
    ADM1_PCODE,
    ADM3_UA,
    ADM2_UA,
    ADM1_UA
  ) |>
  bind_rows(
    tibble(
      ADM3_EN = c("Abroad", "Unknown"),
      ADM2_EN = c("Abroad", "Unknown"),
      ADM1_EN = c("Abroad", "Unknown"),
      ADM3_PCODE = c("Abroad", "Unknown"),
      ADM2_PCODE = c("Abroad", "Unknown"),
      ADM1_PCODE = c("Abroad", "Unknown"),
      ADM3_UA = c("За кордоном", "Невідомо"),
      ADM2_UA = c("За кордоном", "Невідомо"),
      ADM1_UA = c("За кордоном", "Невідомо")
    )
  )

flows <- fread(file.path(
  out_dir,
  "model",
  "deterministic",
  "mobilePhone_deterministic_agesex.csv"
))

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
  origin_raion_PCODE := fifelse(
    origin_raion_PCODE == 'Unknown',
    'Unknown',
    str_sub(origin_raion_PCODE, 1, 6)
  )
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
  destination_raion_PCODE := fifelse(
    destination_raion_PCODE == 'Unknown',
    'Unknown',
    str_sub(destination_raion_PCODE, 1, 6)
  )
]

setnames(flows, "monthlyFlow_hat_calibrated", "pop_estimated")

## remove ngct -----
flows <- flows[!grepl("ngct", destination_hromada)]


# remove unwanted column
flows[,
  c(
    "pi_hat",
    "scaling_factor",
    "monthlyFlow_hat_agesex",
    'origin_macroregion',
    'destination_macroregion',
    '2025-05-01',
    '2025-06-01',
    'dip_ratio'
  ) := NULL
]

# aggregate to create stocks ----
stocks <- flows[,
  .(pop_estimated = sum(pop_estimated, na.rm = TRUE)),
  by = .(
    t,
    a_name,
    s_name,
    hromada = destination_hromada,
    oblast = destination_oblast,
    raion = destination_raion,
    hromada_PCODE = destination_hromada_PCODE,
    raion_PCODE = destination_raion_PCODE
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
  flows_sub <- flows_sub[,
    c(
      "origin_hromada",
      "origin_raion",
      "origin_oblast",
      "destination_hromada",
      "destination_raion",
      "destination_oblast"
    ) := NULL
  ]
  flows_sub <- flows_sub[, pop_estimated := round(pop_estimated, 3)]

  stocks_sub <- stocks[t == month]
  stocks_sub <- stocks_sub[,
    c(
      "hromada",
      "raion",
      "oblast"
    ) := NULL
  ]
  stocks_sub <- stocks_sub[, pop_estimated := round(pop_estimated)]

  # reshape to wide format
  flows_sub_wide <- dcast(
    flows_sub,
    t +
      origin_hromada_PCODE +
      origin_raion_PCODE +
      destination_hromada_PCODE +
      destination_raion_PCODE ~
      s_name + a_name,
    value.var = "pop_estimated",
    fill = 0
  )

  setnames(
    flows_sub_wide,
    old = c(
      "origin_hromada_PCODE",
      "origin_raion_PCODE",
      "destination_hromada_PCODE",
      "destination_raion_PCODE"
    ),
    new = c(
      "origin_ADM3_PCODE",
      "origin_ADM2_PCODE",
      "destination_ADM3_PCODE",
      "destination_ADM2_PCODE"
    )
  )

  flows_sub_wide <- merge(
    flows_sub_wide,
    pcodes |>
      select(
        destination_ADM3_PCODE = ADM3_PCODE,
        destination_ADM2_PCODE = ADM2_PCODE,
        destination_ADM1_PCODE = ADM1_PCODE
      ),
    by.x = c("destination_ADM3_PCODE", "destination_ADM2_PCODE"),
    by.y = c("destination_ADM3_PCODE", "destination_ADM2_PCODE"),
    all.x = TRUE
  )

  flows_sub_wide <- merge(
    flows_sub_wide,
    pcodes |>
      select(
        origin_ADM3_PCODE = ADM3_PCODE,
        origin_ADM2_PCODE = ADM2_PCODE,
        origin_ADM1_PCODE = ADM1_PCODE
      ),
    by.x = c("origin_ADM3_PCODE", "origin_ADM2_PCODE"),
    by.y = c("origin_ADM3_PCODE", "origin_ADM2_PCODE"),
    all.x = TRUE
  )

  stocks_sub_wide <- dcast(
    stocks_sub,
    t +
      hromada_PCODE +
      raion_PCODE ~
      s_name + a_name,
    value.var = "pop_estimated",
    fill = 0
  )

  setnames(
    stocks_sub_wide,
    old = c(
      "hromada_PCODE",
      "raion_PCODE"
    ),
    new = c(
      "ADM3_PCODE",
      "ADM2_PCODE"
    )
  )

  stocks_sub_wide <- merge(
    stocks_sub_wide,
    pcodes,
    by.x = c("ADM3_PCODE", "ADM2_PCODE"),
    by.y = c("ADM3_PCODE", "ADM2_PCODE"),
    all.x = TRUE
  )

  # create total columns
  age_groups <- unique(flows$a_name)
  for (age in age_groups) {
    flows_sub_wide[,
      paste0("T_", age) := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("_", age, "$"))
    ]
    stocks_sub_wide[,
      paste0("T_", age) := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("_", age, "$"))
    ]
  }
  sexes <- unique(flows$s_name)
  for (sex in sexes) {
    flows_sub_wide[,
      paste0(sex, "_TL") := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("^", sex, "_"))
    ]
    stocks_sub_wide[,
      paste0(sex, "_TL") := rowSums(.SD, na.rm = TRUE),
      .SDcols = patterns(paste0("^", sex, "_"))
    ]
  }
  flows_sub_wide[,
    "T_TL" := rowSums(.SD, na.rm = TRUE),
    .SDcols = paste0(sexes, "_TL")
  ]
  stocks_sub_wide[,
    "T_TL" := rowSums(.SD, na.rm = TRUE),
    .SDcols = paste0(sexes, "_TL")
  ]

  # write output
  flows_cols <- c(
    "t",
    "origin_ADM1_PCODE",
    "origin_ADM2_PCODE",
    "origin_ADM3_PCODE",
    "destination_ADM1_PCODE",
    "destination_ADM2_PCODE",
    "destination_ADM3_PCODE",
    names(flows_sub_wide)[
      !(names(flows_sub_wide) %in%
        c(
          "t",
          "origin_ADM1_PCODE",
          "origin_ADM2_PCODE",
          "origin_ADM3_PCODE",
          "destination_ADM1_PCODE",
          "destination_ADM2_PCODE",
          "destination_ADM3_PCODE"
        ))
    ]
  )
  stocks_cols <- c(
    "t",
    "ADM1_PCODE",
    "ADM2_PCODE",
    "ADM3_PCODE",
    "ADM1_EN",
    "ADM2_EN",
    "ADM3_EN",
    "ADM1_UA",
    "ADM2_UA",
    "ADM3_UA",
    names(stocks_sub_wide)[
      !(names(stocks_sub_wide) %in%
        c(
          "t",
          "ADM1_PCODE",
          "ADM2_PCODE",
          "ADM3_PCODE",
          "ADM1_EN",
          "ADM2_EN",
          "ADM3_EN",
          "ADM1_UA",
          "ADM2_UA",
          "ADM3_UA"
        ))
    ]
  )
  fwrite(
    flows_sub_wide[, ..flows_cols],
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
    stocks_sub_wide[, ..stocks_cols],
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
