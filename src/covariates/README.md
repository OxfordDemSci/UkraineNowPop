# Covariates

## Folder structure

The covariates code is divided in three: the scripts collecting the data, the one applying the same processing to all the gathered data and the ones creating the report on covariates.

```         
├── collection
│  ├── acled.py             # Script to access and process ACLED conflict data
│  ├── deepstate.py         # Analysis of deep-state actors in conflicts
│  ├── pwtt.py              # Processing of PWTT covariate (building damage)
│  ├── sirens.R             # Analysis of early warning siren data
│  ├── warfires.R           # Fire incidents in war zones analysis
├── processing
│  ├── 00_impute_NAs.py
│  ├── 10_combine_stockVariables.R
├── report
```

## Covariates description

| Label | Name | Provider | Type | Description | Options | Spatial coverage | Temporal coverage | Script | Data source | Collection method | Link |
|------|------|------|------|------|------|------|------|------|------|------|------|
| acled | Conflict Location | Armed Conflict Location and Event Data Project | Discrete | Tracking a range of violent and non-violent actions by or affecting political agents. | withfatalities, territoryUkraine, territoryRussia, eventExplosion (includes armed clash), all | all | daily | acled.py | API | Professional scraping | [Link](https://acleddata.com/) |
| occupied | Territory occupied by Russia | Ukrainian Open-Source intelligence DeepState project | Continuous | Live map of Russian and Ukrainian military operations | Amount and proportion | all | daily | deepstate.py | Web scraping | Crowdsourced | [Link](https://en.wikipedia.org/wiki/DeepStateMap.Live) |
| sirens | Air raid sirens | eTryvoga | Discrete | Air-raid sirens collected by volunteers from eTryvoga channel |  | All except Crimea and Sevastopol | daily | sirens.R | Github repo | Crowdsourced | [Link](https://github.com/Vadimkin/ukrainian-air-raid-sirens-dataset) |
| pwtt | Building change (Pixel-wise T-Test) | Ollie Ballinger | Continuous | Building change detected by the Synthetic Aperture Radar imagery from the Sentinel-1 satellite. |  | all | monthly | pwtt.py | Google Earth Engine | Satellite imagery | [Link](https://github.com/oballinger/PWTT) |
| war_fires | War fires | The Economist | Discrete | War fire detected from temperature anomalies |  | all | daily | warfires.R | Github repo | Satellite imagery | [Link](https://github.com/TheEconomist/the-economist-war-fire-model/) |
| graced | CO2 emissions | GRACED | Continuous | Categorised emissions | Residential, Industry, GroundTransportation | all | monthly | graced.py | Online repo | Remote sensing | [Link](https://carbonmonitor-graced.com/datasets.html) |

## Covariates processing

### Missing values

#### PWTT & GRACED

PWTT and GRACED are collected every month. For the timestamp in between we replicate the previous value.

### Air sirens

There are permanent sirens in Luhansk and Crimea that are not reported. We impute it by setting the value for those oblasts to the maximum value observed in the dataset.

### Standardisation

We want to process the covariates across time and location to pick up different forms of signal.

We implemented two processing levels:

1.  on the covariate itself: we derive summary statistics from the raw value of the covariate

2.  on its standardisation: we implement a Z-score scaling ($X_{i,t} = \frac{x_{i,t} - \bar{x}}{sd(x)}$ ) with different time or spatial window for computing the centering $\bar{x}$ and scaling $sd(x)$.

Four combinations are currently available:
| Combination       | Summary statistics | Time standardisation | Spatial standardisation | Meaning                                                                                                                       |
|-------------------|--------------------|----------------------|-------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| **Extreme outliers**  | `raw`              | `all`                | `country`               | This corresponds to the conventional Z-score across all i and t and should highlight singular outliers across time and space. |
| **Duration**          | `sum_xxweek`       | `NA`                 | `country`               | This should highlight the duration of events in comparison to other spatial units.                                            |
| **Temporal outliers** | `raw`              | `xxweek`             | `NA`                    | This should highlight intensification of events, i.e., outliers across time.                                                  |
| **Spatial outliers**  | `raw`              | `NA`                 | `country`               | This should highlight hotspots of events, i.e., outliers across space.                                                       |


There are three "hyperparameters" : (1) $t$ in the cumulative scaling that is the time window for the cumulative effect (2) $t$ in the temporal scaling that is the time window for the baseline comparison (3) $i$ in the geographical scaling that is the geographical window for the baseline comparison.

Current hyperparameters are: (1) sum over 6, 12 and 24 weeks, (2) centered and scaled over 6, 12 and 24 weeks (3) centered and scaled over all oblasts.

The processing label `std_label` follows the following schema: `sum_stat, time_std, space_std`.

### Output

The final dataset stored under `covariate/final` and called `ua_covariates_oblast.csv` contains the raw value and the processed value. More specifically the column `sum_stat` describes the summary statistics applied to the raw data before any standardisation.

In the output, the `time_std` and `space_std` columns explain how the `center_std` and `scale_std` have been computed to derive the `value_std` column.

## Report on covariates

The covariates visualisation is stored in src/covariates/report. I have organised it into three different reports, each consisting of a Quarto .qmd file and its rendered .html version.

The three reports are as follows:

1.  1_covariates_vis: *Visualise individual covariates*. This report contains scatterplots over time for each covariate along with their 31 + 2 × 3 related standardisations:

-   Raw values
-   Cumulative values (with three different time windows)
-   Time-scaled values (with three different time windows)

2.  2_covariates_corr: *Correlation between covariates*. This report provides a correlation analysis split into three sections:

-   Correlation between processing methods for each covariate
-   Correlation between covariates for each processing method
-   A full correlation table for further exploration
