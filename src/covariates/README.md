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
│  ├──
```

## Covariates description
| Label     | Name                                      | Provider                                           | Type       | Description                                                                 | Options                                                                                                                                   | Spatial coverage             | Temporal coverage | Script      | Data source           | Collection method | Link |
|-----------|-------------------------------------------|----------------------------------------------------|------------|-----------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|------------------------------|------------------|-------------|------------------------|------------------|------|
| acled     | Conflict Location                        | Armed Conflict Location and Event Data Project    | Discrete   | Tracking a range of violent and non-violent actions by or affecting political agents. | withfatalities, disorderPolitical, disorderStrategic, disorderDemonstration, disorderPoliticalDemonstration, eventExplosion, eventBattle, eventStrategic, eventAgainstCivilians, eventProtest, eventRiot | all                          | daily            | acled.py    | API                    | Professional scraping | [Link](https://acleddata.com/) |
| occupied  | Territory occupied by Russia            | Ukrainian Open-Source intelligence DeepState project | Continuous | Live map of Russian and Ukrainian military operations                        | Amount and proportion                                                                                                                   | all                          | daily            | deepstate.py | Web scraping           | Crowdsourced      | [Link](https://en.wikipedia.org/wiki/DeepStateMap.Live) |
| sirens    | Air raid sirens                         | eTryvoga                                           | Discrete   | Air-raid sirens collected by volunteers from eTryvoga channel               |                                                                                                                                          | All except Crimea and Sevastopol | daily            | sirens.R    | Github repo            | Crowdsourced      | [Link](https://github.com/Vadimkin/ukrainian-air-raid-sirens-dataset) |
| pwtt      | Building change (Pixel-wise T-Test)     | Ollie Ballinger                                    | Continuous | Building change detected by the Synthetic Aperture Radar imagery from the Sentinel-1 satellite. |                                                                                                           | all                          | monthly          | pwtt.py     | Google Earth Engine   | Satellite imagery | [Link](https://github.com/oballinger/PWTT) |
| war_fires | War fires                               | The Economist                                      | Discrete   | War fire detected from temperature anomalies                                |                                                                                                                                          | all                          | daily            | warfires.R  | Github repo            | Satellite imagery | [Link](https://github.com/TheEconomist/the-economist-war-fire-model/) |

## Covariates processing

### Missing values
#### PWTT
PWTT is collected every month. For the timestamp in between we replicate the previous value.
#### Air siren
There are permanent sirens in Luhansk and Crimea that are not reported. We impute it by setting the value for those oblasts to the maximum value observed in the dataset.

### Standardisation
We want to standardise covariates across time and location to pick up different forms of signal.
The current methodology is, with $x_{i,t}$ the raw covariate and $X_{i,t}$ the standardised covariate:
- Z-score scaling: 
$X_{i,t} = \frac{x_{i,t} - \bar{x}}{sd(x)}$
- Z-score cumulative scaling: 
$y_{i,t}= \sum_{u=t-t_0}^{t}x_u$
$X_{i,t} = \frac{y_{i,t} - \bar{y}}{sd(y)}$
- Z-score temporal scaling:
$y_{i,t}=  x_{i,t} - mean(x_{i,t}, \dotsc , x_{i,t-t0})= x_{i,t} - \frac{1}{t_0}\sum_{u=t-t_0}^{t}x_{i,u}  $
$X_{i,t} = \frac{y_{i,t}}{sd(y)}$
- Z-score geographical scaling:
$y_{i,t}= x_{i,t} - mean(x_{i,t}, x_{j,t}, \dotsc , x_{l,t}$)
$X_{i,t} = \frac{y_{i,t}}{sd(y)}$

There are three "hyperparameters" :
1. $t_0$ in the cumulative scaling that is the time window for the cumulative effect
2. $t_0$ in the temporal scaling that is the time window for the baseline comparison
3. $j, \dotsc , l$ in the geographical scaling that is the geographical window for the baseline comparison.

Current implementation are:
1. sum over: 4 weeks, 12 weeks, 24 weeks
2. centered over: 4 weeks, 12 weeks, 24 weeks

### `ua_covariates_oblast.csv`
The final dataset stored under `covariate/final` contains the raw value and the processed value. More specifically the column `sum_stat` describes the summary statistics applied to the raw data before any standardisation.
The `time_std` and `space_std` columns explain how the `center_std` and `scale_std` have been computed to derive the `value_std` column.

Processing and standardisation combinations:

![image](https://github.com/user-attachments/assets/495a5e58-a0bd-4a3f-9082-c4f3a9abab58)

Example of the output:

![image](https://github.com/user-attachments/assets/08d47b9b-7e4e-4ed6-93a9-bb8d61bb9b6c)



