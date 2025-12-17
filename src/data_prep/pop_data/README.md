# Population data preperation folder

The scripts stored in `./src/data_prep/pop_data/` obtain and prepare input data for analysis. These scripts are all related to preparation of population data or digital traces used as proxies of population. Outputs from these scripts will be written into `./wd/out/population_proxy/`.

## Script descriptions

### 10_create_master_index.R
Create at the root of the `population_proxy` folder a master index table by custom location, time, age and sex. 
![image](https://github.com/user-attachments/assets/2bdb92ec-62ae-48e7-9c5e-6482a38a4fa5)

### 20_prepare_sma_data.R
Create in `social_media_audience` folder two tables `coutry_facebook_audience.csv` and `coutry_instagram_audience.csv` by pulling from the [social media audience database](http://18.135.72.18![image](https://github.com/user-attachments/assets/063f0bd8-9df1-4aaa-9904-6215b68c46e2)
).
It requires the `sma_API_token` to be setup in your `.env` file.
![image](https://github.com/user-attachments/assets/8ae0b89f-fba5-4d3d-8697-f1cd81afb487)

### 30_border_crossing.R
Pull from the [UNHCR API ](https://api.unhcr.org/docs/refugee-statistics.html) information on in, out and net border crossings.

![image](https://github.com/user-attachments/assets/977049a8-2a2e-4f18-944e-99c9a037024b)

### 50_PDR_Facebook_population_nowcasting.R
Prepares population estimates based on Facebook audience sizes from [Leasure et al 2023 (Population and Development Review)](https://doi.org/10.1111/padr.12558)

### 60_vodafone.R
Prepares commercially-available Vodafone mobile network data for Ukraine.

### 70_DTM.R
obtains population displacement data from the IOM Displacement Tracking Matrix (DTM) AOI.

### 80_refugee.R
Obtains data on age-sex demographics of Ukrainian refugees from Eurostat and UNHCR.

### query_meta_keys.py
Queries Meta's regional identifiers from Oxford's internal social media database.