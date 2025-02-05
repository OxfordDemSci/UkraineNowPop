# UkraineNowPop

Nowcasting Ukraine's population subnationally every day: Bayesian methods and interactive dashboard


## Getting Started  

### Prerequisites

### Installation
1. Clone the Repository
```
git clone https://github.com//OxfordDemSci/UkraineNowPop.git
cd UkraineNowPop
```
2. Install Dependencies
For pytho, use the `requirements.txt` file.

`pip install -r requirements.txt`

For R scripts, run the `requirements.R` script.


3. Configure Environment Variables

Copy the provided env.example file into a `.env` file and configure the variables.

## Usage
### Data Sources

This project integrates data from multiple sources, including:

- ACLED: Armed Conflict Location & Event Data Project
- PWTT - Building damages: Ollie Ballinger's alogirthm to detect building change from satellite imagery.
- Air raid sirens as collected by eTryvoga crowdsourcing platform
- War fires detected from satellite imagery as designed by The Economist team
- Occupied territories as reported by deepstate.
- Social Media Audience Data: Facebook and Instagram audience estimates as collected from the `social_media_audience` database. Please contact the administrator to have access to it.
- Border crossing as reported by UNHCR

### Code Folder structure
```
├── py_helpers/         # Python helper functions
├── R_helpers/          # R helper functions
├── src/                # Main source directory
│   ├── 1_pop_data/     # Scripts for processing population data
│   ├── 2_model/        # Model implementation scripts
│   ├── covariates/     # Covariates used to support the modeling
│   ├── dashboard/      # Visualization and reporting tools
│   ├── simulation/     # Simulation framwework for modelling
```

### Output folder structure
The output of all scripts are stored under the `out_dir` defined in your `.env`. The output folder structure corresponds the `/src/` folder:
```
├── covariates           # Covariates data
├── model                # Model output
├── population_proxy     # Population data
```

## Contributing


## License


## Contact


## Acknowledgements

