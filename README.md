# WHO-Oxford Ukraine NowPop Project
The Ukraine NowPop Project is a collaboration between the World Health Organization country office in Ukraine and the Oxford NowPop team within the Leverhulme Centre for Demographic Science at the University of Oxford. The aim of the project is to develop population nowcasting methodologies to produce up-to-date estimates of population sizes and age-sex demographics at oblast level and finer within Ukraine by integrating multiple streams of digital traces that include mobile network data and social media advertising data, among others.  

The project was organised into two work streams: (1) rapid-response methods using Vodafone network data to estimate population sizes at the hromada level and population flows among hromadas each month, and (2) long-term development of a statistical method to integrate multiple data streams to avoid dependence on any single source and to be sustainable when individual sources are lost. All data used in the project were anonymised and aggregated data; no individual-level or personal data were required to meet the project objectives. 

This repository contains the code developed as part of the project, and there are five key components to be aware of:  
1. Data wrangling that prepares digital traces and geospatial covariates for analysis;
2. Population nowcasting using Vodafone mobile network data;
3. Statistical population nowcasting that integrate Facebook and Instagram advertising data;
4. Web-based dashboard to interactively display population nowcasting results; and
5. Work-in-progress that includes experimental sand box of source code.

## Getting Started  

### Repository Organisation

```
├── data/                    # Data (public-only) used by the source code
├── py_helpers/              # Python helper functions
├── R_helpers/               # R helper functions
├── src/                     # Main source directory
│   ├── pop_data/            # Data wrangling: Digital traces and population data
│   ├── covariates/          # Data wrangling: Geospatial and other covariates
│   ├── model_vodafone/      # Model code: Rapid-response deterministic model based on Vodafone data
│   ├── model_statistical/   # Model code: Bayesian statistical model to integrate Facebook and Instagram data
│   ├── dashboard/           # Dashboard: Visualisation and reporting tools
│   ├── simulation/          # Simulations used to inform modelling decisions
```

### Clone the Repository

```
git clone https://github.com//OxfordDemSci/UkraineNowPop.git
cd UkraineNowPop
```

### Install Dependencies
The shell script `requirements.sh` will install linux system dependencies, followed by Python requirements and R requirements.

For Python only, use the `requirements.txt` file to install requirements with `pip install -r requirements.txt`.

For R only, source the `requirements.R` script with `RScript requirements.R`.

### Configure Environment

Copy the provided `./env.example` file into `./.env` in the repo directory and configure the variables.

## Data Sources

This project integrates data from multiple sources, including:

- Social Media Audience Data: Facebook and Instagram audience estimates as collected from the `social_media_audience` database. Please contact us to request access.

- ACLED: Armed Conflict Location & Event Data Project
- PWTT - Building damages: Ollie Ballinger's alogirthm to detect building change from satellite imagery.
- Air raid sirens as collected by eTryvoga crowdsourcing platform
- War fires detected from satellite imagery as designed by The Economist team
- Occupied territories as reported by deepstate.
- Border crossing as reported by UNHCR

## Contributing
Contributions are welcomed. To contribute please open an issue and/or submit a pull request to merge commits into the `dev` branch that resolve a specific issue or set of issues. 

## License
This repository is published under a [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html) (see `./LICENSE`). 

This guarantees your freedom to:  
1. Use the software for any purpose,
2. Change the software to suit your needs,
3. Share the software with your friends and neighbours, and
4. Share the changes that you make.

In exchange, you must:  
1. Cite us as the original source,
2. Include a copy of the license and copyright notice with the materials, 
3. Use the same license for your modifications, and 
4. Document changes that you make to the materials.

The materials in this repository are made available as-is with a limitation of liability and no warranty of any kind. See full license text for details `./LICENSE`.

## Contact
Please open an issue in the repository to report bugs or contact [@doug-leasure](https://github.com/doug-leasure) with questions or comments.

## Acknowledgements
This work was funded by the World Health Organization in Ukraine and the Leverhulme Centre for Demographic Science at the University of Oxford.
