import pandas as pd
import geopandas as gpd
import net_friction.data_preparation as prep
import os
from dotenv import load_dotenv
from pathlib import Path

# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)
in_dir = Path(os.getenv('in_dir'))
out_dir = Path(os.getenv('out_dir'))

# credentials
key = os.getenv('ACLED_KEY')
email = os.getenv('ACLED_EMAIL')
country = "Ukraine"
start_date = "2022-02-26"
end_date = "2024-11-01"
crs = 6383
accept_acled_terms = True

incident_gdf = prep.get_acled_data_from_api(
    key,
    email,
    country,
    start_date,
    end_date,
    crs,
    accept_acled_terms
)

acled_path = Path(out_dir) / 'covariates' / 'raw' / 'acled.csv'
incident_gdf.to_csv(acled_path, index=False)
incident_gdf.to_file(Path(out_dir) / 'covariates' / 'raw' /'acled.gpkg', driver='GPKG')

# Incident data
def vary_buffer_in_subset(buffer_distance_in_meters):
    # Define the output filenames
    incidents_outfile_csv = out_dir / 'covariates' / 'interim' / f'acled_edge_oblast_{buffer_distance_in_meters}.csv'
    incidents_outfile_gpkg = out_dir / 'covariates' / 'interim' / f'acled_edge_oblast_{buffer_distance_in_meters}.gpkg'

    # Read and process the data
    incident_subset_gdf = prep.subset_incident_data_in_buffer(
        gpd.read_file(out_dir / 'covariates' / 'network_edges_oblast.gpkg'),
        Path(out_dir) / 'covariates' / 'acled.csv',
        incidents_outfile_csv,
        buffer_distance_in_meters,
        6383,
        is_acled=True,
        index_col="event_id_cnty"  # Unique ID field in incidents table
    )

    # Save the results
    incident_subset_gdf.to_file(incidents_outfile_gpkg, driver='GPKG')

for buffer in [1000, 5000, 10000]:
    vary_buffer_in_subset(buffer)
